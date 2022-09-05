use std::{
    cell::RefCell,
    fs::{File, OpenOptions},
    io::{BufRead, BufReader, Seek, SeekFrom, Write},
    path::{Path, PathBuf},
    process::{Command, Stdio},
    time::{Duration, Instant},
};

/* #[cfg(windows)]
use winsafe::{
    self as w, co,
    prelude::{ShellITaskbarList3, UserHwnd},
}; */

use anyhow::{anyhow, Context};
use once_cell::unsync::Lazy;

use crate::{util::format_duration, watch::Sender, Segment};

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ProcessingState {
    Starting,
    Splitting,
    Merging,
    Encoding {
        progress: f32,
        speed: f32,
        eta: Duration,
    },
    CleaningUp,
    Done,
    Failed,
}

pub fn extract_keyframes(file: &Path) -> anyhow::Result<Vec<Duration>> {
    thread_local! {
        static KEYFRAME_CACHE: Lazy<RefCell<lru::LruCache<String, Vec<Duration>>>> =
            Lazy::new(|| RefCell::new(lru::LruCache::new(8)));
    }

    let file_str = file
        .to_str()
        .context("Could not convert file path to string")?;

    let cached_result = KEYFRAME_CACHE.with(|cache| {
        let mut cache = cache.borrow_mut();
        cache.get(file_str).cloned()
    });

    if let Some(keyframes) = cached_result {
        return Ok(keyframes);
    }

    let mut cmd = Command::new("ffprobe");

    #[cfg(windows)]
    {
        const CREATE_NO_WINDOW: u32 = 0x08000000;
        use std::os::windows::process::CommandExt;
        cmd.creation_flags(CREATE_NO_WINDOW);
    }

    cmd.args(["-v", "error"])
        .args(["-skip_frame", "nokey"])
        .args([
            "-show_entries",
            "frame=pts_time,pkt_dts_time,best_effort_timestamp_time",
        ])
        .args(["-select_streams", "v"])
        .args(["-of", r#"csv=p=0:s=\"#])
        .arg(file.to_str().unwrap());

    let output = cmd.output().context("Could not run `ffprobe`")?;

    let lines = output
        .stdout
        .split(|b| *b == b'\n')
        .map(std::str::from_utf8)
        .collect::<Result<Vec<_>, _>>()
        .context("`ffprobe` output contained invalid UTF-8.")?;

    let keyframes: Vec<Duration> = lines
        .into_iter()
        .map(|l| l.trim())
        .filter(|l| !l.is_empty())
        .filter_map(extract_timestamp)
        .map(parse_duration)
        .collect();

    KEYFRAME_CACHE.with(|cache| {
        let mut cache = cache.borrow_mut();
        cache.put(file_str.to_string(), keyframes.clone());
    });

    Ok(keyframes)
}

fn extract_timestamp(line: &str) -> Option<&str> {
    line.split('\\').find(|l| *l != "N/A" && !l.is_empty())
}

fn parse_duration(input: &str) -> Duration {
    let (input, micros) = input
        .split_once('.')
        .map(|(s, m)| {
            (
                s,
                m.parse::<u32>()
                    .unwrap_or_else(|_| panic!("Failure to parse duration: {}", input)),
            )
        })
        .unwrap_or((input, 0));

    let seconds = input
        .rsplit(':')
        .enumerate()
        .map(|(i, s)| s.parse::<usize>().unwrap() * (60usize.pow(i as _)))
        .sum::<usize>();

    Duration::new(seconds as u64, micros * 1000)
}

pub fn cut_into_segments(file: &Path, segments: &[Segment]) -> anyhow::Result<Vec<PathBuf>> {
    let directory = file
        .parent()
        .ok_or_else(|| anyhow!("Could not get parent directory"))?;
    let filename = file
        .file_stem()
        .and_then(|f| f.to_str())
        .ok_or_else(|| anyhow!("Could not get filename from path: {}", file.display()))?;
    let extension = file
        .extension()
        .and_then(|e| e.to_str())
        .ok_or_else(|| anyhow!("Could not get extension from path: {}", file.display()))?;

    let segment_names: Vec<_> = (0..segments.len())
        .into_iter()
        .map(|i| {
            format!(
                "{}_{i:03}_{}.{extension}",
                filename.replace('\'', ""),
                segments[i].description
            )
        })
        .collect();

    let segments = segments
        .iter()
        .zip(segment_names.clone())
        .map(|(s, name)| (format_segment(s), name))
        .flat_map(|(args, segment_name)| {
            args.split_ascii_whitespace()
                .map(|s| s.to_owned())
                .chain(std::iter::once(segment_name))
                .collect::<Vec<_>>()
        });

    let mut command = Command::new("ffmpeg");

    command
        .current_dir(directory)
        .args(["-hide_banner"])
        .args(["-nostats"])
        .args(["-loglevel", "warning"])
        .args(["-i", file.to_str().unwrap()])
        .arg("-y")
        .args(["-analyzeduration", "100M"])
        .args(["-probesize", "100M"])
        .args(segments)
        .stderr(Stdio::piped());

    #[cfg(windows)]
    {
        const CREATE_NO_WINDOW: u32 = 0x08000000;
        use std::os::windows::process::CommandExt;
        command.creation_flags(CREATE_NO_WINDOW);
    }

    let result = command
        .spawn()
        .context(format!("Failed to execute process: {:#?}", command))?;
    let result = result.wait_with_output()?;

    if !result.status.success() {
        println!("{:#?}", command);
        return Err(anyhow!("{}", String::from_utf8_lossy(&result.stderr)));
    } else if !result.stderr.is_empty() {
        eprintln!("{}", String::from_utf8_lossy(&result.stderr));
    }

    let segment_names = segment_names
        .iter()
        .map(|n| directory.join(n))
        .collect::<Vec<_>>();

    assert!(segment_names.iter().all(|n| n.exists()));
    Ok(segment_names)
}

pub fn join_segments(
    file: &Path,
    segment_names: &[PathBuf],
    mut reencode: bool,
    estimated_duration: Duration,
    progress_sender: &Sender<ProcessingState>,
) -> anyhow::Result<PathBuf> {
    let directory = file.parent().context("Failed to get parent directory.")?;
    let filename = file
        .file_stem()
        .and_then(|f| f.to_str())
        .context("Failed to get filename.")?;
    let mut extension = file
        .extension()
        .and_then(|e| e.to_str())
        .context("Failed to get extension.")?;

    let mut command = Command::new("ffmpeg");

    command
        .current_dir(directory)
        .args(["-hide_banner"])
        .args(["-nostats"])
        .args(["-loglevel", "warning"])
        .arg("-y")
        .args(["-analyzeduration", "100M"])
        .args(["-probesize", "100M"])
        .args(["-progress", "progress.txt"]);

    if segment_names.len() > 1 {
        let mut segment_index = File::create(directory.join("segment_index.txt"))
            .context("Failed to create segment index.")?;

        for segment in segment_names {
            writeln!(segment_index, "file '{}'", segment.to_str().unwrap())
                .context("Failed to write segment index.")?;
        }

        command
            .args(["-safe", "0"])
            .args(["-f", "concat"])
            .args(["-i", "segment_index.txt"]);

        if !reencode && matches!(extension, "wmv" | "avi") {
            reencode = true;
        }
    } else {
        command
            .args(["-i", segment_names[0].to_str().unwrap()])
            .arg("-dn")
            .args(["-map_metadata", "-1"]);

        reencode = true;
    }

    command.args(["-c", "copy"]);
    command.args(["-map", "0"]);

    if reencode {
        command.args(["-c:v", "libx265"]);
        command.args(["-crf", "26"]);
        command.args(["-preset", "superfast"]);
        command.args(["-max_muxing_queue_size", "9999"]);

        extension = "mp4";
    }

    let output = format!("{filename}_merged.{extension}");

    command.arg(&output);

    #[cfg(windows)]
    {
        const CREATE_NO_WINDOW: u32 = 0x08000000;
        use std::os::windows::process::CommandExt;
        command.creation_flags(CREATE_NO_WINDOW);
    }

    let mut process = command
        .spawn()
        .context(format!("Failed to execute process: {:#?}", command))?;

    let mut line = String::new();

    let progress_file = OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .truncate(true)
        .open(directory.join("progress.txt"))
        .context("Failed to create progress file.")?;

    let mut pos = 0;
    let mut reader = BufReader::new(progress_file);

    let mut last_progress = Instant::now();
    let mut frame_time = None;
    let mut encode_speed = None;

    /* #[cfg(windows)]
    w::CoInitializeEx(co::COINIT::APARTMENTTHREADED | co::COINIT::DISABLE_OLE1DDE).unwrap();

    #[cfg(windows)]
    let hwnd: w::HWND = w::HWND::GetForegroundWindow().unwrap();

    #[cfg(windows)]
    let taskbar_button = w::CoCreateInstance::<w::ITaskbarList4>(
        &co::CLSID::TaskbarList,
        None,
        co::CLSCTX::INPROC_SERVER,
    )
    .unwrap(); */

    loop {
        if progress_sender.receiver_count() == 0 {
            if let Err(e) = process.kill() {
                eprintln!("Failed to kill process: {}", e);
            }

            return Err(anyhow!("Processing cancelled."));
        }

        let resp = reader.read_line(&mut line);

        match resp {
            Ok(0) => {
                if last_progress.elapsed() > estimated_duration.div_f32(10.0) {
                    eprintln!("No progress?");
                    break;
                }

                std::thread::sleep(Duration::from_millis(500));
                reader
                    .seek(SeekFrom::Start(pos))
                    .context("Failed to seek.")?;

                frame_time = None;
                encode_speed = None;
                continue;
            }
            Ok(len) => {
                let (key, value) = match line.trim().split_once('=') {
                    Some(v) => v,
                    None => break,
                };

                match (key, value) {
                    ("progress", "end") => {
                        progress_sender.send(ProcessingState::Encoding {
                            progress: 1.0,
                            speed: 0.0,
                            eta: Duration::ZERO,
                        });
                        /* #[cfg(windows)]
                        taskbar_button.SetProgressValue(hwnd, 100, 100).unwrap(); */
                        break;
                    }
                    ("out_time", time) => {
                        frame_time = Some(parse_duration(time));
                    }
                    ("speed", speed) => {
                        encode_speed = Some(
                            speed
                                .trim()
                                .trim_end_matches('x')
                                .parse::<f32>()
                                .unwrap_or_default(),
                        );
                    }
                    (
                        "frame" | "stream_0_0_q" | "bitrate" | "total_size" | "out_time_us"
                        | "out_time_ms" | "dup_frames" | "drop_frames" | "fps",
                        _,
                    ) => (),
                    ("progress", "continue") => (),
                    _ => {
                        eprintln!("Unknown progress value: {key} = {value}");
                    }
                }

                if let (Some(time), Some(speed)) = (frame_time, encode_speed) {
                    let progress = time.as_secs_f32() / estimated_duration.as_secs_f32();
                    let duration_left = (estimated_duration.saturating_sub(time)).as_secs_f32();

                    let eta = if speed == 0.0 {
                        Duration::MAX
                    } else {
                        Duration::from_secs_f32(duration_left / speed)
                    };

                    progress_sender.send(ProcessingState::Encoding {
                        progress,
                        speed,
                        eta,
                    });

                    /* #[cfg(windows)]
                    taskbar_button
                        .SetProgressValue(hwnd, (progress * 100.0) as u64, 100)
                        .unwrap(); */

                    frame_time = None;
                    encode_speed = None;
                }

                pos += len as u64;
                line.clear();

                last_progress = Instant::now();
            }
            Err(err) => {
                eprintln!("{}", err);
                break;
            }
        }
    }

    /* #[cfg(windows)]
    w::CoUninitialize(); */

    let result = process
        .wait_with_output()
        .context("Failed to wait for process.")?;

    if !result.status.success() {
        println!(
            "{} {}",
            command.get_program().to_string_lossy(),
            command
                .get_args()
                .map(|a| a.to_string_lossy())
                .collect::<Vec<_>>()
                .join(" ")
        );
        return Err(anyhow!(
            "Failed to encode: {}\n{}",
            String::from_utf8_lossy(&result.stdout),
            String::from_utf8_lossy(&result.stderr)
        ));
    } else if !result.stderr.is_empty() {
        eprintln!("{}", String::from_utf8_lossy(&result.stderr));
    }

    let output = directory.join(&output);
    assert!(output.exists());

    Ok(output)
}

pub fn cleanup(file: &Path, segment_names: &[PathBuf]) {
    let directory = file.parent().unwrap();
    let progress_file = directory.join("progress.txt");

    if progress_file.exists() {
        if let Err(e) = std::fs::remove_file(progress_file) {
            eprintln!("{:?}", e);
        }
    }

    match segment_names {
        [] => (),
        [segment] if segment == file => (),
        segments => {
            if let Err(e) = std::fs::remove_file(directory.join("segment_index.txt")) {
                eprintln!("Failed to remove segment index: {:?}", e);
            }

            for segment in segments {
                if let Err(e) = std::fs::remove_file(segment) {
                    eprintln!("Failed to remove segment: {:?}", e);
                }
            }
        }
    }
}

fn format_segment(segment: &Segment) -> String {
    let start = format_duration(&segment.duration.start);
    let end = format_duration(&(segment.duration.end - segment.duration.start));

    format!("-ss {start} -t {end} -map 0 -dn -map_metadata -1 -c copy")
}
