use std::{
    ops::Range,
    path::{Path, PathBuf},
    time::Duration,
};

use serde::{Deserialize, Serialize};

use crate::{
    util,
    video_rpc::{send_command, PlayerCmd},
};

#[derive(Debug, Default, Clone, Eq, Serialize, Deserialize)]
pub struct Segment {
    pub duration: Range<Duration>,
    pub description: String,
    #[serde(default, skip_serializing)]
    pub path: PathBuf,
}

impl PartialEq for Segment {
    fn eq(&self, other: &Self) -> bool {
        self.duration == other.duration
    }
}

impl PartialOrd for Segment {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Segment {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.duration.start.cmp(&other.duration.start)
    }
}

impl Segment {
    pub fn with_range(duration: Range<Duration>, path: &Path) -> Self {
        Self {
            duration,
            path: path.to_path_buf(),
            ..Default::default()
        }
    }

    pub fn with_start(duration: Duration, path: &Path) -> Self {
        Self::with_range(duration..duration, path)
    }

    pub fn seek_to_start(&self) {
        send_command(PlayerCmd::Seek(self.duration.start));
    }

    pub fn seek_to_end(&self) {
        if !self.is_partial() {
            send_command(PlayerCmd::Seek(self.duration.end));
        }
    }

    pub fn span(&self) -> Duration {
        self.duration.end - self.duration.start
    }

    pub fn is_partial(&self) -> bool {
        self.duration.start == self.duration.end
    }

    pub fn shift(&mut self, seconds: f32, max: f32) {
        let delta = Duration::from_secs_f32(seconds.abs());
        let max = Duration::from_secs_f32(max);

        let delta = if seconds.is_sign_positive() && self.duration.end + delta >= max {
            max.saturating_sub(self.duration.end)
        } else if seconds.is_sign_negative() && delta > self.duration.start {
            self.duration.start
        } else {
            delta
        };

        if seconds.is_sign_negative() {
            self.duration = (self.duration.start.saturating_sub(delta))
                ..(self.duration.end.saturating_sub(delta));
        } else {
            self.duration = (self.duration.start + delta)..(self.duration.end + delta);
        }
    }

    pub fn shift_start(&mut self, seconds: f32) {
        let delta = Duration::from_secs_f32(seconds.abs());

        if seconds.is_sign_positive() && self.span() < delta {
            return;
        }

        let delta = if seconds.is_sign_negative() && delta > self.duration.start {
            self.duration.start
        } else {
            delta
        };

        if seconds.is_sign_negative() {
            self.duration.start = self.duration.start.saturating_sub(delta);
        } else {
            self.duration.start += delta;
        }
    }

    pub fn shift_end(&mut self, seconds: f32, max: f32) {
        let delta = Duration::from_secs_f32(seconds.abs());
        let max = Duration::from_secs_f32(max);

        if seconds.is_sign_negative() && self.span() < delta {
            return;
        }

        let delta = if seconds.is_sign_positive() && self.duration.end + delta >= max {
            max.saturating_sub(self.duration.end)
        } else {
            delta
        };

        if seconds.is_sign_negative() {
            self.duration.end = self.duration.end.saturating_sub(delta);
        } else {
            self.duration.end += delta;
        }
    }

    pub fn align_to_keyframes(
        &mut self,
        adjust_start: bool,
        adjust_end: bool,
        keyframes: &[Duration],
    ) {
        if adjust_start {
            self.duration.start = util::find_closest_keyframe(self.duration.start, keyframes);

            if self.duration.start == self.duration.end {
                let start_idx = keyframes
                    .iter()
                    .position(|&k| k == self.duration.start)
                    .unwrap();

                self.duration.start = keyframes[start_idx - 1];
            }
        }

        if adjust_end {
            self.duration.end = util::find_closest_keyframe(self.duration.end, keyframes);

            if self.duration.end == self.duration.start {
                let end_idx = keyframes
                    .iter()
                    .position(|&k| k == self.duration.end)
                    .unwrap();

                if keyframes.len() > end_idx + 1 {
                    self.duration.end = keyframes[end_idx + 1];
                }
            }
        }
    }
}
