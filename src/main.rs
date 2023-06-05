#![allow(dead_code)]
#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

use std::{
    borrow::Cow,
    path::PathBuf,
    thread::JoinHandle,
    time::{Duration, Instant},
};

use copypasta::ClipboardProvider;
use eframe::{
    egui::{self, Button, Id, Key, PointerButton, ProgressBar, Sense, TextEdit, Widget},
    emath::{Rect, Vec2},
    epaint::{Color32, Stroke},
};
use egui_extras::{Size, StripBuilder, TableBuilder};
use processing::{KeyframeExtractionProgress, ProcessingState, TranscodeOptions};
use segment::Segment;
use util::format_duration;
use video_rpc::{get_player_info, send_command, PlayerCmd, PlayerInfo};
use watch::Sender;

mod processing;
mod segment;
mod util;
mod video_rpc;
mod watch;

fn main() {
    std::env::set_var("RUST_BACKTRACE", "1");

    let options = eframe::NativeOptions {
        always_on_top: false,
        drag_and_drop_support: false,
        transparent: false,
        ..Default::default()
    };

    eframe::run_native(
        "Video Cutter",
        options,
        Box::new(|cc| {
            cc.egui_ctx.set_visuals(egui::Visuals::dark());
            Box::new(Gui::new(cc))
        }),
    );
}

#[derive(Debug, Default)]
struct TimelineState {
    left_bound: f64,
    right_bound: f64,
    max_length: f64,
}

impl TimelineState {
    fn with_duration(duration: Duration) -> Self {
        let length = duration.as_secs_f64();

        Self {
            left_bound: 0.0,
            right_bound: length,
            max_length: length,
        }
    }

    fn contains_segment(&self, segment: &Segment) -> bool {
        if segment.is_partial() {
            (self.left_bound..=self.right_bound).contains(&segment.duration.start.as_secs_f64())
        } else {
            segment.duration.start.as_secs_f64() < self.right_bound
                && segment.duration.end.as_secs_f64() > self.left_bound
        }
    }

    fn get_normalized_position(&self, duration: Duration) -> f64 {
        let duration = duration.as_secs_f64();
        (duration - self.left_bound) / (self.right_bound - self.left_bound)
    }

    fn is_valid(&self) -> bool {
        self.max_length > 0.0
    }

    fn span(&self) -> f64 {
        self.right_bound - self.left_bound
    }

    fn zoom(&mut self, zoom_factor: f32, center: f64) {
        let mut new_left = center + (self.left_bound - center) / (zoom_factor as f64);
        let mut new_right = center + (self.right_bound - center) / (zoom_factor as f64);

        if new_left < 0.0 {
            if new_right - new_left > self.max_length {
                new_left = 0.0;
                new_right = self.max_length;
            } else {
                new_left = 0.0;
                new_right -= new_left;
            }
        }

        if new_right > self.max_length {
            if new_left > (self.max_length - new_right) {
                new_left = 0.0;
                new_right = self.max_length;
            } else {
                new_right = self.max_length;
                new_left -= new_right - self.max_length;
            }
        }

        self.left_bound = new_left;
        self.right_bound = new_right;
    }

    fn pan(&mut self, delta: f64) {
        let delta = if delta < 0.0 {
            delta.max(-self.left_bound)
        } else {
            delta.min(self.max_length - self.right_bound)
        };

        self.left_bound += delta;
        self.right_bound += delta;
    }

    fn draw_segment(
        &self,
        style: &egui::Style,
        painter: &egui::Painter,
        segment: &Segment,
        rect: &Rect,
        is_selected: bool,
    ) -> (Rect, Option<[Rect; 2]>) {
        let start_x = self.get_normalized_position(segment.duration.start) as f32 * rect.width();
        let end_x = self.get_normalized_position(segment.duration.end) as f32 * rect.width();

        let segment_rect = Rect::from_min_size(
            rect.min + Vec2::new(start_x, 0.0),
            Vec2::new(end_x - start_x, rect.height()),
        );

        let fill_color = if is_selected {
            style.visuals.selection.bg_fill
        } else {
            style.visuals.faint_bg_color
        };

        painter.rect_filled(segment_rect, style.visuals.window_rounding, fill_color);

        if !segment.description.is_empty() {
            let font_id = egui::TextStyle::Button.resolve(style);

            let text_color = if is_selected {
                style.visuals.widgets.active.fg_stroke.color
            } else {
                style.visuals.widgets.inactive.fg_stroke.color
            };

            let galley = painter.layout_no_wrap(segment.description.clone(), font_id, text_color);

            painter.galley(
                (segment_rect.center() - galley.rect.center()).to_pos2(),
                galley,
            )
        }

        if is_selected {
            let handle_size = rect.height() / 4.0;
            let handle_y = (rect.height() - handle_size) / 2.0;

            // Draw the handles.
            let left_handle_rect = Rect::from_min_size(
                segment_rect.min + Vec2::new(-handle_size / 2.0, handle_y),
                Vec2::new(handle_size, handle_size),
            );

            let right_handle_rect = Rect::from_min_size(
                segment_rect.min + Vec2::new(segment_rect.width() - (handle_size / 2.0), handle_y),
                Vec2::new(handle_size, handle_size),
            );

            for handle_rect in [left_handle_rect, right_handle_rect].iter() {
                painter.rect_filled(
                    *handle_rect,
                    style.visuals.window_rounding,
                    style.visuals.hyperlink_color,
                );
            }

            (segment_rect, Some([left_handle_rect, right_handle_rect]))
        } else {
            (segment_rect, None)
        }
    }

    fn draw_line(&self, pos: Duration, style: egui::Stroke, painter: &egui::Painter, rect: &Rect) {
        painter.vline(
            rect.min.x + self.get_normalized_position(pos) as f32 * rect.width(),
            rect.min.y..=rect.max.y,
            style,
        );
    }
}

pub struct Gui {
    segments: Vec<Segment>,
    partial_segment: Option<usize>,
    selected_segment: usize,

    timeline_state: TimelineState,

    player_info: Option<PlayerInfo>,
    player_info_channel: watch::Receiver<Option<PlayerInfo>>,

    keyframes: Vec<Duration>,
    keyframes_thread: Option<JoinHandle<anyhow::Result<Vec<Duration>>>>,
    keyframes_updates: Option<watch::Receiver<KeyframeExtractionProgress>>,

    processing_updates: Option<watch::Receiver<ProcessingState>>,

    transcode_options: TranscodeOptions,
}

impl Gui {
    pub fn new(_cc: &eframe::CreationContext<'_>) -> Self {
        let (info_tx, info_rx) = watch::channel(None);

        std::thread::spawn(move || Self::player_info_updater(info_tx));

        Self {
            segments: Vec::new(),
            partial_segment: None,
            selected_segment: 0,

            timeline_state: TimelineState::default(),

            player_info: None,
            player_info_channel: info_rx,

            keyframes: Vec::new(),
            keyframes_thread: None,
            keyframes_updates: None,

            processing_updates: None,
            transcode_options: TranscodeOptions::default(),
        }
    }

    fn player_info_updater(player_info_ch: watch::Sender<Option<PlayerInfo>>) {
        const SLEEP_INTERVAL: Duration = Duration::from_secs(1);

        loop {
            let start = Instant::now();
            player_info_ch.send(get_player_info());
            let elapsed = start.elapsed();

            if elapsed < SLEEP_INTERVAL {
                std::thread::sleep(SLEEP_INTERVAL - elapsed);
            }
        }
    }

    fn update_input(&mut self, ctx: &egui::Context) {
        if ctx.wants_keyboard_input() {
            return;
        }

        let input = ctx.input();

        for event in &input.events {
            let (key, modifiers) = if let egui::Event::Key {
                key,
                modifiers,
                pressed: true,
            } = event
            {
                (key, modifiers)
            } else {
                continue;
            };

            match key {
                Key::Space if modifiers.is_none() && !self.keyframes.is_empty() => {
                    let info = match get_player_info() {
                        Some(info) => info,
                        None => continue,
                    };

                    let prev_keyframe =
                        util::find_previous_keyframe(info.position, &self.keyframes);

                    send_command(PlayerCmd::Seek(prev_keyframe));

                    if let Some(partial_index) = self.partial_segment.take() {
                        let mut partial_segment = &mut self.segments[partial_index];
                        partial_segment.duration.end = prev_keyframe;
                        self.selected_segment = partial_index;
                    } else {
                        let partial_segment = Segment::with_start(prev_keyframe, &info.path);

                        self.segments.push(partial_segment);
                        self.segments.sort_unstable();

                        self.selected_segment = self
                            .segments
                            .iter()
                            .position(|s| s.duration.start == prev_keyframe && s.is_partial())
                            .unwrap();

                        self.partial_segment = Some(self.selected_segment);
                    }

                    self.player_info = Some(info);
                }
                /* // Undo.
                Key::Z if modifiers.matches(Modifiers::CTRL) => match self.partial_segment.take() {
                    Some(segment) => {
                        send_command(PlayerCmd::Seek(segment));
                        self.selected_segment = self.segments.len();
                    }
                    None => {
                        if let Some(segment) = self.segments.pop() {
                            send_command(PlayerCmd::Seek(segment.duration.start));
                        }

                        self.selected_segment = self.segments.len();
                    }
                }, */
                Key::ArrowUp if modifiers.is_none() && !self.segments.is_empty() => {
                    if self.selected_segment > 0 {
                        self.selected_segment -= 1;
                    }
                }
                Key::ArrowDown if modifiers.is_none() && !self.segments.is_empty() => {
                    if self.selected_segment < self.segments.len() - 1 {
                        self.selected_segment += 1;
                    }
                }
                Key::Enter if modifiers.is_none() && !self.segments.is_empty() => {
                    self.segments[self.selected_segment].seek_to_start();
                }
                Key::Delete if modifiers.is_none() && !self.segments.is_empty() => {
                    let removed_segment = self.segments.remove(self.selected_segment);

                    if removed_segment.is_partial() {
                        self.partial_segment = None;
                    }

                    if self.segments.is_empty() {
                        self.selected_segment = 0;
                    } else if self.selected_segment >= self.segments.len() {
                        self.selected_segment -= 1;
                    }
                }
                _ => (),
            }
        }
    }

    fn draw_table(&mut self, ui: &mut egui::Ui) {
        let text_height = egui::TextStyle::Body.resolve(ui.style()).size;

        TableBuilder::new(ui)
            .striped(true)
            .cell_layout(egui::Layout::left_to_right().with_cross_align(egui::Align::Center))
            .column(Size::exact(80.0))
            .column(Size::exact(80.0))
            .column(Size::remainder().at_least(100.0))
            .resizable(true)
            .header(20.0, |mut header| {
                header.col(|ui| {
                    ui.heading("Start");
                });
                header.col(|ui| {
                    ui.heading("End");
                });
                header.col(|ui| {
                    ui.heading("Label");

                    ui.with_layout(egui::Layout::right_to_left(), |ui| {
                        self.draw_clipboard_buttons(ui);
                    });
                });
            })
            .body(|body| {
                let current_path = match &self.player_info {
                    Some(info) => &info.path,
                    None => return,
                };

                let filtered_segments = self
                    .segments
                    .iter()
                    .enumerate()
                    .filter(|(_i, segment)| &segment.path == current_path)
                    .map(|(i, _segment)| i)
                    .collect::<Vec<_>>();

                body.rows(
                    text_height,
                    filtered_segments.len(),
                    |row_index, mut row| {
                        let segment = &mut self.segments[filtered_segments[row_index]];

                        if &segment.path != current_path {
                            return;
                        }

                        let start = row.col(|ui| {
                            ui.selectable_value(
                                &mut self.selected_segment,
                                filtered_segments[row_index],
                                format_duration(&segment.duration.start),
                            );
                        });

                        let end = row.col(|ui| {
                            ui.selectable_value(
                                &mut self.selected_segment,
                                filtered_segments[row_index],
                                {
                                    if !segment.is_partial() {
                                        format_duration(&segment.duration.end)
                                    } else {
                                        "N/A".to_string()
                                    }
                                },
                            );
                        });

                        let desc = row.col(|ui| {
                            ui.add_sized(
                                ui.available_size(),
                                TextEdit::singleline(&mut segment.description),
                            );
                        });

                        if start.double_clicked() {
                            send_command(PlayerCmd::Seek(segment.duration.start));
                        } else if end.double_clicked() {
                            send_command(PlayerCmd::Seek(segment.duration.end));
                        } else if desc.clicked() || desc.double_clicked() {
                            self.selected_segment = filtered_segments[row_index];
                        }
                    },
                );
            })
    }

    fn draw_clipboard_buttons(&mut self, ui: &mut egui::Ui) {
        ui.horizontal(|ui| {
            if ui
                .add_enabled(
                    !self.segments.is_empty() && self.player_info.is_some(),
                    Button::new("Copy"),
                )
                .clicked()
            {
                let current_path = self.player_info.as_ref().unwrap().path.clone();

                let current_segments: Vec<_> = self
                    .segments
                    .iter()
                    .filter(|segment| segment.path == current_path)
                    .collect();

                let segments = match serde_json::to_string(&current_segments) {
                    Ok(segments) => segments,
                    Err(err) => {
                        eprintln!("Failed to serialize segments: {}", err);
                        return;
                    }
                };

                ui.output().copied_text = segments;
            }

            let clipboard_segments: Option<Vec<Segment>> = {
                let mut clipboard = match copypasta::ClipboardContext::new() {
                    Ok(clipboard) => clipboard,
                    Err(err) => {
                        eprintln!("Failed to get clipboard: {}", err);
                        return;
                    }
                };

                let clipboard_contents = clipboard.get_contents().unwrap_or_default();
                serde_json::from_str(&clipboard_contents).ok()
            };

            if ui
                .add_enabled(
                    clipboard_segments.is_some() && self.player_info.is_some(),
                    Button::new("Paste"),
                )
                .clicked()
            {
                let current_path = self.player_info.as_ref().unwrap().path.clone();
                let mut new_segments = clipboard_segments.unwrap_or_default();

                for segment in &mut new_segments {
                    segment.path = current_path.clone();
                    segment.align_to_keyframes(true, true, &self.keyframes);
                }

                // Remove old segments associated with this path, and replace with new ones.
                self.segments.retain(|segment| segment.path != current_path);
                self.segments.extend(new_segments);

                self.selected_segment = 0;
                self.partial_segment = None;
            }
        });
    }

    fn draw_timeline(&mut self, ui: &mut egui::Ui) {
        let info = match &self.player_info {
            Some(info) => info.clone(),
            None => return,
        };

        let (pos, duration) = (info.position, info.duration.as_secs_f32());

        ui.horizontal(|ui| {
            ui.set_width(ui.available_width());
            ui.set_min_height(50.0);

            let mut segment_rects = Vec::with_capacity(self.segments.len());

            let painter = ui.painter();
            let rect = ui.max_rect();

            painter.rect_filled(
                rect,
                ui.style().visuals.window_rounding,
                ui.style().visuals.extreme_bg_color,
            );

            let (width, _) = rect.size().into();

            for (i, segment) in self
                .segments
                .iter()
                .enumerate()
                .filter(|(_i, s)| s.path == info.path)
                .filter(|(_i, s)| self.timeline_state.contains_segment(s))
            {
                if segment.is_partial() {
                    self.timeline_state.draw_line(
                        segment.duration.start,
                        Stroke::new(1.0, Color32::RED),
                        painter,
                        &rect,
                    );
                    continue;
                }

                let (rect, handles) = self.timeline_state.draw_segment(
                    ui.style(),
                    painter,
                    segment,
                    &rect,
                    i == self.selected_segment,
                );

                segment_rects.push((i, rect, handles));
            }

            self.timeline_state
                .draw_line(pos, ui.style().visuals.selection.stroke, painter, &rect);

            let mut interacted_with_segment = false;
            let mut new_position = None;

            for (i, rect, handles) in segment_rects {
                if let Some([left, right]) = handles {
                    let left_response = ui.allocate_rect(left, Sense::drag());
                    let right_response = ui.allocate_rect(right, Sense::drag());

                    let segment = &mut self.segments[i];

                    if !interacted_with_segment {
                        interacted_with_segment = left_response.clicked()
                            || left_response.dragged()
                            || right_response.clicked()
                            || right_response.dragged();
                    }

                    if left_response.dragged_by(PointerButton::Primary) {
                        let delta_pos = (left_response.drag_delta().x / width) * duration;
                        segment.shift_start(delta_pos);

                        new_position = Some(segment.duration.start);
                        continue;
                    } else if right_response.dragged_by(PointerButton::Primary) {
                        let delta_pos = (right_response.drag_delta().x / width) * duration;
                        segment.shift_end(delta_pos, duration);

                        new_position = Some(segment.duration.end);
                        continue;
                    } else if left_response.drag_released() {
                        segment.align_to_keyframes(true, false, &self.keyframes);
                    } else if right_response.drag_released() {
                        segment.align_to_keyframes(false, true, &self.keyframes);
                    }
                }

                let response = ui.allocate_rect(rect, Sense::click_and_drag());

                if !interacted_with_segment {
                    interacted_with_segment = response.clicked() || response.dragged();
                }

                if response.clicked() {
                    self.selected_segment = i;
                } else if response.dragged_by(PointerButton::Primary) {
                    self.selected_segment = i;
                    let delta_pos = (response.drag_delta().x / width) * duration;

                    let segment = &mut self.segments[i];
                    segment.shift(delta_pos, duration);

                    new_position = Some(segment.duration.start);
                } else if response.drag_released() {
                    let segment = &mut self.segments[i];
                    segment.align_to_keyframes(true, true, &self.keyframes);
                }
            }

            let timeline_response = ui.allocate_rect(rect, Sense::click_and_drag());

            if !interacted_with_segment {
                if (timeline_response.drag_started()
                    && timeline_response.dragged_by(PointerButton::Primary))
                    || timeline_response.clicked()
                {
                    if let Some(mouse_pos) = ui.ctx().pointer_interact_pos() {
                        let mouse_timeline_pos = mouse_pos.x - rect.min.x;
                        let new_pos = ((mouse_timeline_pos / width) * duration)
                            .min(duration)
                            .max(0.0);

                        new_position = Some(Duration::from_secs_f32(new_pos));
                    }
                } else if timeline_response.dragged_by(PointerButton::Primary) {
                    let delta_pos = (timeline_response.drag_delta().x / width) * duration;
                    let new_pos = (pos.as_secs_f32() + delta_pos).min(duration).max(0.0);
                    new_position = Some(Duration::from_secs_f32(new_pos));
                }
            }

            if let Some(hover_pos) = timeline_response.hover_pos() {
                if timeline_response.dragged_by(PointerButton::Secondary) {
                    let delta_pos = (timeline_response.drag_delta().x / width) as f64
                        * self.timeline_state.span();
                    self.timeline_state.pan(-delta_pos);
                } else if ui.input().scroll_delta.y != 0.0 {
                    let delta_pos =
                        (ui.input().scroll_delta.y / width) as f64 * self.timeline_state.span();
                    self.timeline_state.pan(delta_pos);
                } else if ui.input().zoom_delta() != 0.0 {
                    let hover_duration = (hover_pos.x / width) as f64 * self.timeline_state.span()
                        + self.timeline_state.left_bound;

                    self.timeline_state
                        .zoom(ui.input().zoom_delta(), hover_duration);
                }
            }

            if let Some(new_position) = new_position {
                send_command(PlayerCmd::Seek(new_position));

                if let Some(mut info) = self.player_info.as_mut() {
                    info.position = new_position;
                }
            }
        });
    }

    fn draw_bottom_bar(&mut self, ui: &mut egui::Ui) {
        ui.horizontal(|ui| {
            let mut encoding_enabled = ui
                .ctx()
                .data()
                .get_persisted(Id::new("encoding_enabled"))
                .unwrap_or(false);

            if self.processing_updates.is_some() {
                if ui.button("Cancel").clicked() {
                    self.processing_updates = None;
                }
            } else {
                ui.add_enabled_ui(
                    !self.keyframes.is_empty() && (!self.segments.is_empty() || encoding_enabled),
                    |ui| {
                        if ui.button("Process").clicked() {
                            self.process(encoding_enabled);
                        }
                    },
                );
            }

            if ui.checkbox(&mut encoding_enabled, "Re-encode").changed() {
                ui.ctx()
                    .data()
                    .insert_persisted(Id::new("encoding_enabled"), encoding_enabled);
            }

            ui.add_space(10.0);

            if let Some(update) = &self.processing_updates.as_mut().map(|up| up.get()) {
                let (percentage, text) = match update {
                    ProcessingState::Starting => (0.00, "Starting processing...".into()),
                    ProcessingState::Splitting => (0.25, "Splitting video into chunks...".into()),
                    ProcessingState::Merging => (0.50, "Merging chunks into a new video...".into()),
                    ProcessingState::Encoding {
                        progress,
                        speed,
                        eta,
                    } => {
                        let speed = ui.ctx().animate_value_with_time(
                            Id::new("process_encoding_speed"),
                            *speed,
                            0.5,
                        );

                        (
                            *progress,
                            Cow::Owned(format!(
                                "Encoding video... [{speed:.1}x] ({} remaining...)",
                                util::format_duration_human(eta)
                            )),
                        )
                    }
                    ProcessingState::CleaningUp => (0.75, "Cleaning up...".into()),
                    ProcessingState::Done => {
                        self.processing_updates = None;
                        (1.00, "Done!".into())
                    }
                    ProcessingState::Failed => {
                        self.processing_updates = None;
                        (1.00, "Failed!".into())
                    }
                };

                let percentage = ui.ctx().animate_value_with_time(
                    Id::new("process_percentage"),
                    percentage,
                    0.5,
                );

                ProgressBar::new(percentage)
                    .text(text.as_ref())
                    .animate(true)
                    .ui(ui);
            } else if encoding_enabled {
                self.transcode_options.draw(ui);
            }
        });
    }

    fn process(&mut self, encode: bool) {
        let file = if let Some(PlayerInfo { path, .. }) = &self.player_info {
            path.to_owned()
        } else {
            return;
        };

        let segments = self
            .segments
            .iter()
            .filter(|s| s.path == file)
            .cloned()
            .collect::<Vec<_>>();

        let estimated_duration = if !segments.is_empty() {
            segments.iter().map(|s| s.span()).sum()
        } else {
            self.player_info.as_ref().unwrap().duration
        };

        let (tx, rx) = watch::channel(ProcessingState::Starting);
        self.processing_updates = Some(rx);
        let options = self.transcode_options;

        std::thread::spawn(move || {
            Self::process_thread(file, segments, encode, options, tx, estimated_duration);
        });
    }

    fn process_thread(
        file: PathBuf,
        segments: Vec<Segment>,
        encode: bool,
        options: TranscodeOptions,
        tx: Sender<ProcessingState>,
        estimated_duration: Duration,
    ) {
        if tx.receiver_count() == 0 {
            return;
        }

        let segment_names = if !segments.is_empty() {
            tx.send(ProcessingState::Splitting);

            let names = match processing::cut_into_segments(&file, &segments) {
                Ok(names) => names,
                Err(e) => {
                    eprintln!("Failed to split video: {:?}", e);
                    tx.send(ProcessingState::Failed);
                    return;
                }
            };

            tx.send(ProcessingState::Merging);
            names
        } else {
            tx.send(ProcessingState::Encoding {
                progress: 0.0,
                speed: 0.0,
                eta: estimated_duration,
            });

            vec![file.clone()]
        };

        if tx.receiver_count() == 0 {
            processing::cleanup(&file, &segment_names);
            return;
        }

        if let Err(e) = processing::join_segments(
            &file,
            &segment_names,
            encode,
            options,
            estimated_duration,
            &tx,
        ) {
            eprintln!("Failed to join segments: {:?}", e);
            tx.send(ProcessingState::Failed);
            processing::cleanup(&file, &segment_names);
            return;
        }

        tx.send(ProcessingState::CleaningUp);
        processing::cleanup(&file, &segment_names);
        tx.send(ProcessingState::Done);
    }
}

impl eframe::App for Gui {
    fn update(&mut self, ctx: &egui::Context, frame: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |ui| {
            if let Some(update) = self.player_info_channel.get_if_new() {
                let mut update_keyframes = false;

                let info = match (&self.player_info, update) {
                    (
                        _,
                        Some(PlayerInfo {
                            duration: Duration::ZERO,
                            ..
                        }),
                    ) => {
                        self.player_info = None;
                        self.timeline_state = TimelineState::default();
                        return;
                    }

                    (Some(old), Some(new)) if old.path != new.path => {
                        frame.set_window_title(&format!(
                            "Connected to {}",
                            new.path.to_string_lossy()
                        ));
                        update_keyframes = true;

                        self.keyframes.clear();
                        self.timeline_state = TimelineState::with_duration(new.duration);
                        new
                    }
                    (Some(_), Some(info)) => info,
                    (None, Some(info)) => {
                        frame.set_window_title(&format!(
                            "Connected to {}",
                            info.path.to_string_lossy()
                        ));

                        update_keyframes = true;
                        self.timeline_state = TimelineState::with_duration(info.duration);
                        info
                    }
                    (Some(_), None) => {
                        self.keyframes.clear();

                        self.timeline_state = TimelineState::default();
                        self.player_info = None;

                        frame.set_window_title("Listening for videos...");
                        return;
                    }
                    (None, None) => return,
                };

                if update_keyframes {
                    let (progress_sender, progress_receiver) =
                        watch::channel(KeyframeExtractionProgress::default());

                    let path = info.path.clone();

                    let keyframes_thread = std::thread::spawn(move || {
                        processing::extract_keyframes(&path, progress_sender)
                    });

                    self.keyframes_thread = Some(keyframes_thread);
                    self.keyframes_updates = Some(progress_receiver);
                }

                self.player_info = Some(info);
            }

            if let Some(thread) = self.keyframes_thread.as_ref() {
                if thread.is_finished() {
                    let thread = self.keyframes_thread.take().unwrap();

                    self.keyframes = match thread.join() {
                        Ok(Ok(keyframes)) => keyframes,
                        Ok(Err(e)) => {
                            eprintln!("Failed to extract keyframes: {e:?}");
                            Vec::new()
                        }
                        Err(e) => {
                            eprintln!("Keyframes thread panicked: {e:?}");
                            Vec::new()
                        }
                    };

                    if let Some(info) = &self.player_info {
                        self.keyframes.push(info.duration);
                    }

                    if !self.keyframes.is_empty() && self.keyframes[0] != Duration::ZERO {
                        self.keyframes.insert(0, Duration::ZERO);
                    }
                } else {
                    let keyframe_progress = self
                        .keyframes_updates
                        .as_mut()
                        .map(|p| p.get())
                        .unwrap_or_default();

                    StripBuilder::new(ui)
                        .size(Size::exact(25.0))
                        .size(Size::remainder())
                        .vertical(|mut strip| {
                            strip.cell(|ui| self.draw_bottom_bar(ui));
                            strip.cell(|ui| {
                                ui.horizontal(|ui| {
                                    ui.label("Fetching keyframes:");
                                    ui.add_space(10.0);

                                    let percentage = ui.ctx().animate_value_with_time(
                                        Id::new("keyframe_percentage"),
                                        keyframe_progress.progress,
                                        0.5,
                                    );

                                    ProgressBar::new(percentage)
                                        .show_percentage()
                                        .animate(true)
                                        .ui(ui);
                                });
                            });
                        });
                }
            } else if !self.keyframes.is_empty() {
                StripBuilder::new(ui)
                    .size(Size::initial(25.0))
                    .size(Size::remainder())
                    .size(Size::relative(0.15))
                    .vertical(|mut strip| {
                        strip.cell(|ui| self.draw_bottom_bar(ui));
                        strip.cell(|ui| self.draw_table(ui));
                        strip.cell(|ui| self.draw_timeline(ui));
                    });
            } else {
                StripBuilder::new(ui)
                    .size(Size::exact(25.0))
                    .size(Size::remainder())
                    .vertical(|mut strip| {
                        strip.cell(|ui| self.draw_bottom_bar(ui));
                        strip.cell(|ui| {
                            ui.horizontal(|ui| {
                                ui.label("Waiting for video...");
                            });
                        });
                    });
            }
        });

        self.update_input(ctx);
    }

    fn on_exit_event(&mut self) -> bool {
        true
    }
}
