use std::time::Duration;

use eframe::{
    egui::plot::{Polygon, Value, Values},
    emath::Rect,
};

pub fn format_duration(duration: &Duration) -> String {
    let hours = duration.as_secs() / 3600;
    let minutes = (duration.as_secs() % 3600) / 60;
    let seconds = duration.as_secs() % 60;
    let millis = duration.subsec_millis();

    format!("{:02}:{:02}:{:02}.{}", hours, minutes, seconds, millis)
}

pub fn format_duration_human(duration: &Duration) -> String {
    if *duration == Duration::MAX {
        return "Unknown".to_string();
    }

    match [
        duration.as_secs() / 3600,
        (duration.as_secs() % 3600) / 60,
        duration.as_secs() % 60,
    ] {
        [0, 0, 0] => "0 seconds".to_string(),
        [0, 0, s] => format!("{s} second{}", (s > 1).then(|| "s").unwrap_or_default()),
        [0, m, 0] => format!("{m} minute{}", (m > 1).then(|| "s").unwrap_or_default()),
        [0, m, s] => format!(
            "{m} minute{} and {s} second{}",
            (m > 1).then(|| "s").unwrap_or_default(),
            (s > 1).then(|| "s").unwrap_or_default()
        ),
        [h, 0, _] => format!("{h} hour{}", (h > 1).then(|| "s").unwrap_or_default(),),
        [h, m, _] => format!(
            "{h} hour{} and {m} minute{}",
            (h > 1).then(|| "s").unwrap_or_default(),
            (m > 1).then(|| "s").unwrap_or_default()
        ),
    }
}

pub fn find_closest_keyframe(time: Duration, keyframes: &[Duration]) -> Duration {
    let partition_point = keyframes.partition_point(|k| *k <= time);
    let (smaller, bigger) = (
        &keyframes[partition_point.saturating_sub(1)],
        &keyframes[partition_point.min(keyframes.len() - 1)],
    );

    if time.saturating_sub(*smaller) < bigger.saturating_sub(time) {
        *smaller
    } else {
        *bigger
    }
}

pub fn find_previous_keyframe(time: Duration, keyframes: &[Duration]) -> Duration {
    let partition_point = keyframes.partition_point(|k| *k <= time);
    keyframes[partition_point.saturating_sub(1)]
}

pub fn rect_into_polygon(rect: &Rect) -> Polygon {
    let (x, y) = rect.left_bottom().into();
    let (w, h) = rect.size().into();

    let points = [(x, y), (x + w, y), (x + w, y + h), (x, y + h)]
        .into_iter()
        .map(|(x, y)| Value::new(x, y));

    Polygon::new(Values::from_values_iter(points))
}
