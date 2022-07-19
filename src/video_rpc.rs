use std::{io::ErrorKind, path::PathBuf, str::FromStr, time::Duration};

use crate::util;

const STATUS_URL: &str = "http://127.0.0.1:13579/status.html";
const COMMAND_URL: &str = "http://127.0.0.1:13579/command.html";

pub(crate) fn get_player_info() -> Option<PlayerInfo> {
    let response = match minreq::get(STATUS_URL).with_timeout(5).send() {
        Ok(response) => response,
        Err(minreq::Error::IoError(err)) if err.kind() == ErrorKind::ConnectionRefused => {
            return None;
        }
        Err(e) => {
            eprintln!("{:?}", e);
            return None;
        }
    };

    let response = match response.as_str() {
        Ok(response) => response,
        Err(e) => {
            eprintln!("{:?}", e);
            return None;
        }
    };

    match response.parse() {
        Ok(parsed_info) => Some(parsed_info),
        Err(e) => {
            eprintln!("{:?}", e);
            None
        }
    }
}

pub(crate) fn send_command(command: PlayerCmd) {
    let response = minreq::post(COMMAND_URL)
        .with_body(command.to_string())
        .with_timeout(5)
        .send();

    if let Err(e) = response {
        eprintln!("{e:?}");
    }
}

#[derive(Debug, Clone)]
pub struct PlayerInfo {
    pub path: PathBuf,
    pub position: Duration,
    pub duration: Duration,
}

impl FromStr for PlayerInfo {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = s
            .strip_prefix("OnStatus(")
            .and_then(|s| s.strip_suffix(')'))
            .ok_or_else(|| anyhow::anyhow!("invalid format: {s}"))?;

        match s
            .split(',')
            .map(|s| s.trim())
            .collect::<Vec<_>>()
            .as_slice()
        {
            [_, _, position, _, duration, _, _, _, path] => Ok(Self {
                path: PathBuf::from(path.trim_matches('"')),
                position: Duration::from_millis(position.parse()?),
                duration: Duration::from_millis(duration.parse()?),
            }),
            _ => Err(anyhow::anyhow!("invalid format: {s}")),
        }
    }
}

#[derive(Copy, Clone)]
pub(crate) enum PlayerCmd {
    Seek(Duration),
    StepBackward,
    StepForward,
    ToggleStatistics,
    PrevKeyFrame,
    NextKeyFrame,
}

impl PlayerCmd {
    pub(crate) fn to_body(self) -> String {
        use PlayerCmd::*;

        match self {
            Seek(position) => format!(
                "wm_command=-1&position={}",
                util::format_duration(&position)
            ),
            StepForward => "wm_command=891&null=0".to_owned(),
            StepBackward => "wm_command=892&null=0".to_owned(),
            ToggleStatistics => "wm_command=821&null=0".to_owned(),
            PrevKeyFrame => "wm_command=897&null=0".to_owned(),
            NextKeyFrame => "wm_command=898&null=0".to_owned(),
        }
    }
}

impl std::fmt::Display for PlayerCmd {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_body())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_player_info() {
        let info: PlayerInfo = r#"OnStatus("beans.mp4", "Paused", 597208, "00:09:57", 839936, "00:14:00", 0, 30, "C:\Videos\beans.mp4")"#.parse().unwrap();

        assert_eq!(info.path.to_str(), Some(r"C:\Videos\beans.mp4"));
        assert_eq!(info.position, Duration::new(597, 208_000_000));
        assert_eq!(info.duration, Duration::new(839, 936_000_000));
    }
}
