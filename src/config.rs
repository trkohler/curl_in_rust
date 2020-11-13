use serde::Deserialize;
use std::fs;
use std::path::PathBuf;

use crate::app::App;
use crate::directories::DIRECTORIES;

#[derive(Debug, Deserialize)]
pub struct Config {
    pub verbose: Option<u8>,
    pub form: Option<bool>,
    pub auth: Option<String>,
    pub token: Option<String>,
    pub secure: Option<bool>,
}

pub fn config_file(app: &App) -> PathBuf {
    app.config
        .as_ref()
        .cloned()
        .filter(|config_path| config_path.is_file())
        .unwrap_or_else(|| DIRECTORIES.config().join("config"))
}

pub fn read_config_file(path: PathBuf) -> Option<Config> {
    fs::read_to_string(path).ok().map(|content| {
        let config: Config = toml::from_str(&content).unwrap();
        config
    })
}
