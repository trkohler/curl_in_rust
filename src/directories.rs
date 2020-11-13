use lazy_static::lazy_static;
use std::path::{Path, PathBuf};

#[cfg(target_os = "macos")]
use std::env;

pub struct Directories {
    config: PathBuf,
}

impl Directories {
    fn new() -> Option<Directories> {
        #[cfg(target_os = "macos")]
        let config_op = env::var_os("XDG_CONFIG_HOME")
            .map(PathBuf::from)
            .filter(|p| p.is_absolute())
            .or_else(|| dirs::home_dir().map(|d| d.join(".config")));

        #[cfg(not(target_os = "macos"))]
        let config_op = dirs::config_dir();

        let config = config_op.map(|d| d.join("hurl"))?;

        Some(Directories { config })
    }

    pub fn config(&self) -> &Path {
        &self.config
    }
}

lazy_static! {
    pub static ref DIRECTORIES: Directories =
        Directories::new().expect("Could not get home directory");
}
