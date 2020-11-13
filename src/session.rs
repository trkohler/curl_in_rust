use crate::app::{App, Parameter};
use crate::directories::DIRECTORIES;
use crate::errors::HurlResult;

use reqwest::header::COOKIE;
use reqwest::RequestBuilder;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs::{create_dir_all, File, OpenOptions};
use std::io::{BufReader, BufWriter};
use std::path::PathBuf;

#[derive(Debug, Default, Serialize, Deserialize)]
pub struct Session {
    path: PathBuf,
    name: String,
    host: String,
    auth: Option<String>,
    token: Option<String>,
    headers: HashMap<String, String>,
    cookies: Vec<(String, String)>,
}

impl Session {
    pub fn new(app: &App, name: String, host: String) -> Self {
        let path = Session::path(app, &name, &host);
        Session {
            path,
            name,
            host,
            ..Default::default()
        }
    }

    pub fn load(app: &App, name: &str, host: &str) -> HurlResult<Self> {
        let path = Session::path(app, name, host);
        let file = File::open(path)?;
        let reader = BufReader::new(file);
        serde_json::from_reader(reader).map_err(|e| e.into())
    }

    pub fn get_or_create(app: &App, name: String, host: String) -> Self {
        match Session::load(app, &name, &host) {
            Ok(session) => session,
            Err(_) => Session::new(app, name, host),
        }
    }

    fn path(app: &App, name: &str, host: &str) -> PathBuf {
        let mut session_dir = Session::dir(app, host);
        let mut filename = make_safe_pathname(name);
        filename.push_str(".json");
        session_dir.push(filename);
        session_dir
    }

    fn dir(app: &App, host: &str) -> PathBuf {
        let mut session_dir = app
            .session_dir
            .as_ref()
            .cloned()
            .filter(|session_dir| session_dir.is_dir())
            .unwrap_or_else(|| DIRECTORIES.config().join("sessions"));
        session_dir.push(make_safe_pathname(host));
        session_dir
    }

    pub fn save(&self, app: &App) -> HurlResult<()> {
        let dir = Session::dir(app, &self.host);
        create_dir_all(dir)?;
        let file = OpenOptions::new()
            .create(true)
            .write(true)
            .truncate(true)
            .open(&self.path)?;
        let writer = BufWriter::new(file);
        serde_json::to_writer(writer, &self).map_err(|e| e.into())
    }

    pub fn update_with_parameters(&mut self, parameters: &Vec<Parameter>) {
        for parameter in parameters.iter() {
            match parameter {
                Parameter::Header { key, value } => {
                    let lower_key = key.to_ascii_lowercase();
                    if lower_key.starts_with("content-") || lower_key.starts_with("if-") {
                        continue;
                    }
                    self.headers.insert(key.clone(), value.clone());
                }
                _ => {}
            }
        }
    }

    pub fn update_auth(&mut self, auth: &Option<String>, token: &Option<String>) {
        if auth.is_some() {
            self.auth = auth.clone()
        }

        if token.is_some() {
            self.token = token.clone();
        }
    }

    pub fn add_to_request(&self, mut builder: RequestBuilder) -> RequestBuilder {
        for (key, value) in self.headers.iter() {
            builder = builder.header(key, value);
        }
        let cookies = self
            .cookies
            .iter()
            .map(|(name, value)| format!("{}={}", name, value))
            .collect::<Vec<String>>()
            .join("; ");
        if cookies.is_empty() {
            return builder;
        }
        builder.header(COOKIE, cookies)
    }

    pub fn update_with_response(&mut self, resp: &reqwest::Response) {
        for cookie in resp.cookies() {
            self.cookies
                .push((cookie.name().to_owned(), cookie.value().to_owned()));
        }
    }
}

pub fn make_safe_pathname(s: &str) -> String {
    let mut buf = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-' | ' ' => buf.push(c),
            _ => buf.push('_'),
        }
    }
    buf
}