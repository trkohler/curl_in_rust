use crate::app::{App, Method, Parameter};
use crate::errors::{Error, HurlResult};
use crate::session::Session;

use log::{self, debug, info, log_enabled, trace};
use reqwest::multipart::Form;
use reqwest::{Client, RequestBuilder, Response, Url};
use serde_json::Value;
use std::collections::HashMap;
use std::fs::File;
use std::io::BufReader;
use std::time::Instant;

pub fn perform_method(
    app: &App,
    method: &Method,
    session: &mut Option<Session>,
) -> HurlResult<Response> {
    let method_data = method.data();
    perform(
        app,
        method.into(),
        session,
        &method_data.url,
        &method_data.parameters,
    )
}

pub fn perform(
    app: &App,
    method: reqwest::Method,
    session: &mut Option<Session>,
    raw_url: &str,
    parameters: &Vec<Parameter>,
) -> HurlResult<Response> {
    let client = Client::new();
    let url = parse(app, raw_url)?;
    debug!("Parsed url: {}", url);

    let is_multipart = parameters.iter().any(|p| p.is_form_file());
    if is_multipart {
        trace!("Making multipart request because form file was given");
        if !app.form {
            return Err(Error::NotFormButHasFormFile);
        }
    }
    let mut builder = client.request(method, url);
    builder = handle_session(
        builder,
        session,
        parameters,
        !app.read_only,
        &app.auth,
        &app.token,
    );
    builder = handle_parameters(builder, app.form, is_multipart, parameters)?;
    builder = handle_auth(builder, &app.auth, &app.token)?;

    if log_enabled!(log::Level::Info) {
        let start = Instant::now();
        let result = builder.send().map_err(From::from);
        let elapsed = start.elapsed();
        info!("Elapsed time: {:?}", elapsed);
        result
    } else {
        builder.send().map_err(From::from)
    }
}

fn parse(app: &App, s: &str) -> Result<Url, reqwest::UrlError> {
    if s.starts_with(":/") {
        return Url::parse(&format!("http://localhost{}", &s[1..]));
    } else if s.starts_with(":") {
        return Url::parse(&format!("http://localhost{}", s));
    }
    match Url::parse(s) {
        Ok(url) => Ok(url),
        Err(_e) => {
            if app.secure {
                Url::parse(&format!("https://{}", s))
            } else {
                Url::parse(&format!("http://{}", s))
            }
        }
    }
}

fn handle_parameters(
    mut builder: RequestBuilder,
    is_form: bool,
    is_multipart: bool,
    parameters: &Vec<Parameter>,
) -> HurlResult<RequestBuilder> {
    let mut data: HashMap<&String, Value> = HashMap::new();
    let mut multipart = if is_multipart {
        Some(Form::new())
    } else {
        None
    };
    for param in parameters.iter() {
        match param {
            Parameter::Header { key, value } => {
                trace!("Assing header: {}", key);
                builder = builder.header(key, value);
            }
            Parameter::Data { key, value } => {
                trace!("Adding data: {}", key);
                if multipart.is_none() {
                    data.insert(key, Value::String(value.to_owned()));
                } else {
                    multipart = multipart.map(|m| m.text(key.to_owned(), value.to_owned()));
                }
            }
            Parameter::Query { key, value } => {
                trace!("Adding query parameter: {}", key);
                builder = builder.query(&[(key, value)]);
            }
            Parameter::RawJsonData { key, value } => {
                trace!("Adding JSON data: {}", key);
                let v: Value = serde_json::from_str(value)?;
                data.insert(key, v);
            }
            Parameter::RawJsonDataFile { key, filename } => {
                trace!("Adding JSON data for key={} from file={}", key, filename);
                let file = File::open(filename)?;
                let reader = BufReader::new(file);
                let v: Value = serde_json::from_reader(reader)?;
                data.insert(key, v);
            }
            Parameter::DataFile { key, filename } => {
                trace!("Adding data from file={} for key={}", filename, key);
                let value = std::fs::read_to_string(filename)?;
                data.insert(key, Value::String(value));
            }
            Parameter::FormFile { key, filename } => {
                trace!("Adding file={} with key={}", filename, key);
                multipart = Some(
                    multipart
                        .unwrap()
                        .file(key.to_owned(), filename.to_owned())?,
                );
            }
        }
    }
    if let Some(m) = multipart {
        builder = builder.multipart(m);
    } else {
        if !data.is_empty() {
            if is_form {
                builder = builder.form(&data);
            } else {
                builder = builder.json(&data);
            }
        }
    }

    Ok(builder)
}

fn handle_auth(
    mut builder: RequestBuilder,
    auth: &Option<String>,
    token: &Option<String>,
) -> HurlResult<RequestBuilder> {
    if let Some(auth_string) = auth {
        let (username, maybe_password) = parse_auth(&auth_string)?;
        trace!("Parsed basic authentication. Username={}", username);
        builder = builder.basic_auth(username, maybe_password);
    }
    if let Some(bearer) = token {
        trace!("Parsed bearer authentication. Token={}", bearer);
        builder = builder.bearer_auth(bearer);
    }
    Ok(builder)
}

fn parse_auth(s: &str) -> HurlResult<(String, Option<String>)> {
    if let Some(idx) = s.find(":") {
        let (username, password_with_colon) = s.split_at(idx);
        let password = password_with_colon.trim_start_matches(":");
        if password.is_empty() {
            return Ok((username.to_owned(), None));
        } else {
            return Ok((username.to_owned(), Some(password.to_owned())));
        }
    } else {
        let password = rpassword::read_password_from_tty(Some("Password: "))?;
        return Ok((s.to_owned(), Some(password)));
    }
}

fn handle_session(
    mut builder: RequestBuilder,
    session: &mut Option<Session>,
    parameters: &Vec<Parameter>,
    update_session: bool,
    auth: &Option<String>,
    token: &Option<String>,
) -> RequestBuilder {
    if let Some(s) = session {
        trace!("Adding session data to request");
        builder = s.add_to_request(builder);
        if update_session {
            trace!("Updating session with parameters from this request");
            s.update_with_parameters(parameters);
            s.update_auth(auth, token);
        }
    }
    builder
}
