use crate::errors::{Error, HurlResult};
use syntect::highlighting::ThemeSet;
use syntect::parsing::syntax_definition::SyntaxDefinition;
use syntect::parsing::{SyntaxSet, SyntaxSetBuilder};

pub fn build() -> HurlResult<(SyntaxSet, ThemeSet)> {
    let mut builder = SyntaxSetBuilder::new();
    let http_syntax_def = include_str!("../HTTP.sublime-syntax");
    let def = SyntaxDefinition::load_from_str(http_syntax_def, true, None)
        .map_err(|_| Error::SyntaxLoadError("HTTP"))?;
    builder.add(def);

    let json_syntax_def = include_str!("../JSON.sublime-syntax");
    let json_def = SyntaxDefinition::load_from_str(json_syntax_def, true, None)
    .map_err(|_| Error::SyntaxLoadError("JSON"))?;
    builder.add(json_def);

    let ss = builder.build();
    let ts = ThemeSet::load_defaults();
    Ok((ss, ts))
}
