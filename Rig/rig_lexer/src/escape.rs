pub fn escape(ch: char) -> Result<char, ()> {
    match ch {
        'n' => Ok('\n'),
        'r' => Ok('\r'),
        't' => Ok('\t'),
        '\\' => Ok('\\'),
        '\"' => Ok('"'),
        _ => Err(()),
    }
}
