use git_version::git_version;

mod core;
mod lex;

//-------------------------------------------------------------------------------------------------

fn write_version() {
    let name: &str = env!("CARGO_PKG_NAME");
    let version: &str = env!("CARGO_PKG_VERSION");
    let git_version: &str = git_version!();
    eprintln!("{name} {version} ({git_version})");
}

fn main() {
    write_version();

    let mut l = lex::Lexer::new("hello world");

    let mut tokens = vec![];
    loop {
        let t = l.next_token();
        if let (Ok(lex::Token::Eof), ..) = t { break; }
        tokens.push(t);
    }
    let (source, map) = l.close();
    let sm = core::SourceMap::new("(string)", source, map);
    for (token, span) in tokens {
        match token {
            Ok(token) => println!("{token}:\n{}", sm.show_span(span, true)),
            Err(msg) => println!("Error '{msg}':\n{}", sm.show_span(span, true))
        }
    }
}


//-------------------------------------------------------------------------------------------------
