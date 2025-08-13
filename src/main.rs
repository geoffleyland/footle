use git_version::git_version;


//-------------------------------------------------------------------------------------------------

fn write_version() {
    let name: &str = env!("CARGO_PKG_NAME");
    let version: &str = env!("CARGO_PKG_VERSION");
    let git_version: &str = git_version!();
    eprintln!("{name} {version} ({git_version})");
}

fn main() {
    write_version();
}


//-------------------------------------------------------------------------------------------------
