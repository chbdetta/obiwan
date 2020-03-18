use clap::{App, Arg};
use obiwan::{run_file, run_string, Mode};

fn main() {
    let matches = App::new("obiwan")
        .author("Hayden C. <hchen222@ucsc.edu>")
        .arg(Arg::with_name("input").index(1))
        .arg(
            Arg::with_name("token")
                .short("t")
                .long("token")
                .takes_value(true)
                .help("Output TokenList instead of evaluation result"),
        )
        .arg(
            Arg::with_name("code")
                .long("code")
                .takes_value(true)
                .help("Output formatted code instead of evaluation result"),
        )
        .arg(
            Arg::with_name("ast")
                .short("a")
                .long("ast")
                .takes_value(true)
                .help("Output AST instead of evaluation result"),
        )
        .arg(
            Arg::with_name("file")
                .short("f")
                .long("file")
                .takes_value(true)
                .help("The file path to parse"),
        )
        .get_matches();

    let mode = if matches.is_present("code") {
        Mode::Code
    } else if matches.is_present("ast") {
        Mode::Ast
    } else if matches.is_present("token") {
        Mode::Token
    } else {
        Default::default()
    };

    match matches.value_of("file") {
        Some(file_path) => {
            run_file(file_path, mode);
        }
        _ => match matches.value_of("input") {
            Some(src) => {
                run_string(src, mode);
            }
            _ => panic!("No file nor input provided"),
        },
    }
}
