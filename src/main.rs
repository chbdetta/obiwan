use clap::{App, Arg};
use obiwan::{run_file, run_string};

fn main() {
    let matches = App::new("obiwan")
        .author("Hayden C. <hchen222@ucsc.edu>")
        .arg(Arg::with_name("input").index(1))
        .arg(
            Arg::with_name("file")
                .short("f")
                .long("file")
                .takes_value(true),
        )
        .get_matches();

    match matches.value_of("file") {
        Some(file_path) => {
            run_file(file_path);
        }
        _ => match matches.value_of("input") {
            Some(src) => {
                run_string(src);
            }
            _ => panic!("No file nor input provided"),
        },
    }
}
