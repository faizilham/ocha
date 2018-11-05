extern crate ocha;

use std::env;
use std::process;

fn main() {
    let args : Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: ocha filename");
    } else {
        if let Err(_) = ocha::run_file(args[1].clone()){
            process::exit(1);
        }
    }
}
