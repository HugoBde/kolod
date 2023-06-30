use std::env::args;
use std::process;

use kolod::gba::GBA;
use simple_logger::SimpleLogger;

fn main() {

    // Initialise logger
    SimpleLogger::new().init().unwrap();

    // Pull command-line args
    let mut args = args();

    let mut my_gba = GBA::new();

    println!("{:?}", my_gba.cpu);

    // Load game in memory
    if let Some(file_name) = args.nth(1) {

        match my_gba.load_game(&file_name) {
            Ok(_) => log::info!("Successfully loaded game: {}", file_name),
            Err(e) => log::error!("Error: {}", e),
        }
    } else {

        println!("Usage: ./kolod <game file name>");

        process::exit(1);
    }

    my_gba.run();
}
