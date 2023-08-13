use std::{
    collections::HashMap,
    io::{self, Write},
};

use armchair::{
    parser::parser::{compile_one_behavior, CompileError},
    Behavior,
};

fn main() {
    println!("Armchair v1.0.0");

    let mut behaviors: HashMap<String, Behavior> = HashMap::new();
    let mut builder = "".to_string();

    loop {
        if builder.is_empty() {
            print!("\narmchair>");
        } else {
            print!("     ...>");
        }
        io::stdout().flush().unwrap();
        if let Err(_) = io::stdin().read_line(&mut builder) {
            return;
        }
        'find_behavior: loop {
            match compile_one_behavior(&builder) {
                Ok(((name, behavior), code)) => {
                    behaviors.insert(name, behavior);
                    builder = code;
                }
                Err(CompileError::Incomplete) => {
                    break 'find_behavior;
                }
                Err(CompileError::Error(message)) => {
                    println!("{}", message);

                    builder = "".into();
                    break 'find_behavior;
                }
            }
        }
    }
}
