use std::{
    collections::HashMap,
    io::{self, Write},
    sync::{Arc, Mutex},
};

use armchair::{
    parser::parser::{compile_one_behavior, CompileError, ParseResult},
    Behavior, Constraint, ExecuteEnvironment,
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
                Ok((ParseResult::Behavior((name, behavior)), code)) => {
                    behaviors.insert(name, behavior);
                    builder = code.into();
                }
                Ok((ParseResult::Question(agent), code)) => {
                    builder = code.into();

                    let constraints = Arc::new(Mutex::new(HashMap::new()));
                    // 実行
                    let environment = ExecuteEnvironment::new(&behaviors, constraints.clone());

                    let res = agent.solve(&environment);

                    match res {
                        Ok(_) => {
                            for (variable, constraint) in constraints.lock().unwrap().iter() {
                                if let Constraint::EqualTo(atom) = constraint {
                                    println!("{} = {};", variable, atom.to_string())
                                }
                            }
                        }
                        Err(_) => println!("Contradiction detected.\n"),
                    }
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
