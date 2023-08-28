pub mod executors {
    use std::collections::HashMap;

    use crate::{
        parser::parser::{compile_one_behavior, CompileError, ParseResult},
        Behavior, Constraint, ExecuteEnvironment,
    };

    pub fn execute(code: &str) -> Result<Vec<HashMap<String, Constraint>>, String> {
        let mut behaviors: HashMap<String, Behavior> = HashMap::new();
        let mut code_mut = code.clone();
        let mut results = vec![];

        while !code_mut.is_empty() {
            match compile_one_behavior(&code_mut) {
                Ok((ParseResult::Behavior((name, behavior)), remain_code)) => {
                    behaviors.insert(name, behavior);
                    code_mut = remain_code.into();
                }
                Ok((ParseResult::Question(agent), remain_code)) => {
                    code_mut = remain_code.into();

                    // 実行
                    let environment =
                        ExecuteEnvironment::new(&behaviors, agent.variables().into_iter());

                    let res = agent.solve(&environment);

                    match res {
                        Ok(_) => {
                            let mut map = HashMap::new();
                            for variable in agent.global_variables() {
                                let cons = environment.key_store().get_constraint(&variable);
                                if cons.is_err() {
                                    assert!(false)
                                }

                                map.insert(variable, cons.unwrap());
                            }
                            results.push(map);
                        }
                        Err(_) => return Err("Execute Failed".into()),
                    }
                }
                Err(CompileError::Error(err)) => return Err(err),
                Err(CompileError::Incomplete) => return Err("Incomplete".to_string()),
            }
        }

        Ok(results)
    }
}
