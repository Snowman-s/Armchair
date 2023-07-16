use num_rational::Rational64;
use std::collections::HashMap;

struct AskTerm {
    prove: fn(constraints: &HashMap<String, Constraint>) -> ConstraintCheckResult,
}

#[derive(PartialEq, Eq, Clone, Debug)]
enum Constraint {
    None,
    String(String),
    Number(Rational64),
    Atom(String),
}

struct Constraints {
    /** 変数名:制約*/
    constraints: HashMap<String, Constraint>,
}

impl Constraints {
    fn tell(&mut self, variable_id: String, c: &Constraint) -> ConstraintCheckResult {
        if let Some(s) = self.constraints.get(&variable_id) {
            if s != &Constraint::None && s != c {
                return ConstraintCheckResult::CONTRADICTION;
            }
        }

        self.constraints.insert(variable_id.clone(), c.clone());
        let mut ret_map: HashMap<String, Constraint> = HashMap::new();
        ret_map.insert(variable_id, c.clone());
        ConstraintCheckResult::SUCCEED(ret_map)
    }

    fn ask(&self, ask_term: &AskTerm) -> ConstraintCheckResult {
        (ask_term.prove)(&self.constraints)
    }

    fn get_constraint(&self, variable_id: String) -> Option<Constraint> {
        self.constraints.get(&variable_id).cloned()
    }
}

enum ConstraintCheckResult {
    /**  constraints の一部を返す*/
    SUCCEED(HashMap<String, Constraint>),
    CONTRADICTION,
}

struct ExecuteEnvironment<'a> {
    behaviors: &'a HashMap<String, Behavior>,
    key_store: Constraints,
}

struct Behavior {
    variable_list: Vec<String>,
    solve: fn(environment: &mut ExecuteEnvironment) -> Result<(), ()>,
}

fn call(
    env: &mut ExecuteEnvironment,
    question: &Behavior,
    argument_variables: Vec<String>,
) -> Result<(), ()> {
    let mut new_key_store = HashMap::new();
    for (index, variable_id) in question.variable_list.iter().enumerate() {
        new_key_store.insert(
            variable_id.to_string(),
            env.key_store
                .get_constraint(argument_variables[index].clone())
                .unwrap(),
        );
    }

    let mut environment = ExecuteEnvironment {
        behaviors: env.behaviors,
        key_store: Constraints {
            constraints: new_key_store,
        },
    };

    let solve_result = (question.solve)(&mut environment);
    if let Err(_) = solve_result {
        return Err(());
    } else {
        for (index, variable_id) in question.variable_list.iter().enumerate() {
            let tell_result = env.key_store.tell(
                argument_variables.get(index).unwrap().into(),
                &environment
                    .key_store
                    .get_constraint(variable_id.into())
                    .unwrap(),
            );
            if let ConstraintCheckResult::CONTRADICTION = tell_result {
                return Err(());
            }
        }
        return Ok(());
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::{
        call, Behavior, Constraint, ConstraintCheckResult, Constraints, ExecuteEnvironment,
    };

    #[test]
    fn it_works() {
        /*
        Behavior
        - A(O) {O="AAAAA"}
        Question
        - A(X)?
        - Q(X) {A(X)} とみなされる。
        */
        let mut behaviors: HashMap<String, Behavior> = HashMap::new();

        behaviors.insert(
            "A".into(),
            Behavior {
                variable_list: vec!["O".into()],
                solve: |env| {
                    if let ConstraintCheckResult::CONTRADICTION = env
                        .key_store
                        .tell("O".into(), &Constraint::String("AAAAA".into()))
                    {
                        Err(())
                    } else {
                        Ok(())
                    }
                },
            },
        );

        let question: Behavior = Behavior {
            solve: |env| {
                let a = env.behaviors.get("A").unwrap();
                let call_result = call(env, a, vec!["X".into()]);
                if let Ok(_) = call_result {
                    return Ok(());
                } else {
                    return Err(());
                }
            },
            variable_list: vec!["X".into()],
        };

        let mut env = ExecuteEnvironment {
            behaviors: &mut behaviors,
            key_store: Constraints {
                constraints: HashMap::from([("X".into(), Constraint::None)]),
            },
        };

        let call_result = call(&mut env, &question, vec!["X".into()]);

        if let Err(_) = call_result {
            assert!(false);
        }

        match env.key_store.get_constraint("X".into()).unwrap() {
            Constraint::String(s) => assert_eq!(s, "AAAAA"),
            _ => {
                assert!(false);
            }
        };
    }
}
