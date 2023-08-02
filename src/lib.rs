mod expressions;
mod parser;

use expressions::expressions::Expressions;
use num_rational::Rational64;
use std::collections::HashMap;
use std::sync::{Arc, Condvar, Mutex};
use std::thread;

struct AskTerm {
    prove: fn(constraints: &ExecuteEnvironment) -> ConstraintCheckResult,
}

#[derive(PartialEq, Eq, Clone, Debug)]
enum Constraint {
    None,
    EqualTo(Atom),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Atom {
    /** 真なるアトム (Stringは名前としての存在であってコードからは参照すべきではない) */
    Atom(String),
    /** 数値としてのアトム */
    Number(Rational64),
}

struct Constraints {
    /** 変数名:制約*/
    constraints: Arc<Mutex<HashMap<String, Constraint>>>,
    condvar: Condvar,
}

impl Constraints {
    fn tell(&self, variable_id: String, c: &Constraint) -> ConstraintCheckResult {
        if let Some(s) = self.constraints.lock().unwrap().get(&variable_id) {
            if s != &Constraint::None && s != c {
                return ConstraintCheckResult::CONTRADICTION;
            }
        }

        self.constraints
            .lock()
            .unwrap()
            .insert(variable_id.clone(), c.clone());
        let mut ret_map: HashMap<String, Constraint> = HashMap::new();
        ret_map.insert(variable_id, c.clone());

        self.condvar.notify_all();

        ConstraintCheckResult::SUCCEED(ret_map)
    }

    fn get_constraint(&self, variable_id: String) -> Constraint {
        self.constraints
            .lock()
            .unwrap()
            .get(&variable_id)
            .unwrap_or(&Constraint::None)
            .clone()
    }
}

enum ConstraintCheckResult {
    /**  constraints の一部を返す*/
    SUCCEED(HashMap<String, Constraint>),
    CONTRADICTION,
}

pub struct ExecuteEnvironment<'a> {
    behaviors: &'a HashMap<String, Behavior>,
    key_store: Constraints,
}

struct Behavior {
    argument_list: Vec<String>,
    root: Box<dyn Agent>,
}

trait Agent: Send + Sync {
    fn solve(&self, environment: &ExecuteEnvironment) -> Result<(), ()>;
}

fn call(
    env: &ExecuteEnvironment,
    question: &Behavior,
    argument_variables: Vec<String>,
) -> Result<(), ()> {
    let mut new_key_store = HashMap::new();
    for (index, variable_id) in question.argument_list.iter().enumerate() {
        new_key_store.insert(
            variable_id.to_string(),
            env.key_store
                .get_constraint(argument_variables[index].clone()),
        );
    }

    let new_key_store_arc = Arc::new(Mutex::new(new_key_store));

    let environment = ExecuteEnvironment {
        behaviors: env.behaviors,
        key_store: Constraints {
            constraints: new_key_store_arc,
            condvar: Condvar::new(),
        },
    };

    let solve_result = question.root.solve(&environment);
    if let Err(_) = solve_result {
        return Err(());
    } else {
        for (index, variable_id) in question.argument_list.iter().enumerate() {
            let tell_result = env.key_store.tell(
                argument_variables.get(index).unwrap().into(),
                &(environment.key_store.get_constraint(variable_id.into())),
            );
            if let ConstraintCheckResult::CONTRADICTION = tell_result {
                return Err(());
            }
        }
        return Ok(());
    }
}

struct TellAgent {
    variable_id: String,
    expression: Expressions,
}

impl Agent for TellAgent {
    fn solve(&self, environment: &ExecuteEnvironment<'_>) -> Result<(), ()> {
        let expr_solved = self.expression.solve(&environment);

        if let ConstraintCheckResult::CONTRADICTION = environment
            .key_store
            .tell(self.variable_id.clone(), &Constraint::EqualTo(expr_solved))
        {
            Err(())
        } else {
            Ok(())
        }
    }
}

fn create_tell_agent(variable_id: String, expr: Expressions) -> Box<dyn Agent> {
    Box::new(TellAgent {
        variable_id,
        expression: expr,
    })
}

struct CallAgent {
    behavior_name: String,
    argument_variable_list: Vec<String>,
}

impl Agent for CallAgent {
    fn solve(&self, environment: &ExecuteEnvironment<'_>) -> Result<(), ()> {
        let a = environment.behaviors.get(&self.behavior_name).unwrap();
        let call_result = call(environment, a, self.argument_variable_list.clone());
        if let Ok(_) = call_result {
            return Ok(());
        } else {
            return Err(());
        }
    }
}

fn create_call_agent(behavior_name: String, argument_variable_list: Vec<String>) -> Box<dyn Agent> {
    Box::new(CallAgent {
        behavior_name,
        argument_variable_list,
    })
}

struct LinearAgent {
    children: Vec<Box<dyn Agent>>,
}

impl Agent for LinearAgent {
    fn solve(&self, environment: &ExecuteEnvironment<'_>) -> Result<(), ()> {
        let mut result = Result::Ok(());
        thread::scope(|scope| {
            let mut threads = vec![];
            for element in &self.children {
                threads.push(
                    thread::Builder::new()
                        .spawn_scoped(scope, move || element.solve(environment))
                        .unwrap(),
                );
            }

            for thread in threads {
                let ret = thread.join().unwrap();
                if let Err(()) = ret {
                    result = Err(());
                }
            }
        });

        return result;
    }
}

fn create_linear_agent(children: Vec<Box<dyn Agent>>) -> Box<dyn Agent> {
    Box::new(LinearAgent { children })
}

struct AskAgent {
    ask_term: AskTerm,
    then: Box<dyn Agent>,
}

impl Agent for AskAgent {
    fn solve(&self, environment: &ExecuteEnvironment) -> Result<(), ()> {
        let check_res = &(self.ask_term.prove)(&environment);

        if let ConstraintCheckResult::CONTRADICTION = check_res {
            Err(())
        } else {
            self.then.solve(environment)
        }
    }
}

fn create_ask_agent(ask_term: AskTerm, then: Box<dyn Agent>) -> Box<dyn Agent> {
    Box::new(AskAgent { ask_term, then })
}

#[cfg(test)]
mod tests {
    use std::{
        collections::HashMap,
        sync::{Arc, Condvar, Mutex},
    };

    use crate::{
        call, create_ask_agent, create_call_agent, create_linear_agent, create_tell_agent,
        expressions::expressions::{Expression, Expressions},
        AskTerm, Atom, Behavior, Constraint, ConstraintCheckResult, Constraints,
        ExecuteEnvironment,
    };

    #[test]
    fn simple_constraint_system() {
        /*
        Behavior
        - A(!O) {O="AAAAA"}
        Question
        - ask A(!X)?
        - Q(!X) {A(!X)} とみなされる。
        */
        let mut behaviors: HashMap<String, Behavior> = HashMap::new();

        behaviors.insert(
            "A".into(),
            Behavior {
                argument_list: vec!["O".into()],
                root: create_tell_agent(
                    "O".into(),
                    Expressions {
                        exprs: vec![Expression::Atom(Atom::Atom("AAAAA".into()))],
                    },
                ),
            },
        );

        let question: Behavior = Behavior {
            argument_list: vec!["X".into()],
            root: create_call_agent("A".into(), vec!["X".into()]),
        };

        let env = ExecuteEnvironment {
            behaviors: &behaviors,
            key_store: Constraints {
                constraints: Arc::new(Mutex::new(HashMap::from([("X".into(), Constraint::None)]))),
                condvar: Condvar::new(),
            },
        };

        let call_result = call(&env, &question, vec!["X".into()]);

        if let Err(_) = call_result {
            assert!(false);
        }

        if let Constraint::EqualTo(Atom::Atom(s)) = env.key_store.get_constraint("X".into()) {
            assert_eq!(s, "AAAAA");
        } else {
            assert!(false);
        };
    }

    #[test]
    fn parallel_constraint_system() {
        /*
        Behavior
        - A(!O) {O="AAAAA"}
        - B(!O) {O="BBBBB"}
        Question
        - ask A(!X), B(!Y)?
        - Q(!X, !Y) {A(!X), B(!Y)} とみなされる。
        */

        let mut behaviors: HashMap<String, Behavior> = HashMap::new();

        behaviors.insert(
            "A".into(),
            Behavior {
                argument_list: vec!["O".into()],
                root: create_tell_agent(
                    "O".into(),
                    Expressions {
                        exprs: vec![Expression::Atom(Atom::Atom("AAAAA".into()))],
                    },
                ),
            },
        );
        behaviors.insert(
            "B".into(),
            Behavior {
                argument_list: vec!["O".into()],
                root: create_tell_agent(
                    "O".into(),
                    Expressions {
                        exprs: vec![Expression::Atom(Atom::Atom("BBBBB".into()))],
                    },
                ),
            },
        );

        let question: Behavior = Behavior {
            argument_list: vec!["X".into(), "Y".into()],
            root: create_linear_agent(vec![
                create_call_agent("A".into(), vec!["X".into()]),
                create_call_agent("B".into(), vec!["Y".into()]),
            ]),
        };

        let env = ExecuteEnvironment {
            behaviors: &behaviors,
            key_store: Constraints {
                constraints: Arc::new(Mutex::new(HashMap::from([
                    ("X".into(), Constraint::None),
                    ("Y".into(), Constraint::None),
                ]))),
                condvar: Condvar::new(),
            },
        };

        let call_result = call(&env, &question, vec!["X".into(), "Y".into()]);

        if let Err(_) = call_result {
            assert!(false);
        }

        if let Constraint::EqualTo(Atom::Atom(s)) = env.key_store.get_constraint("X".into()) {
            assert_eq!(s, "AAAAA");
        } else {
            assert!(false);
        };

        if let Constraint::EqualTo(Atom::Atom(s)) = env.key_store.get_constraint("Y".into()) {
            assert_eq!(s, "BBBBB");
        } else {
            assert!(false);
        };
    }
    #[test]
    fn ask_constraint_system() {
        /*
        Behavior
        - A(!O) {O="AAAAA"}
        - B(!O) {I="AAAAA" -> O="BBBBB", A(!I)}
        Question
        - ask B(!X)?
        - Q(!X) {B(!X)} とみなされる。
        */
        let behaviors: HashMap<String, Behavior> = HashMap::from([
            (
                "A".into(),
                Behavior {
                    argument_list: vec!["O".into()],
                    root: create_tell_agent(
                        "O".into(),
                        Expressions {
                            exprs: vec![Expression::Atom(Atom::Atom("AAAAA".into()))],
                        },
                    ),
                },
            ),
            (
                "B".into(),
                Behavior {
                    argument_list: vec!["O".into()],
                    root: create_linear_agent(vec![
                        create_ask_agent(
                            AskTerm {
                                prove: |constraints| {
                                    let mut constraints_inner =
                                        constraints.key_store.constraints.lock().unwrap();
                                    loop {
                                        if let Some(Constraint::EqualTo(atom)) =
                                            constraints_inner.get("I".into())
                                        {
                                            if *atom == Atom::Atom("AAAAA".into()) {
                                                return ConstraintCheckResult::SUCCEED(
                                                    HashMap::new(),
                                                );
                                            } else {
                                                return ConstraintCheckResult::CONTRADICTION;
                                            }
                                        }

                                        constraints_inner = constraints
                                            .key_store
                                            .condvar
                                            .wait(constraints_inner)
                                            .unwrap();
                                    }
                                },
                            },
                            create_tell_agent(
                                "O".into(),
                                Expressions {
                                    exprs: vec![Expression::Atom(Atom::Atom("BBBBB".into()))],
                                },
                            ),
                        ),
                        create_call_agent("A".into(), vec!["I".into()]),
                    ]),
                },
            ),
        ]);

        let question: Behavior = Behavior {
            argument_list: vec!["X".into()],
            root: create_call_agent("B".into(), vec!["X".into()]),
        };

        let env = ExecuteEnvironment {
            behaviors: &behaviors,
            key_store: Constraints {
                constraints: Arc::new(Mutex::new(HashMap::from([("X".into(), Constraint::None)]))),
                condvar: Condvar::new(),
            },
        };

        let call_result = call(&env, &question, vec!["X".into()]);

        if let Err(_) = call_result {
            assert!(false);
        }

        if let Constraint::EqualTo(Atom::Atom(s)) = env.key_store.get_constraint("X".into()) {
            assert_eq!(s, "BBBBB");
        } else {
            assert!(false);
        };
    }

    #[test]
    fn propagation() {
        /*
        Behavior
        - A(!O) {O=X, X=3}
        Question
        - ask A(!X)?
        - Q(!X) {A(!X)} とみなされる。
        */

        let mut behaviors: HashMap<String, Behavior> = HashMap::new();

        behaviors.insert(
            "A".into(),
            Behavior {
                argument_list: vec!["O".into()],
                root: create_linear_agent(vec![
                    create_tell_agent(
                        "O".into(),
                        Expressions {
                            exprs: vec![Expression::Variable("X".into())],
                        },
                    ),
                    create_tell_agent(
                        "X".into(),
                        Expressions {
                            exprs: vec![Expression::Atom(Atom::Number(3.into()))],
                        },
                    ),
                ]),
            },
        );

        let question: Behavior = Behavior {
            argument_list: vec!["X".into()],
            root: create_linear_agent(vec![create_call_agent("A".into(), vec!["X".into()])]),
        };

        let env = ExecuteEnvironment {
            behaviors: &behaviors,
            key_store: Constraints {
                constraints: Arc::new(Mutex::new(HashMap::from([("X".into(), Constraint::None)]))),
                condvar: Condvar::new(),
            },
        };

        let call_result = call(&env, &question, vec!["X".into()]);

        if let Err(_) = call_result {
            assert!(false);
        }

        if let Constraint::EqualTo(Atom::Number(s)) = env.key_store.get_constraint("X".into()) {
            assert_eq!(s, 3.into());
        } else {
            assert!(false);
        };
    }
}
