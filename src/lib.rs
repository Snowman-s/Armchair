pub mod expressions;
pub mod parser;

use expressions::expressions::Expressions;
use num_rational::Rational64;
use std::collections::HashMap;
use std::sync::{Arc, Condvar, Mutex};
use std::thread;

trait AskTerm: Send + Sync {
    fn prove(&self, constraints: &ExecuteEnvironment) -> ConstraintCheckResult;
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Constraint {
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

impl ToString for Atom {
    fn to_string(&self) -> String {
        match self {
            Atom::Atom(atom) => format!("\'{}\'", atom),
            Atom::Number(rational) => rational.to_string(),
        }
    }
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

    /** Wait until the variable's constraint is not None */
    fn wait_until_grounded(&self, variable_id: String) -> Constraint {
        let mut constraints_inner = self.constraints.lock().unwrap();
        loop {
            if let Some(Constraint::EqualTo(atom)) = constraints_inner.get(&variable_id) {
                return Constraint::EqualTo(atom.clone());
            }

            constraints_inner = self.condvar.wait(constraints_inner).unwrap();
        }
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

impl ExecuteEnvironment<'_> {
    pub fn new(
        behaviors: &HashMap<String, Behavior>,
        map: Arc<Mutex<HashMap<String, Constraint>>>,
    ) -> ExecuteEnvironment {
        ExecuteEnvironment {
            behaviors,
            key_store: Constraints {
                constraints: map,
                condvar: Condvar::new(),
            },
        }
    }
}

pub struct Behavior {
    param_list: Vec<BehaviorParam>,
    root: Box<dyn Agent>,
}

pub enum BehaviorParam {
    Asker(String),
    Teller(String),
}

pub trait Agent: Send + Sync {
    fn solve(&self, environment: &ExecuteEnvironment) -> Result<(), ()>;
}

fn call(
    env: &ExecuteEnvironment,
    question: &Behavior,
    argument_variables: &Vec<CallArgument>,
) -> Result<(), ()> {
    let mut new_key_store = HashMap::new();
    for (index, param) in question.param_list.iter().enumerate() {
        // If both of argument and param is Asker, argument must be grounded.
        if let BehaviorParam::Asker(param_id) = param {
            if let CallArgument::Asker(argument_id) = &argument_variables[index] {
                new_key_store.insert(
                    param_id.to_string(),
                    env.key_store.wait_until_grounded(argument_id.clone()),
                );
                continue;
            }
        }
        // If both of argument and param is Teller, argument don't have to be grounded.
        if let BehaviorParam::Teller(param_id) = param {
            if let CallArgument::Teller(argument_id) = &argument_variables[index] {
                new_key_store.insert(
                    param_id.to_string(),
                    env.key_store.get_constraint(argument_id.clone()),
                );
                continue;
            }
        }

        // If neither of those, we cannot proceed process.
        return Err(());
    }

    let new_key_store_arc = Arc::new(Mutex::new(new_key_store));

    let new_environment = ExecuteEnvironment {
        behaviors: env.behaviors,
        key_store: Constraints {
            constraints: new_key_store_arc,
            condvar: Condvar::new(),
        },
    };

    let solve_result = question.root.solve(&new_environment);
    if let Err(_) = solve_result {
        return Err(());
    } else {
        // Copy "lower" environments.
        for (index, param) in question.param_list.iter().enumerate() {
            if let BehaviorParam::Teller(param_id) = param {
                if let CallArgument::Teller(argument_id) = argument_variables.get(index).unwrap() {
                    let tell_result = env.key_store.tell(
                        argument_id.into(),
                        &(new_environment.key_store.get_constraint(param_id.into())),
                    );
                    if let ConstraintCheckResult::CONTRADICTION = tell_result {
                        return Err(());
                    }
                }
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
    argument_variable_list: Vec<CallArgument>,
}

enum CallArgument {
    Asker(String),
    Teller(String),
}

impl Agent for CallAgent {
    fn solve(&self, environment: &ExecuteEnvironment<'_>) -> Result<(), ()> {
        let a = environment.behaviors.get(&self.behavior_name).unwrap();
        let call_result = call(environment, a, &self.argument_variable_list);
        if let Ok(_) = call_result {
            return Ok(());
        } else {
            return Err(());
        }
    }
}

fn create_call_agent(
    behavior_name: String,
    argument_variable_list: Vec<CallArgument>,
) -> Box<dyn Agent> {
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
    ask_term: Box<dyn AskTerm>,
    then: Box<dyn Agent>,
}

impl Agent for AskAgent {
    fn solve(&self, environment: &ExecuteEnvironment) -> Result<(), ()> {
        let check_res = &self.ask_term.prove(&environment);

        if let ConstraintCheckResult::CONTRADICTION = check_res {
            Ok(())
        } else {
            self.then.solve(environment)
        }
    }
}

fn create_ask_agent(ask_term: Box<dyn AskTerm>, then: Box<dyn Agent>) -> Box<dyn Agent> {
    Box::new(AskAgent { ask_term, then })
}

struct AskTermAEqualB {
    left: Expressions,
    right: Expressions,
}

impl AskTerm for AskTermAEqualB {
    fn prove(&self, constraints: &ExecuteEnvironment) -> ConstraintCheckResult {
        if self.left.solve(constraints) == self.right.solve(constraints) {
            ConstraintCheckResult::SUCCEED(HashMap::new())
        } else {
            ConstraintCheckResult::CONTRADICTION
        }
    }
}

fn create_ask_term_a_equal_b(left: Expressions, right: Expressions) -> Box<dyn AskTerm> {
    Box::new(AskTermAEqualB { left, right })
}

#[cfg(test)]
mod tests {
    use std::{
        collections::HashMap,
        sync::{Arc, Condvar, Mutex},
    };

    use crate::{
        call, create_ask_agent, create_ask_term_a_equal_b, create_call_agent, create_linear_agent,
        create_tell_agent,
        expressions::expressions::{Expression, Expressions},
        parser::parser::{compile_one_behavior, ParseResult},
        Atom, Behavior, BehaviorParam, CallArgument, Constraint, Constraints, ExecuteEnvironment,
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
                param_list: vec![BehaviorParam::Teller("O".into())],
                root: create_tell_agent(
                    "O".into(),
                    Expressions {
                        exprs: vec![Expression::Atom(Atom::Atom("AAAAA".into()))],
                    },
                ),
            },
        );

        let question: Behavior = Behavior {
            param_list: vec![BehaviorParam::Teller("X".into())],
            root: create_call_agent("A".into(), vec![CallArgument::Teller("X".into())]),
        };

        let env = ExecuteEnvironment {
            behaviors: &behaviors,
            key_store: Constraints {
                constraints: Arc::new(Mutex::new(HashMap::from([("X".into(), Constraint::None)]))),
                condvar: Condvar::new(),
            },
        };

        let call_result = call(&env, &question, &vec![CallArgument::Teller("X".into())]);

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
                param_list: vec![BehaviorParam::Teller("O".into())],
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
                param_list: vec![BehaviorParam::Teller("O".into())],
                root: create_tell_agent(
                    "O".into(),
                    Expressions {
                        exprs: vec![Expression::Atom(Atom::Atom("BBBBB".into()))],
                    },
                ),
            },
        );

        let question: Behavior = Behavior {
            param_list: vec![
                BehaviorParam::Teller("X".into()),
                BehaviorParam::Teller("Y".into()),
            ],
            root: create_linear_agent(vec![
                create_call_agent("A".into(), vec![CallArgument::Teller("X".into())]),
                create_call_agent("B".into(), vec![CallArgument::Teller("Y".into())]),
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

        let call_result = call(
            &env,
            &question,
            &vec![
                CallArgument::Teller("X".into()),
                CallArgument::Teller("Y".into()),
            ],
        );

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
                    param_list: vec![BehaviorParam::Teller("O".into())],
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
                    param_list: vec![BehaviorParam::Teller("O".into())],
                    root: create_linear_agent(vec![
                        create_ask_agent(
                            create_ask_term_a_equal_b(
                                Expressions {
                                    exprs: vec![Expression::Variable("I".into())],
                                },
                                Expressions {
                                    exprs: vec![Expression::Atom(Atom::Atom("AAAAA".into()))],
                                },
                            ),
                            create_tell_agent(
                                "O".into(),
                                Expressions {
                                    exprs: vec![Expression::Atom(Atom::Atom("BBBBB".into()))],
                                },
                            ),
                        ),
                        create_call_agent("A".into(), vec![CallArgument::Teller("I".into())]),
                    ]),
                },
            ),
        ]);

        let question: Behavior = Behavior {
            param_list: vec![BehaviorParam::Teller("X".into())],
            root: create_call_agent("B".into(), vec![CallArgument::Teller("X".into())]),
        };

        let env = ExecuteEnvironment {
            behaviors: &behaviors,
            key_store: Constraints {
                constraints: Arc::new(Mutex::new(HashMap::from([("X".into(), Constraint::None)]))),
                condvar: Condvar::new(),
            },
        };

        let call_result = call(&env, &question, &vec![CallArgument::Teller("X".into())]);

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
                param_list: vec![BehaviorParam::Teller("O".into())],
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
            param_list: vec![BehaviorParam::Teller("X".into())],
            root: create_linear_agent(vec![create_call_agent(
                "A".into(),
                vec![CallArgument::Teller("X".into())],
            )]),
        };

        let env = ExecuteEnvironment {
            behaviors: &behaviors,
            key_store: Constraints {
                constraints: Arc::new(Mutex::new(HashMap::from([("X".into(), Constraint::None)]))),
                condvar: Condvar::new(),
            },
        };

        let call_result = call(&env, &question, &vec![CallArgument::Teller("X".into())]);

        if let Err(_) = call_result {
            assert!(false);
        }

        if let Constraint::EqualTo(Atom::Number(s)) = env.key_store.get_constraint("X".into()) {
            assert_eq!(s, 3.into());
        } else {
            assert!(false);
        };
    }

    #[test]
    fn simple_compile_test() {
        match compile_one_behavior("Agent(!A) :: A=atom.") {
            Ok((ParseResult::Behavior(name_and_behavior), _)) => {
                let behaviors: HashMap<String, Behavior> = HashMap::from([name_and_behavior]);

                let question: Behavior = Behavior {
                    param_list: vec![BehaviorParam::Teller("X".into())],
                    root: create_linear_agent(vec![create_call_agent(
                        "Agent".into(),
                        vec![CallArgument::Teller("X".into())],
                    )]),
                };

                let env = ExecuteEnvironment {
                    behaviors: &behaviors,
                    key_store: Constraints {
                        constraints: Arc::new(Mutex::new(HashMap::from([(
                            "X".into(),
                            Constraint::None,
                        )]))),
                        condvar: Condvar::new(),
                    },
                };

                let call_result = call(&env, &question, &vec![CallArgument::Teller("X".into())]);

                if let Err(_) = call_result {
                    assert!(false);
                }

                assert_eq!(
                    env.key_store.get_constraint("X".into()),
                    Constraint::EqualTo(Atom::Atom("atom".into()))
                );
            }
            _ => {
                assert!(false);
            }
        }
    }
    #[test]
    fn complex_compile_test() {
        match compile_one_behavior("Agent(!X) ::_variable=atom->X='Atom', _variable=A, A=atom.") {
            Ok((ParseResult::Behavior(name_and_behavior), _)) => {
                let behaviors: HashMap<String, Behavior> = HashMap::from([name_and_behavior]);

                let question: Behavior = Behavior {
                    param_list: vec![BehaviorParam::Teller("X".into())],
                    root: create_linear_agent(vec![create_call_agent(
                        "Agent".into(),
                        vec![CallArgument::Teller("X".into())],
                    )]),
                };

                let env = ExecuteEnvironment {
                    behaviors: &behaviors,
                    key_store: Constraints {
                        constraints: Arc::new(Mutex::new(HashMap::from([(
                            "X".into(),
                            Constraint::None,
                        )]))),
                        condvar: Condvar::new(),
                    },
                };

                let call_result = call(&env, &question, &vec![CallArgument::Teller("X".into())]);

                if let Err(_) = call_result {
                    assert!(false);
                }

                assert_eq!(
                    env.key_store.get_constraint("X".into()),
                    Constraint::EqualTo(Atom::Atom("Atom".into()))
                );
            }
            _ => {
                assert!(false);
            }
        }
    }

    #[test]
    fn question_test() {
        let mut code = "Human(I, !O) :: I=socrates -> O=y, I=knowledge -> O=n. ".to_string()
            + " Die(I, !O) :: Human(I, !O)."
            + " ? Die(I, !O), I=socrates."
            + " ? Die(I, !O), I=knowledge.";

        let mut behaviors = HashMap::new();

        match compile_one_behavior(&code) {
            Ok((ParseResult::Behavior((name, behavior)), remain_code)) => {
                behaviors.insert(name, behavior);
                code = remain_code.into();
            }
            _ => {
                assert!(false)
            }
        }
        match compile_one_behavior(&code) {
            Ok((ParseResult::Behavior((name, behavior)), remain_code)) => {
                behaviors.insert(name, behavior);
                code = remain_code.into();
            }
            _ => {
                assert!(false)
            }
        }
        match compile_one_behavior(&code) {
            Ok((ParseResult::Question(agent), remain)) => {
                code = remain.into();

                let constraints = Arc::new(Mutex::new(HashMap::new()));
                // 実行
                let environment = ExecuteEnvironment::new(&behaviors, constraints.clone());

                let res = agent.solve(&environment);

                match res {
                    Ok(_) => {
                        assert_eq!(
                            constraints.lock().unwrap().to_owned(),
                            HashMap::from([
                                (
                                    "I".into(),
                                    Constraint::EqualTo(Atom::Atom("socrates".into()))
                                ),
                                ("O".into(), Constraint::EqualTo(Atom::Atom("y".into())))
                            ])
                        )
                    }
                    Err(_) => assert!(false),
                }
            }
            _ => {
                assert!(false)
            }
        }
        match compile_one_behavior(&code) {
            Ok((ParseResult::Question(agent), remain)) => {
                assert!(remain.is_empty());

                let constraints = Arc::new(Mutex::new(HashMap::new()));
                // 実行
                let environment = ExecuteEnvironment::new(&behaviors, constraints.clone());

                let res = agent.solve(&environment);

                match res {
                    Ok(_) => {
                        assert_eq!(
                            constraints.lock().unwrap().to_owned(),
                            HashMap::from([
                                (
                                    "I".into(),
                                    Constraint::EqualTo(Atom::Atom("knowledge".into()))
                                ),
                                ("O".into(), Constraint::EqualTo(Atom::Atom("n".into())))
                            ])
                        )
                    }
                    Err(_) => assert!(false),
                }
            }
            _ => {
                assert!(false)
            }
        }
    }
}
