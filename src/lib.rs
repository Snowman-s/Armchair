pub mod expressions;
pub mod parser;

use expressions::expressions::Expressions;
use num_rational::Rational64;
use std::collections::{HashMap, HashSet};
use std::sync::{Arc, Condvar, Mutex};
use std::thread;

trait AskTerm: Send + Sync {
    fn prove(&self, constraints: &ExecuteEnvironment) -> ConstraintCheckResult;

    fn variables(&self) -> HashSet<String>;
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

pub struct Constraints {
    /** 変数名:制約*/
    constraints: HashMap<String, VariableInfo>,
}

type VariableInfo = (
    // I assume variable rights cannot be changed.
    Arc<Mutex<VariableRight>>,
    Arc<(Mutex<Constraint>, Condvar)>,
);

/*
  Remaining variable rights.
*/
enum VariableRight {
    All,
    Askable,
    Tellable,
    None,
}

impl Constraints {
    pub fn new(variables: impl Iterator<Item = String>) -> Constraints {
        Constraints {
            constraints: variables
                .map(|variable_id| {
                    (
                        variable_id,
                        (
                            Arc::new(Mutex::new(VariableRight::All)),
                            Arc::new((Mutex::new(Constraint::None), Condvar::new())),
                        ),
                    )
                })
                .collect(),
        }
    }

    /**
     * this function blocks execution until conforming "guard",
     *
     * Both arguments' and params' length must be same.
     *
     * "initilize_variable" is used for "inner" variables.
     * If it's an element already in param, it is ignored.
     */
    fn copy_to_new_constraint(
        &self,
        arguments: &Vec<CallArgument>,
        param: &Vec<BehaviorParam>,
        initilize_variable_list: impl Iterator<Item = String>,
    ) -> Result<Constraints, ()> {
        let mut new_constraints = HashMap::new();

        for (index, argument) in arguments.iter().enumerate() {
            let new_variable = match argument {
                CallArgument::Asker(argument_variable) => {
                    self.get_ask_argument_rights(argument_variable)
                }
                CallArgument::Teller(argument_variable) => self.get_tell_rights(argument_variable),
            }?;
            new_constraints.insert(param[index].get_variable_name().clone(), new_variable);
        }

        for init in initilize_variable_list {
            if !new_constraints.contains_key(&init) {
                new_constraints.insert(
                    init.clone(),
                    (
                        Arc::new(Mutex::new(VariableRight::All)),
                        Arc::new((Mutex::new(Constraint::None), Condvar::new())),
                    ),
                );
            }
        }

        Ok(Constraints {
            constraints: new_constraints,
        })
    }

    fn tell(&self, variable_id: String, told: Constraint) -> Result<(), ()> {
        let (_, original_cons_arc) = self.get_tell_rights(&variable_id)?;

        let (original_cons, condvar) = &*original_cons_arc;
        *original_cons.lock().unwrap() = told;

        condvar.notify_all();

        Ok(())
    }

    /**
     * Err, if variable doesn't have ask-rights.
     */
    pub fn get_constraint(&self, variable_id: &String) -> Result<Constraint, ()> {
        let (_, cons_arc) = self.get_ask_rights(variable_id)?;

        let (cons, _) = &*cons_arc;

        let ret = cons.lock().unwrap().clone();

        Ok(ret)
    }

    /** Wait until the variable's constraint is not None.
     Err, if variable doesn't have ask-rights.
    */
    fn wait_until_grounded(&self, variable_id: &String) -> Result<Constraint, ()> {
        let (_, arc) = self.get_ask_rights(variable_id)?;

        let (arc_constraints, condvar) = &*arc;

        let mut the_constraint = arc_constraints.lock().unwrap();
        loop {
            if let Constraint::EqualTo(_) = *the_constraint {
                return Ok(the_constraint.clone());
            }

            the_constraint = condvar.wait(the_constraint).unwrap();
        }
    }

    /**  private function
      - \<UnInitilized> | AskRight -> Set Tell-Right to self. Return (Ask-Right, data)
      - Others -> Return Err.
    */
    fn get_ask_rights(&self, variable_id: &String) -> Result<VariableInfo, ()> {
        match self.constraints.get(variable_id) {
            Some((right, data)) => {
                let right_guard = right.lock().unwrap();
                match *right_guard {
                    VariableRight::All | VariableRight::Askable => {
                        return Ok((Arc::new(Mutex::new(VariableRight::Askable)), data.clone()));
                    }
                    _ => {
                        return Err(());
                    }
                };
            }
            None => Err(()),
        }
    }

    /**  private function
      - \<UnInitilized> | AskRight -> Return (Ask-Right, data)
      - TellRight -> Set None-Right to self. Return (Tell-Right, data).
      - NoneRight -> Err(())
    */
    fn get_ask_argument_rights(&self, variable_id: &String) -> Result<VariableInfo, ()> {
        match self.constraints.get(variable_id) {
            Some((right, data)) => {
                let mut right_guard = right.lock().unwrap();
                match *right_guard {
                    VariableRight::All | VariableRight::Askable => {
                        return Ok((Arc::new(Mutex::new(VariableRight::Askable)), data.clone()));
                    }
                    VariableRight::Tellable => {
                        *right_guard = VariableRight::None;
                        return Ok((Arc::new(Mutex::new(VariableRight::Tellable)), data.clone()));
                    }
                    VariableRight::None => return Err(()),
                };
            }
            None => Err(()),
        }
    }

    /**  private function
      - \<UnInitilized> -> Set Ask-Right to self. Return (Tell-Right, data)
      - TellRight -> Return (Tell-Right, data).
      - Others -> Return Err.
    */
    fn get_tell_rights(&self, variable_id: &String) -> Result<VariableInfo, ()> {
        match self.constraints.get(variable_id) {
            Some((right, data)) => {
                let mut right_guard = right.lock().unwrap();
                match *right_guard {
                    VariableRight::All => *right_guard = VariableRight::Askable,
                    VariableRight::Tellable => *right_guard = VariableRight::None,
                    _ => {
                        return Err(());
                    }
                };
                return Ok((Arc::new(Mutex::new(VariableRight::Tellable)), data.clone()));
            }
            None => Err(()),
        }
    }
}

enum ConstraintCheckResult {
    /**  constraints の一部を返す*/
    SUCCEED,
    CONTRADICTION,
}

pub struct ExecuteEnvironment<'a> {
    behaviors: &'a HashMap<String, Behavior>,
    key_store: Constraints,
}

impl ExecuteEnvironment<'_> {
    pub fn new(
        behaviors: &HashMap<String, Behavior>,
        top_variables: impl Iterator<Item = String>,
    ) -> ExecuteEnvironment {
        ExecuteEnvironment {
            behaviors,
            key_store: Constraints::new(top_variables),
        }
    }

    pub fn key_store(&self) -> &Constraints {
        &self.key_store
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

impl BehaviorParam {
    fn get_variable_name(&self) -> &String {
        match self {
            BehaviorParam::Asker(s) => s,
            BehaviorParam::Teller(s) => s,
        }
    }
}

pub trait Agent: Send + Sync {
    fn solve(&self, environment: &ExecuteEnvironment) -> Result<(), ()>;
    fn variable_list(&self) -> HashSet<String>;
}

fn call(
    env: &ExecuteEnvironment,
    question: &Behavior,
    argument_variables: &Vec<CallArgument>,
) -> Result<(), ()> {
    let new_environment = ExecuteEnvironment {
        behaviors: env.behaviors,
        key_store: env.key_store.copy_to_new_constraint(
            argument_variables,
            &question.param_list,
            question.root.variable_list().into_iter(),
        )?,
    };

    question.root.solve(&new_environment)?;

    Ok(())
}

struct TellAgent {
    variable_id: String,
    expression: Expressions,
}

impl Agent for TellAgent {
    fn solve(&self, environment: &ExecuteEnvironment<'_>) -> Result<(), ()> {
        let expr_solved = self.expression.solve(&environment)?;

        environment
            .key_store
            .tell(self.variable_id.clone(), Constraint::EqualTo(expr_solved))
    }

    fn variable_list(&self) -> HashSet<String> {
        let mut expr: HashSet<String> = self.expression.variables();
        expr.insert(self.variable_id.clone());
        expr
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

impl CallArgument {
    fn get_variable_name(&self) -> &String {
        match self {
            CallArgument::Asker(s) => s,
            CallArgument::Teller(s) => s,
        }
    }
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

    fn variable_list(&self) -> HashSet<String> {
        self.argument_variable_list
            .iter()
            .map(|arg| arg.get_variable_name().clone())
            .collect()
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

    fn variable_list(&self) -> HashSet<String> {
        self.children
            .iter()
            .flat_map(|child| child.variable_list())
            .collect()
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

    fn variable_list(&self) -> HashSet<String> {
        let mut ret = self.ask_term.variables();

        ret.extend(self.then.variable_list());

        ret
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
            ConstraintCheckResult::SUCCEED
        } else {
            ConstraintCheckResult::CONTRADICTION
        }
    }

    fn variables(&self) -> HashSet<String> {
        let mut ret = self.left.variables();
        ret.extend(self.right.variables());

        ret
    }
}

fn create_ask_term_a_equal_b(left: Expressions, right: Expressions) -> Box<dyn AskTerm> {
    Box::new(AskTermAEqualB { left, right })
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::{
        call, create_ask_agent, create_ask_term_a_equal_b, create_call_agent, create_linear_agent,
        create_tell_agent,
        expressions::expressions::{Expression, Expressions},
        parser::parser::{compile_one_behavior, ParseResult},
        Atom, Behavior, BehaviorParam, CallArgument, Constraint, ExecuteEnvironment,
    };

    #[test]
    fn simple_constraint_system() {
        /*
        Behavior
        - A(!O) {O="AAAAA"}
        Question
        - ?A(!X)
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

        let question = create_call_agent("A".into(), vec![CallArgument::Teller("X".into())]);

        let env = ExecuteEnvironment::new(&behaviors, question.variable_list().into_iter());

        if let Err(_) = question.solve(&env) {
            assert!(false);
        }

        if let Result::Ok(Constraint::EqualTo(Atom::Atom(s))) =
            env.key_store.get_constraint(&"X".into())
        {
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
        - ?A(!X), B(!Y)
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

        let question = create_linear_agent(vec![
            create_call_agent("A".into(), vec![CallArgument::Teller("X".into())]),
            create_call_agent("B".into(), vec![CallArgument::Teller("Y".into())]),
        ]);

        let env = ExecuteEnvironment::new(&behaviors, question.variable_list().into_iter());

        let call_result = question.solve(&env);

        if let Err(_) = call_result {
            assert!(false);
        }

        if let Ok(Constraint::EqualTo(Atom::Atom(s))) = env.key_store.get_constraint(&"X".into()) {
            assert_eq!(s, "AAAAA");
        } else {
            assert!(false);
        };

        if let Ok(Constraint::EqualTo(Atom::Atom(s))) = env.key_store.get_constraint(&"Y".into()) {
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
        - ?B(!X)
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

        let question = create_call_agent("B".into(), vec![CallArgument::Teller("X".into())]);

        let env = ExecuteEnvironment::new(&behaviors, question.variable_list().into_iter());

        let call_result = question.solve(&env);

        if let Err(_) = call_result {
            assert!(false);
        }

        if let Ok(Constraint::EqualTo(Atom::Atom(s))) = env.key_store.get_constraint(&"X".into()) {
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
        - ?A(!X)
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

        let question = create_linear_agent(vec![create_call_agent(
            "A".into(),
            vec![CallArgument::Teller("X".into())],
        )]);

        let env = ExecuteEnvironment::new(&behaviors, question.variable_list().into_iter());

        let call_result = question.solve(&env);

        if let Err(_) = call_result {
            assert!(false);
        }

        if let Ok(Constraint::EqualTo(Atom::Number(s))) = env.key_store.get_constraint(&"X".into())
        {
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

                let env =
                    ExecuteEnvironment::new(&behaviors, question.root.variable_list().into_iter());

                let call_result = call(&env, &question, &vec![CallArgument::Teller("X".into())]);

                if let Err(_) = call_result {
                    assert!(false);
                }

                if let Ok(Constraint::EqualTo(Atom::Atom(s))) =
                    env.key_store.get_constraint(&"X".into())
                {
                    assert_eq!(s, "atom");
                } else {
                    assert!(false);
                };
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

                let env =
                    ExecuteEnvironment::new(&behaviors, question.root.variable_list().into_iter());

                let call_result = call(&env, &question, &vec![CallArgument::Teller("X".into())]);

                if let Err(_) = call_result {
                    assert!(false);
                }

                if let Ok(Constraint::EqualTo(Atom::Atom(s))) =
                    env.key_store.get_constraint(&"X".into())
                {
                    assert_eq!(s, "Atom");
                } else {
                    assert!(false);
                };
            }
            _ => {
                assert!(false);
            }
        }
    }

    #[test]
    fn question_test() {
        let mut code = "Human(I, !O) :: I=socrates -> O=y, I=knowledge -> O=n. ".to_string()
            + " Die(I, O) :: Human(I, O)."
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

                // 実行
                let environment =
                    ExecuteEnvironment::new(&behaviors, agent.variable_list().into_iter());

                let res = agent.solve(&environment);

                match res {
                    Ok(_) => {
                        if let Ok(Constraint::EqualTo(Atom::Atom(s))) =
                            environment.key_store.get_constraint(&"I".into())
                        {
                            assert_eq!(s, "socrates");
                        } else {
                            assert!(false);
                        };

                        if let Ok(Constraint::EqualTo(Atom::Atom(s))) =
                            environment.key_store.get_constraint(&"O".into())
                        {
                            assert_eq!(s, "y");
                        } else {
                            assert!(false);
                        };
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

                // 実行
                let environment =
                    ExecuteEnvironment::new(&behaviors, agent.variable_list().into_iter());

                let res = agent.solve(&environment);

                match res {
                    Ok(_) => {
                        if let Ok(Constraint::EqualTo(Atom::Atom(s))) =
                            environment.key_store.get_constraint(&"I".into())
                        {
                            assert_eq!(s, "knowledge");
                        } else {
                            assert!(false);
                        };

                        if let Ok(Constraint::EqualTo(Atom::Atom(s))) =
                            environment.key_store.get_constraint(&"O".into())
                        {
                            assert_eq!(s, "n");
                        } else {
                            assert!(false);
                        };
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
