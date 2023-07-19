use futures::executor::block_on;
use futures::future::join_all;
use futures::Future;
use num_rational::Rational64;
use std::collections::HashMap;
use std::pin::Pin;
use std::sync::{Arc, Mutex};

struct AskTerm {
    prove: fn(constraints: &HashMap<String, Constraint>) -> ConstraintCheckResult,
}

#[derive(PartialEq, Eq, Clone, Debug)]
enum Constraint {
    None,
    EqualTo(Atom),
}

#[derive(PartialEq, Eq, Clone, Debug)]
enum Atom {
    /** 真なるアトム (Stringは名前としての存在であってコードからは参照すべきではない) */
    Atom(String),
    /** 数値としてのアトム */
    Number(Rational64),
}

#[derive(Clone)]
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

#[derive(Clone)]
struct ExecuteEnvironment<'a> {
    behaviors: &'a HashMap<String, Behavior>,
    key_store: Constraints,
}

struct Behavior {
    argument_list: Vec<String>,
    root: Box<dyn Agent>,
}

trait Agent {
    fn solve(&self, environment: Arc<Mutex<ExecuteEnvironment>>) -> Result<(), ()>;
}

fn call(
    env: Arc<Mutex<ExecuteEnvironment>>,
    question: &Behavior,
    argument_variables: Vec<String>,
) -> Result<(), ()> {
    let mut new_key_store = HashMap::new();
    for (index, variable_id) in question.argument_list.iter().enumerate() {
        new_key_store.insert(
            variable_id.to_string(),
            env.lock()
                .unwrap()
                .key_store
                .get_constraint(argument_variables[index].clone())
                .unwrap(),
        );
    }

    let environment = Arc::new(Mutex::new(ExecuteEnvironment {
        behaviors: env.lock().unwrap().behaviors,
        key_store: Constraints {
            constraints: new_key_store,
        },
    }));

    let solve_result = question.root.solve(Arc::clone(&environment));
    if let Err(_) = solve_result {
        return Err(());
    } else {
        for (index, variable_id) in question.argument_list.iter().enumerate() {
            let tell_result = env.lock().unwrap().key_store.tell(
                argument_variables.get(index).unwrap().into(),
                &(environment
                    .lock()
                    .unwrap()
                    .key_store
                    .get_constraint(variable_id.into()))
                .unwrap(),
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
    atom: Atom,
}

impl Agent for TellAgent {
    fn solve(&self, environment: Arc<std::sync::Mutex<ExecuteEnvironment<'_>>>) -> Result<(), ()> {
        if let ConstraintCheckResult::CONTRADICTION = environment.lock().unwrap().key_store.tell(
            self.variable_id.clone(),
            &Constraint::EqualTo(self.atom.clone()),
        ) {
            Err(())
        } else {
            Ok(())
        }
    }
}

fn create_tell_agent(variable_id: String, atom: Atom) -> Box<dyn Agent> {
    Box::new(TellAgent { variable_id, atom })
}

struct CallAgent {
    behavior_name: String,
    argument_variable_list: Vec<String>,
}

impl Agent for CallAgent {
    fn solve(&self, environment: Arc<std::sync::Mutex<ExecuteEnvironment<'_>>>) -> Result<(), ()> {
        let a = environment
            .lock()
            .unwrap()
            .behaviors
            .get(&self.behavior_name)
            .unwrap();
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
    fn solve(&self, environment: Arc<std::sync::Mutex<ExecuteEnvironment<'_>>>) -> Result<(), ()> {
        let mut threads: Vec<Pin<Box<dyn Future<Output = Result<(), ()>>>>> = vec![];
        for element in self.children.as_slice() {
            let new_pointer = Arc::clone(&environment);
            threads.push(Box::pin(async move { element.solve(new_pointer) }));
        }

        for result in block_on(join_all(threads)) {
            if let Err(()) = result {
                return Err(());
            }
        }

        return Ok(());
    }
}

fn create_linear_agent(children: Vec<Box<dyn Agent>>) -> Box<dyn Agent> {
    Box::new(LinearAgent { children })
}

#[cfg(test)]
mod tests {
    use std::{
        collections::HashMap,
        sync::{Arc, Mutex},
    };

    use crate::{
        call, create_call_agent, create_linear_agent, create_tell_agent, Atom, Behavior,
        Constraint, Constraints, ExecuteEnvironment,
    };

    #[test]
    fn simple_constraint_system() {
        /*
        Behavior
        - A(!O) {O="AAAAA"}
        Question
        - ask A(!X)?
        - Q(!Ans) {A(!X), Ans=X} とみなされる。少し実際のコードでは省略している。
        */
        let mut behaviors: HashMap<String, Behavior> = HashMap::new();

        behaviors.insert(
            "A".into(),
            Behavior {
                argument_list: vec!["O".into()],
                root: create_tell_agent("O".into(), Atom::Atom("AAAAA".into()).into()),
            },
        );

        let question: Behavior = Behavior {
            argument_list: vec!["X".into()],
            root: create_call_agent("A".into(), vec!["X".into()]),
        };

        let env = Arc::new(Mutex::new(ExecuteEnvironment {
            behaviors: &behaviors,
            key_store: Constraints {
                constraints: HashMap::from([("X".into(), Constraint::None)]),
            },
        }));

        let call_result = call(Arc::clone(&env), &question, vec!["X".into()]);

        if let Err(_) = call_result {
            assert!(false);
        }

        if let Constraint::EqualTo(Atom::Atom(s)) = env
            .lock()
            .unwrap()
            .key_store
            .get_constraint("X".into())
            .unwrap()
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
        - ask A(!X), B(!Y)?
        - Q(!Ans, !Ans2) {A(!X), Ans=X, B(!Y), Ans2=Y} とみなされる。少し実際のコードでは省略している。
        */

        let mut behaviors: HashMap<String, Behavior> = HashMap::new();

        behaviors.insert(
            "A".into(),
            Behavior {
                argument_list: vec!["O".into()],
                root: create_tell_agent("O".into(), Atom::Atom("AAAAA".into()).into()),
            },
        );
        behaviors.insert(
            "B".into(),
            Behavior {
                argument_list: vec!["O".into()],
                root: create_tell_agent("O".into(), Atom::Atom("BBBBB".into()).into()),
            },
        );

        let question: Behavior = Behavior {
            argument_list: vec!["X".into(), "Y".into()],
            root: create_linear_agent(vec![
                create_call_agent("A".into(), vec!["X".into()]),
                create_call_agent("B".into(), vec!["Y".into()]),
            ]),
        };

        let env = Arc::new(Mutex::new(ExecuteEnvironment {
            behaviors: &behaviors,
            key_store: Constraints {
                constraints: HashMap::from([
                    ("X".into(), Constraint::None),
                    ("Y".into(), Constraint::None),
                ]),
            },
        }));

        let call_result = call(Arc::clone(&env), &question, vec!["X".into(), "Y".into()]);

        if let Err(_) = call_result {
            assert!(false);
        }

        if let Constraint::EqualTo(Atom::Atom(s)) = env
            .lock()
            .unwrap()
            .key_store
            .get_constraint("X".into())
            .unwrap()
        {
            assert_eq!(s, "AAAAA");
        } else {
            assert!(false);
        };

        if let Constraint::EqualTo(Atom::Atom(s)) = env
            .lock()
            .unwrap()
            .key_store
            .get_constraint("Y".into())
            .unwrap()
        {
            assert_eq!(s, "BBBBB");
        } else {
            assert!(false);
        };
    }
}
