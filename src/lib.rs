use num_rational::Rational64;
use std::collections::HashMap;

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
    argument_list: Vec<String>,
    root: Box<dyn Agent>,
}

trait Agent {
    fn solve(&self, environment: &mut ExecuteEnvironment) -> Result<(), ()>;
}

fn call(
    env: &mut ExecuteEnvironment,
    question: &Behavior,
    argument_variables: Vec<String>,
) -> Result<(), ()> {
    let mut new_key_store = HashMap::new();
    for (index, variable_id) in question.argument_list.iter().enumerate() {
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

    let solve_result = question.root.solve(&mut environment);
    if let Err(_) = solve_result {
        return Err(());
    } else {
        for (index, variable_id) in question.argument_list.iter().enumerate() {
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

struct TellAgent {
    variable_id: String,
    atom: Atom,
}

impl Agent for TellAgent {
    fn solve(&self, environment: &mut ExecuteEnvironment) -> Result<(), ()> {
        if let ConstraintCheckResult::CONTRADICTION = environment.key_store.tell(
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
    fn solve(&self, environment: &mut ExecuteEnvironment) -> Result<(), ()> {
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

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::{
        call, create_call_agent, create_tell_agent, Atom, Behavior, Constraint,
        ConstraintCheckResult, Constraints, ExecuteEnvironment,
    };

    #[test]
    fn simple_constraint_system() {
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
                argument_list: vec!["O".into()],
                root: create_tell_agent("O".into(), Atom::Atom("AAAAA".into()).into()),
            },
        );

        let question: Behavior = Behavior {
            argument_list: vec!["X".into()],
            root: create_call_agent("A".into(), vec!["X".into()]),
        };

        let mut env = ExecuteEnvironment {
            behaviors: &behaviors,
            key_store: Constraints {
                constraints: HashMap::from([("X".into(), Constraint::None)]),
            },
        };

        let call_result = call(&mut env, &question, vec!["X".into()]);

        if let Err(_) = call_result {
            assert!(false);
        }

        if let Constraint::EqualTo(Atom::Atom(s)) =
            env.key_store.get_constraint("X".into()).unwrap()
        {
            assert_eq!(s, "AAAAA");
        } else {
            assert!(false);
        };
    }
}
