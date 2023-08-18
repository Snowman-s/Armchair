pub mod expressions {
    use std::collections::{HashSet, VecDeque};

    use crate::{Atom, Constraint, ExecuteEnvironment};

    pub struct Expressions {
        pub exprs: Vec<Expression>,
    }

    impl Expressions {
        pub fn solve(&self, env: &ExecuteEnvironment) -> Result<Atom, ()> {
            let mut stack = VecDeque::new();

            for expr in &self.exprs {
                expr.apply(env, &mut stack);
            }

            stack.get(0).ok_or_else(|| ()).cloned()
        }

        pub fn variables(&self) -> HashSet<String> {
            self.exprs
                .iter()
                .filter_map(|expr| match expr {
                    Expression::Atom(_) => None,
                    Expression::Variable(v) => Some(v.clone()),
                })
                .collect()
        }
    }

    pub enum Expression {
        Atom(Atom),
        Variable(String),
    }

    impl Expression {
        fn apply(&self, env: &ExecuteEnvironment, stack: &mut VecDeque<Atom>) -> Result<(), ()> {
            match self {
                Self::Atom(atom) => stack.push_front(atom.clone()),
                Self::Variable(variable_id) => {
                    stack.push_front(match env.key_store.wait_until_grounded(variable_id)? {
                        Constraint::EqualTo(atom) => atom,
                        _ => panic!(),
                    });
                }
            };

            return Ok(());
        }
    }
}
