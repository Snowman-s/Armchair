pub mod expressions {
    use std::collections::VecDeque;

    use crate::{Atom, Constraint, ExecuteEnvironment};

    pub struct Expressions {
        pub exprs: Vec<Expression>,
    }

    impl Expressions {
        pub fn solve(&self, env: &ExecuteEnvironment) -> Atom {
            let mut stack = VecDeque::new();

            for expr in &self.exprs {
                expr.apply(env, &mut stack);
            }

            stack[0].clone()
        }
    }

    pub enum Expression {
        Atom(Atom),
        Variable(String),
    }

    impl Expression {
        fn apply(&self, env: &ExecuteEnvironment, stack: &mut VecDeque<Atom>) {
            match self {
                Self::Atom(atom) => stack.push_front(atom.clone()),
                Self::Variable(variable_id) => {
                    let mut constraints_inner = env.key_store.constraints.lock().unwrap();
                    loop {
                        if let Some(Constraint::EqualTo(atom)) =
                            constraints_inner.get(variable_id.into())
                        {
                            stack.push_front(atom.clone());
                            break;
                        }

                        constraints_inner = env.key_store.condvar.wait(constraints_inner).unwrap();
                    }
                }
            }
        }
    }
}
