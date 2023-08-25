pub mod expressions {
    use std::collections::{HashSet, VecDeque};

    use num_rational::Rational64;

    use crate::{Atom, CompoundArg, Constraint, ExecuteEnvironment};

    pub struct Expressions {
        pub exprs: Vec<Expression>,
    }

    impl Expressions {
        pub fn solve(&self, env: &ExecuteEnvironment) -> Result<Atom, ()> {
            let mut stack = VecDeque::new();

            for expr in &self.exprs {
                expr.apply(env, &mut stack)?;
            }

            stack.get(0).ok_or_else(|| ()).cloned()
        }

        pub fn variables(&self) -> HashSet<String> {
            self.exprs
                .iter()
                .flat_map(|expr| match expr {
                    Expression::Variable(v) => vec![v.clone()],
                    Expression::Compound(_, args) => args
                        .iter()
                        .filter_map(|s| match s {
                            ExpressionCompoundArg::Expression => None,
                            ExpressionCompoundArg::Variable(v) => Some(v.clone()),
                        })
                        .collect(),
                    _ => vec![],
                })
                .collect()
        }
    }

    pub enum Expression {
        Atom(Atom),
        Variable(String),
        Compound(String, Vec<ExpressionCompoundArg>),
        TwoNumberCalc(TwoNumberCalcType),
    }

    pub enum ExpressionCompoundArg {
        Expression,
        Variable(String),
    }

    impl Expression {
        fn apply(&self, env: &ExecuteEnvironment, stack: &mut VecDeque<Atom>) -> Result<(), ()> {
            match self {
                Expression::Atom(atom) => stack.push_front(atom.clone()),
                Expression::Variable(variable_id) => {
                    stack.push_front(match env.key_store.wait_until_grounded(variable_id)? {
                        Constraint::EqualTo(atom) => atom,
                        _ => panic!(),
                    });
                }
                Expression::TwoNumberCalc(calc_type) => {
                    self.two_number_calc(stack, calc_type.get_func())?
                }
                Expression::Compound(name, args) => {
                    let mut comp_arg: Vec<CompoundArg> = vec![];

                    for exp_comp_arg in args {
                        match exp_comp_arg {
                            ExpressionCompoundArg::Expression => {
                                let pop = stack.pop_front().ok_or(())?;
                                comp_arg.push(CompoundArg::Atom(pop));
                            }
                            ExpressionCompoundArg::Variable(variable) => {
                                let v_info = env.key_store.constraints.get(variable).ok_or(())?;

                                comp_arg.push(CompoundArg::Variable(v_info.clone()));
                            }
                        }
                    }

                    stack.push_front(Atom::Compound(name.to_string(), comp_arg));
                }
            };

            return Ok(());
        }

        fn two_number_calc(
            &self,
            stack: &mut VecDeque<Atom>,
            func: fn(Rational64, Rational64) -> Rational64,
        ) -> Result<(), ()> {
            if let Some(Atom::Number(b)) = stack.pop_front() {
                if let Some(Atom::Number(a)) = stack.pop_front() {
                    stack.push_front(Atom::Number(func(a, b)));
                    return Ok(());
                }
            }
            return Err(());
        }
    }

    pub enum TwoNumberCalcType {
        Plus,
        Minus,
        Multiply,
        Divide,
        Mod,
    }

    impl TwoNumberCalcType {
        fn get_func(&self) -> fn(Rational64, Rational64) -> Rational64 {
            match self {
                TwoNumberCalcType::Plus => |a, b| a + b,
                TwoNumberCalcType::Minus => |a, b| a - b,
                TwoNumberCalcType::Multiply => |a, b| a * b,
                TwoNumberCalcType::Divide => |a, b| a / b,
                TwoNumberCalcType::Mod => |a, b| a % b,
            }
        }
    }
}
