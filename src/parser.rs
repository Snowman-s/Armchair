pub mod parser {
    #[derive(Debug, PartialEq, Eq)]
    pub struct LexerBehavior {
        name: String,
        arguments: LexerBehaviorArguments,
        root: LexerAgent,
    }

    #[derive(Debug, PartialEq, Eq)]
    pub struct LexerBehaviorArguments {
        // 仮置き
        terms: Vec<String>,
    }

    #[derive(Debug, PartialEq, Eq)]
    pub enum LexerAgent {
        TellAgent(LexerTellAgent),
        CallAgent(LexerCallAgent),
        AskAgent(Box<LexerAskAgent>),
        LinearAgent(Box<LexerLinearAgent>),
    }

    impl LexerAgent {
        fn compile(&self) -> Box<dyn Agent> {
            match self {
                LexerAgent::TellAgent(lexer) => Box::new(TellAgent {
                    variable_id: lexer.variable.clone(),
                    expression: lexer.expression.compile(),
                }),
                LexerAgent::CallAgent(lexer) => Box::new(CallAgent {
                    behavior_name: lexer.behavior_name.clone(),
                    argument_variable_list: lexer.argument_variable_list.clone(),
                }),
                LexerAgent::AskAgent(lexer) => Box::new(AskAgent {
                    ask_term: lexer.ask_term.compile(),
                    then: lexer.then.compile(),
                }),
                LexerAgent::LinearAgent(lexer) => Box::new(LinearAgent {
                    children: lexer.children.iter().map(|c| c.compile()).collect(),
                }),
            }
        }
    }

    #[derive(Debug, PartialEq, Eq)]
    pub struct LexerTellAgent {
        variable: String,
        expression: LexerExpressions,
    }

    #[derive(Debug, PartialEq, Eq)]
    pub struct LexerCallAgent {
        behavior_name: String,
        argument_variable_list: Vec<String>,
    }

    #[derive(Debug, PartialEq, Eq)]
    pub struct LexerAskAgent {
        ask_term: LexerAskTerm,
        then: LexerAgent,
    }

    #[derive(Debug, PartialEq, Eq)]
    pub enum LexerAskTerm {
        AEqualB(LexerExpressions, LexerExpressions),
    }

    impl LexerAskTerm {
        fn compile(&self) -> Box<dyn AskTerm> {
            Box::new(match self {
                Self::AEqualB(left, right) => AskTermAEqualB {
                    left: left.compile(),
                    right: right.compile(),
                },
            })
        }
    }

    #[derive(Debug, PartialEq, Eq)]
    pub struct LexerLinearAgent {
        children: Vec<LexerAgent>,
    }

    #[derive(Debug, PartialEq, Eq)]
    pub enum LexerExpressions {
        Variable(String),
        AtomString(String),
    }

    impl LexerExpressions {
        fn to_vec(&self) -> Vec<Expression> {
            match self {
                LexerExpressions::Variable(v) => vec![Expression::Variable(v.into())],
                LexerExpressions::AtomString(s) => vec![Expression::Atom(Atom::Atom(s.into()))],
            }
        }
        fn compile(&self) -> Expressions {
            Expressions {
                exprs: self.to_vec(),
            }
        }
    }

    #[derive(Debug, PartialEq, Eq)]
    pub enum CompileError {
        Error(String),
        Incomplete,
    }

    use nom::{
        branch::alt,
        bytes::{
            complete::take_until,
            streaming::{take_while, take_while_m_n},
        },
        character::complete::multispace0,
        character::streaming::{alpha1, char},
        combinator::{map, opt, recognize},
        multi::{many0, separated_list0, separated_list1},
        sequence::tuple,
        IResult,
    };

    use crate::{
        expressions::expressions::{Expression, Expressions},
        Agent, AskAgent, AskTerm, AskTermAEqualB, Atom, Behavior, CallAgent, LinearAgent,
        TellAgent,
    };

    type Res<'a, T> = IResult<&'a str, T>;

    pub fn compile_one_behavior(code: &str) -> Result<((String, Behavior), String), CompileError> {
        let lexer = parse_behavior(code);
        match lexer {
            Ok((code, ok_lexer)) => compile_behavior(&ok_lexer)
                .map(|behavior| (behavior, code.into()))
                .map_err(|s| CompileError::Error(s)),
            Err(nom::Err::Incomplete(_)) => Err(CompileError::Incomplete),
            Err(err) => Err(CompileError::Error(err.to_string())),
        }
    }

    pub fn parse_behavior(code: &str) -> Res<LexerBehavior> {
        map(
            tuple((
                multispace0,
                alpha1,
                multispace0,
                parse_behavior_arguments,
                multispace0,
                char(':'),
                char(':'),
                multispace0,
                parse_agents,
                multispace0,
                char('.'),
                multispace0,
            )),
            |(_, name, _, arguments, _, _, _, _, root, _, _, _)| LexerBehavior {
                name: name.into(),
                arguments,
                root,
            },
        )(code)
    }

    fn parse_behavior_arguments(code: &str) -> Res<LexerBehaviorArguments> {
        map(
            tuple((
                char('('),
                multispace0,
                opt(tuple((
                    alpha1,
                    multispace0,
                    many0(tuple((char(','), multispace0, alpha1))),
                ))),
                multispace0,
                char(')'),
            )),
            |(_, _, args, _, _)| LexerBehaviorArguments {
                terms: args
                    .map::<Vec<String>, fn((&str, _, Vec<_>)) -> Vec<String>>(|(first, _, rest)| {
                        let mut ret = vec![first.into()];
                        for (_, _, variable) in rest {
                            ret.push(variable.into());
                        }
                        ret
                    })
                    .unwrap_or(vec![]),
            },
        )(code)
    }

    fn parse_agents(code: &str) -> Res<LexerAgent> {
        alt((
            //優先
            parse_linear_agent,
            parse_ask_agent,
            //非優先
            parse_tell_agent,
            parse_call_agent,
        ))(code)
    }

    fn parse_bracket_agents(code: &str) -> Res<LexerAgent> {
        map(
            tuple((char('('), multispace0, parse_agents, multispace0, char(')'))),
            |(_, _, it, _, _)| it,
        )(code)
    }

    fn parse_agents_without_linear(code: &str) -> Res<LexerAgent> {
        alt((
            //優先
            parse_ask_agent,
            //非優先
            parse_tell_agent,
            parse_call_agent,
        ))(code)
    }

    fn parse_agents_without_linear_or_bracket(code: &str) -> Res<LexerAgent> {
        alt((parse_bracket_agents, parse_agents_without_linear))(code)
    }

    fn parse_tell_agent(code: &str) -> Res<LexerAgent> {
        map(
            tuple((
                parse_variable,
                multispace0,
                char('='),
                multispace0,
                parse_expressions,
            )),
            |(var, _, _, _, exp): (&str, _, _, _, LexerExpressions)| {
                LexerAgent::TellAgent(LexerTellAgent {
                    variable: var.into(),
                    expression: exp,
                })
            },
        )(code)
    }

    fn parse_call_agent(code: &str) -> Res<LexerAgent> {
        map(
            tuple((
                alpha1,
                multispace0,
                char('('),
                separated_list0(tuple((multispace0, char(','), multispace0)), alpha1),
                multispace0,
                char(')'),
            )),
            |(behavior_name, _, _, exp, _, _): (&str, _, _, Vec<&str>, _, _)| {
                LexerAgent::CallAgent(LexerCallAgent {
                    behavior_name: behavior_name.into(),
                    argument_variable_list: exp.into_iter().map(|s| s.into()).collect(),
                })
            },
        )(code)
    }

    fn parse_ask_agent(code: &str) -> Res<LexerAgent> {
        map(
            tuple((
                parse_ask_term,
                multispace0,
                char('-'),
                char('>'),
                multispace0,
                parse_agents_without_linear_or_bracket,
            )),
            |(ask_term, _, _, _, _, child)| {
                LexerAgent::AskAgent(Box::new(LexerAskAgent {
                    ask_term,
                    then: child,
                }))
            },
        )(code)
    }

    fn parse_ask_term(code: &str) -> Res<LexerAskTerm> {
        map(
            tuple((
                parse_expressions,
                multispace0,
                char('='),
                multispace0,
                parse_expressions,
            )),
            |(leftside, _, _, _, rightside): (LexerExpressions, _, _, _, LexerExpressions)| {
                LexerAskTerm::AEqualB(leftside, rightside)
            },
        )(code)
    }

    fn parse_linear_agent(code: &str) -> Res<LexerAgent> {
        map(
            tuple((
                parse_agents_without_linear_or_bracket,
                multispace0,
                char(','),
                multispace0,
                separated_list1(
                    tuple((multispace0, char(','), multispace0)),
                    parse_agents_without_linear_or_bracket,
                ),
            )),
            |(a1, _, _, _, mut a2s)| {
                let mut children = vec![a1];
                children.append(&mut a2s);
                LexerAgent::LinearAgent(Box::new(LexerLinearAgent { children }))
            },
        )(code)
    }

    fn parse_until_non_symbol(code: &str) -> Res<&str> {
        recognize(take_while(|s: char| {
            ![',', '(', ')', '\'', ' ', '　', ':', '-', '>', '.', '='].contains(&s)
        }))(code)
    }

    fn parse_variable(code: &str) -> Res<&str> {
        alt((
            // '_' から始まる
            recognize(tuple((char('_'), parse_until_non_symbol))),
            // アルファベット大文字から始まる
            recognize(tuple((
                take_while_m_n(1, 1, |chr| ('A' <= chr && chr <= 'Z')),
                parse_until_non_symbol,
            ))),
        ))(code)
    }

    fn parse_expressions(code: &str) -> Res<LexerExpressions> {
        alt((
            // 変数
            map(parse_variable, |s| LexerExpressions::Variable(s.into())),
            // 文字列アトム
            map(
                alt((
                    // '' で囲まれている
                    map(
                        tuple((char('\''), take_until("'"), char('\''))),
                        |(_, s, _)| s,
                    ),
                    // 任意の文字列
                    parse_until_non_symbol,
                )),
                |s| LexerExpressions::AtomString(s.into()),
            ),
        ))(code)
    }

    fn compile_behavior(lexer: &LexerBehavior) -> Result<(String, Behavior), String> {
        Ok((
            lexer.name.clone(),
            Behavior {
                argument_list: lexer.arguments.terms.clone(),
                root: lexer.root.compile(),
            },
        ))
    }

    #[cfg(test)]
    mod tests {
        use nom::Needed;

        use crate::parser::parser::{
            LexerAgent, LexerBehavior, LexerBehaviorArguments, LexerExpressions, LexerTellAgent,
        };

        use super::parse_behavior;

        #[test]
        fn lexer_test() {
            match parse_behavior("agent() :: A=B. ") {
                Ok((code, tree)) => {
                    assert!(code.is_empty());
                    assert_eq!(
                        tree,
                        LexerBehavior {
                            name: "agent".to_string(),
                            arguments: LexerBehaviorArguments { terms: vec![] },
                            root: LexerAgent::TellAgent(LexerTellAgent {
                                variable: "A".into(),
                                expression: LexerExpressions::Variable("B".into())
                            })
                        }
                    )
                }
                Err(_) => {
                    assert!(false);
                }
            }
        }
        #[test]
        fn lexer_incomplete_test() {
            match parse_behavior("agent() :: A=") {
                Err(nom::Err::Incomplete(Needed::Size(_))) => {
                    // do nothing (assert true)
                }
                _ => assert!(false),
            }
        }
    }
}
