pub mod parser {
    #[derive(Debug)]
    pub struct LexerBehaviors {
        behaviors: Vec<LexerBehavior>,
    }

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
    }

    #[derive(Debug, PartialEq, Eq)]
    pub struct LexerTellAgent {
        variable: String,
        expression: String,
    }

    use nom::{
        character::complete::multispace0,
        character::streaming::{alpha1, char},
        combinator::map,
        sequence::tuple,
        IResult,
    };

    use crate::{create_tell_agent, Agent};

    type Res<'a, T> = IResult<&'a str, T>;

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
        map(tuple((char('('), multispace0, char(')'))), |(_, _, _)| {
            LexerBehaviorArguments { terms: vec![] }
        })(code)
    }

    fn parse_agents(code: &str) -> Res<LexerAgent> {
        let tell = parse_tell_agent(code);
        match tell {
            Ok((str, agent)) => Ok((str, LexerAgent::TellAgent(agent))),
            Err(res) => Err(res),
        }
    }

    fn parse_tell_agent(code: &str) -> Res<LexerTellAgent> {
        map(
            tuple((alpha1, multispace0, char('='), multispace0, alpha1)),
            |(var, _, _, _, exp): (&str, _, _, _, &str)| LexerTellAgent {
                variable: var.into(),
                expression: exp.into(),
            },
        )(code)
    }

    #[cfg(test)]
    mod tests {
        use nom::Needed;

        use crate::parser::parser::{
            LexerAgent, LexerBehavior, LexerBehaviorArguments, LexerTellAgent,
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
                                expression: "B".into()
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
