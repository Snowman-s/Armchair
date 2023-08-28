pub mod parser {
    #[derive(Debug, PartialEq, Eq)]
    pub struct LexerBehavior {
        name: String,
        arguments: Vec<LexerBehaviorParameter>,
        root: LexerAgent,
    }

    #[derive(Debug, PartialEq, Eq)]
    pub enum ParseResult<B, Q> {
        Behavior(B),
        Question(Q),
    }

    #[derive(Debug, PartialEq, Eq, Clone)]
    pub enum LexerBehaviorParameter {
        Asker(String),
        Teller(String),
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
                    argument_variables: lexer
                        .argument_variable_list
                        .iter()
                        .map(|s| match s {
                            LexerCallArgument::Asker(variable) => {
                                CallArgument::Asker(variable.into())
                            }
                            LexerCallArgument::Teller(variable) => {
                                CallArgument::Teller(variable.into())
                            }
                            LexerCallArgument::Expression(expr) => {
                                CallArgument::Expression(expr.compile())
                            }
                        })
                        .collect(),
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
        argument_variable_list: Vec<LexerCallArgument>,
    }

    #[derive(Debug, PartialEq, Eq)]
    pub enum LexerCallArgument {
        Asker(String),
        Teller(String),
        Expression(LexerExpressions),
    }

    #[derive(Debug, PartialEq, Eq)]
    pub struct LexerAskAgent {
        ask_term: LexerAskTerm,
        then: LexerAgent,
    }

    #[derive(Debug, PartialEq, Eq)]
    pub enum LexerAskTerm {
        Vec(LexerAskTermVec),
        Exists(LexerAskTermExists),
    }

    impl LexerAskTerm {
        fn compile(&self) -> Box<dyn AskTerm> {
            match self {
                LexerAskTerm::Vec(vec) => vec.compile(),
                LexerAskTerm::Exists(exists) => exists.compile(),
            }
        }
    }

    #[derive(Debug, PartialEq, Eq)]
    pub struct LexerAskTermVec {
        first: LexerExpressions,
        remain: Vec<(LexerAskTermOp, LexerExpressions)>,
    }

    #[derive(Debug, PartialEq, Eq)]
    pub enum LexerAskTermOp {
        AEqualB,
        AGTB,
        AGEB,
        ALTB,
        ALEB,
    }
    impl LexerAskTermOp {
        fn compile(&self) -> AskTermOp {
            match self {
                LexerAskTermOp::AEqualB => AskTermOp::AEqualB,
                LexerAskTermOp::AGTB => AskTermOp::AGTB,
                LexerAskTermOp::AGEB => AskTermOp::AGEB,
                LexerAskTermOp::ALTB => AskTermOp::ALTB,
                LexerAskTermOp::ALEB => AskTermOp::ALEB,
            }
        }
    }

    impl LexerAskTermVec {
        fn compile(&self) -> Box<dyn AskTerm> {
            create_ask_term_vec(
                self.first.compile(),
                self.remain
                    .iter()
                    .map(|(a, b)| (a.compile(), b.compile()))
                    .collect(),
            )
        }
    }

    #[derive(Debug, PartialEq, Eq)]
    pub struct LexerAskTermExists {
        exis_term: LexerExisTerm,
        expr: LexerExpressions,
    }

    impl LexerAskTermExists {
        fn compile(&self) -> Box<dyn AskTerm> {
            create_ask_term_exists(self.exis_term.compile(), self.expr.compile())
        }
    }

    #[derive(Debug, PartialEq, Eq)]
    pub enum LexerExisTerm {
        Variable(String),
        Compound(String, Vec<LexerExistCompoundArg>),
    }

    #[derive(Debug, PartialEq, Eq)]
    pub enum LexerExistCompoundArg {
        Expression(LexerExpressions),
        Term(Box<LexerExisTerm>),
    }

    impl LexerExisTerm {
        fn compile(&self) -> ExistTerm {
            match self {
                LexerExisTerm::Variable(v) => ExistTerm::Variable(v.clone()),
                LexerExisTerm::Compound(name, args) => ExistTerm::Compound(
                    name.to_string(),
                    args.iter()
                        .map(|arg| match arg {
                            LexerExistCompoundArg::Expression(expr) => {
                                ExistTermCompoundArg::Expression(expr.compile())
                            }
                            LexerExistCompoundArg::Term(term) => {
                                ExistTermCompoundArg::Term(term.compile())
                            }
                        })
                        .collect(),
                ),
            }
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
        AtomNumber(Rational64),
        Compound(String, Vec<LexerCompoundArgument>),
        TwoNumberCalc(
            Box<LexerExpressions>,
            Vec<(LexerTwoNumberCalcType, Box<LexerExpressions>)>,
        ),
    }

    #[derive(Debug, PartialEq, Eq)]
    pub enum LexerCompoundArgument {
        Variable(String),
        Other(Box<LexerExpressions>),
    }

    #[derive(Debug, PartialEq, Eq)]
    pub enum LexerTwoNumberCalcType {
        Plus,
        Minus,
        Multiply,
        Divide,
        Mod,
    }

    impl LexerExpressions {
        fn to_vec(&self) -> Vec<Expression> {
            match self {
                LexerExpressions::Variable(v) => vec![Expression::Variable(v.into())],
                LexerExpressions::AtomString(s) => vec![Expression::Atom(Atom::Atom(s.into()))],
                LexerExpressions::AtomNumber(ratio) => vec![Expression::Atom(Atom::Number(*ratio))],
                LexerExpressions::Compound(name, args) => {
                    let mut compiled = vec![];

                    for arg in args.iter().rev() {
                        match arg {
                            LexerCompoundArgument::Variable(_) => {}
                            LexerCompoundArgument::Other(expr) => compiled.extend(expr.to_vec()),
                        }
                    }

                    compiled.push(Expression::Compound(
                        name.to_string(),
                        args.into_iter()
                            .map(|a| match a {
                                LexerCompoundArgument::Variable(v) => {
                                    ExpressionCompoundArg::Variable(v.to_string())
                                }
                                LexerCompoundArgument::Other(_) => {
                                    ExpressionCompoundArg::Expression
                                }
                            })
                            .collect(),
                    ));

                    compiled
                }
                LexerExpressions::TwoNumberCalc(first, remain) => {
                    let mut vec = first.to_vec();

                    for (calc_type, expr) in remain {
                        vec.extend(expr.to_vec());

                        vec.push(Expression::TwoNumberCalc(match calc_type {
                            LexerTwoNumberCalcType::Plus => TwoNumberCalcType::Plus,
                            LexerTwoNumberCalcType::Minus => TwoNumberCalcType::Minus,
                            LexerTwoNumberCalcType::Multiply => TwoNumberCalcType::Multiply,
                            LexerTwoNumberCalcType::Divide => TwoNumberCalcType::Divide,
                            LexerTwoNumberCalcType::Mod => TwoNumberCalcType::Mod,
                        }));
                    }

                    vec
                }
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
        character::streaming::char,
        combinator::{map, not, opt, peek, recognize},
        multi::{many0, many1, separated_list0, separated_list1},
        sequence::tuple,
        IResult,
    };
    use num_rational::Rational64;

    use crate::{
        create_ask_term_exists, create_ask_term_vec,
        expressions::expressions::{
            Expression, ExpressionCompoundArg, Expressions, TwoNumberCalcType,
        },
        Agent, AskAgent, AskTerm, AskTermOp, Atom, Behavior, BehaviorParam, CallAgent,
        CallArgument, ExistTerm, ExistTermCompoundArg, LinearAgent, TellAgent,
    };

    type Res<'a, T> = IResult<&'a str, T>;

    pub fn compile_one_behavior(
        code: &str,
    ) -> Result<(ParseResult<(String, Behavior), Box<dyn Agent>>, &str), CompileError> {
        let lexer = parse_behavior(code);
        match lexer {
            Ok((code, result)) => match result {
                ParseResult::Behavior(lexer) => compile_behavior(&lexer)
                    .map(|(name, behavior)| (ParseResult::Behavior((name.into(), behavior)), code))
                    .map_err(|s| CompileError::Error(s)),
                ParseResult::Question(question) => {
                    Ok((ParseResult::Question(question.compile()), code))
                }
            },
            Err(nom::Err::Incomplete(_)) => Err(CompileError::Incomplete),
            Err(err) => Err(CompileError::Error(err.to_string())),
        }
    }

    pub fn parse_behavior(code: &str) -> Res<ParseResult<LexerBehavior, LexerAgent>> {
        alt((
            map(
                tuple((
                    multispace0,
                    parse_until_non_symbol,
                    multispace0,
                    parse_behavior_parameters,
                    multispace0,
                    char(':'),
                    char(':'),
                    multispace0,
                    parse_agents,
                    multispace0,
                    char('.'),
                    multispace0,
                )),
                |(_, name, _, arguments, _, _, _, _, root, _, _, _)| {
                    ParseResult::Behavior(LexerBehavior {
                        name: name.into(),
                        arguments,
                        root,
                    })
                },
            ),
            map(
                tuple((
                    multispace0,
                    char('?'),
                    multispace0,
                    parse_agents,
                    multispace0,
                    char('.'),
                    multispace0,
                )),
                |(_, _, _, it, _, _, _)| ParseResult::Question(it),
            ),
        ))(code)
    }

    fn parse_behavior_parameters(code: &str) -> Res<Vec<LexerBehaviorParameter>> {
        map(
            tuple((
                char('('),
                multispace0,
                opt(tuple((
                    parse_behavior_param,
                    multispace0,
                    many0(tuple((char(','), multispace0, parse_behavior_param))),
                ))),
                multispace0,
                char(')'),
            )),
            |(_, _, args, _, _)| {
                args.map(|(first, _, rest)| {
                    let mut ret = vec![first];
                    for (_, _, variable) in rest {
                        ret.push(variable);
                    }
                    ret
                })
                .unwrap_or(vec![])
            },
        )(code)
    }

    fn parse_behavior_param(code: &str) -> Res<LexerBehaviorParameter> {
        alt((
            map(
                tuple((char('!'), multispace0, parse_variable)),
                |(_, _, v)| LexerBehaviorParameter::Teller(v.into()),
            ),
            map(parse_variable, |v| LexerBehaviorParameter::Asker(v.into())),
        ))(code)
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
                parse_until_non_symbol,
                multispace0,
                char('('),
                separated_list0(
                    tuple((multispace0, char(','), multispace0)),
                    parse_call_argument,
                ),
                multispace0,
                char(')'),
            )),
            |(behavior_name, _, _, exp, _, _)| {
                LexerAgent::CallAgent(LexerCallAgent {
                    behavior_name: behavior_name.into(),
                    argument_variable_list: exp,
                })
            },
        )(code)
    }

    fn parse_call_argument(code: &str) -> Res<LexerCallArgument> {
        alt((
            map(
                tuple((char('!'), multispace0, parse_variable)),
                |(_, _, v)| LexerCallArgument::Teller(v.into()),
            ),
            map(
                tuple((
                    parse_variable,
                    multispace0,
                    peek(alt((char(','), char(')')))),
                )),
                |(v, _, _)| LexerCallArgument::Asker(v.into()),
            ),
            map(parse_expressions, |e| LexerCallArgument::Expression(e)),
        ))(code)
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
        alt((
            map(
                tuple((
                    parse_exist_term,
                    multispace0,
                    char(':'),
                    char('='),
                    multispace0,
                    parse_expressions,
                )),
                |(exis_term, _, _, _, _, expr)| {
                    LexerAskTerm::Exists(LexerAskTermExists { exis_term, expr })
                },
            ),
            map(
                tuple((
                    parse_expressions,
                    many1(tuple((
                        multispace0,
                        alt((
                            map(char('='), |_| LexerAskTermOp::AEqualB),
                            map(tuple((char('<'), char('='))), |_| LexerAskTermOp::ALEB),
                            map(tuple((char('>'), char('='))), |_| LexerAskTermOp::AGEB),
                            map(char('<'), |_| LexerAskTermOp::ALTB),
                            map(char('>'), |_| LexerAskTermOp::AGTB),
                        )),
                        multispace0,
                        parse_expressions,
                    ))),
                )),
                |(first, remain)| {
                    LexerAskTerm::Vec(LexerAskTermVec {
                        first: first,
                        remain: remain
                            .into_iter()
                            .map(
                                |(_, op, _, expr): (_, LexerAskTermOp, _, LexerExpressions)| {
                                    (op, expr)
                                },
                            )
                            .collect(),
                    })
                },
            ),
        ))(code)
    }

    fn parse_exist_term(code: &str) -> Res<LexerExisTerm> {
        alt((
            // コンパウンド
            map(
                tuple((
                    parse_until_non_symbol,
                    multispace0,
                    char('('),
                    multispace0,
                    separated_list0(
                        tuple((multispace0, char(','), multispace0)),
                        alt((
                            map(parse_exist_term, |term| {
                                LexerExistCompoundArg::Term(Box::new(term))
                            }),
                            map(parse_expressions, |expr| {
                                LexerExistCompoundArg::Expression(expr)
                            }),
                        )),
                    ),
                    char(')'),
                )),
                |(name, _, _, _, vec, _)| LexerExisTerm::Compound(name.to_string(), vec),
            ),
            // 変数
            map(parse_variable, |v| LexerExisTerm::Variable(v.to_string())),
        ))(code)
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
            //2項演算 加減算
            parse_two_number_add_expression,
            //2項演算 乗除算
            parse_two_number_multiply_expression,
            // 単項
            parse_expression_term,
        ))(code)
    }

    fn parse_two_number_add_expression(code: &str) -> Res<LexerExpressions> {
        map(
            tuple((
                alt((parse_two_number_multiply_expression, parse_expression_term)),
                many1(tuple((
                    multispace0,
                    alt((
                        map(char('+'), |_| LexerTwoNumberCalcType::Plus),
                        map(
                            tuple((peek(not(tuple((char('-'), char('>'))))), char('-'))),
                            |_| LexerTwoNumberCalcType::Minus,
                        ),
                    )),
                    multispace0,
                    alt((parse_two_number_multiply_expression, parse_expression_term)),
                ))),
            )),
            |(first, remain)| {
                LexerExpressions::TwoNumberCalc(
                    Box::new(first),
                    remain
                        .into_iter()
                        .map(|(_, calctype, _, expr)| (calctype, Box::new(expr)))
                        .collect(),
                )
            },
        )(code)
    }

    fn parse_two_number_multiply_expression(code: &str) -> Res<LexerExpressions> {
        map(
            tuple((
                parse_expression_term,
                many1(tuple((
                    multispace0,
                    alt((
                        map(char('*'), |_| LexerTwoNumberCalcType::Multiply),
                        map(char('/'), |_| LexerTwoNumberCalcType::Divide),
                        map(char('%'), |_| LexerTwoNumberCalcType::Mod),
                    )),
                    multispace0,
                    parse_expression_term,
                ))),
            )),
            |(first, remain)| {
                LexerExpressions::TwoNumberCalc(
                    Box::new(first),
                    remain
                        .into_iter()
                        .map(|(_, calctype, _, expr)| (calctype, Box::new(expr)))
                        .collect(),
                )
            },
        )(code)
    }

    fn parse_expression_term(code: &str) -> Res<LexerExpressions> {
        alt((
            // 括弧式
            map(
                tuple((
                    char('('),
                    multispace0,
                    parse_expressions,
                    multispace0,
                    char(')'),
                )),
                |(_, _, s, _, _)| s,
            ),
            // コンパウンド
            map(
                tuple((
                    parse_until_non_symbol,
                    multispace0,
                    char('('),
                    multispace0,
                    separated_list0(
                        tuple((multispace0, char(','), multispace0)),
                        alt((
                            map(
                                tuple((
                                    parse_variable,
                                    peek(tuple((multispace0, alt((char(','), char(')')))))),
                                )),
                                |(v, _)| LexerCompoundArgument::Variable(v.to_string()),
                            ),
                            map(parse_expressions, |v| {
                                LexerCompoundArgument::Other(Box::new(v))
                            }),
                        )),
                    ),
                    char(')'),
                )),
                |(name, _, _, _, vec, _)| LexerExpressions::Compound(name.to_string(), vec),
            ),
            // 変数
            map(parse_variable, |s| LexerExpressions::Variable(s.into())),
            // 有理数アトム
            map(nom::character::streaming::i64, |decimal| {
                LexerExpressions::AtomNumber(Rational64::new(decimal, 1))
            }),
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
                param_list: lexer
                    .arguments
                    .iter()
                    .map(|s| match s {
                        LexerBehaviorParameter::Asker(v) => BehaviorParam::Asker(v.into()),
                        LexerBehaviorParameter::Teller(v) => BehaviorParam::Teller(v.into()),
                    })
                    .collect(),
                root: lexer.root.compile(),
            },
        ))
    }

    #[cfg(test)]
    mod tests {
        use nom::Needed;
        use num_rational::Rational64;

        use crate::parser::parser::{
            LexerAgent, LexerAskTerm, LexerAskTermOp, LexerAskTermVec, LexerBehavior,
            LexerCompoundArgument, LexerExpressions, LexerTellAgent, LexerTwoNumberCalcType,
            ParseResult,
        };

        use super::{parse_ask_agent, parse_behavior, parse_tell_agent};

        #[test]
        fn lexer_test() {
            match parse_behavior("agent() :: A=B. ") {
                Ok((code, tree)) => {
                    assert!(code.is_empty());
                    assert_eq!(
                        tree,
                        ParseResult::Behavior(LexerBehavior {
                            name: "agent".to_string(),
                            arguments: vec![],
                            root: LexerAgent::TellAgent(LexerTellAgent {
                                variable: "A".into(),
                                expression: LexerExpressions::Variable("B".into())
                            })
                        })
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
        #[test]
        fn lexer_two_term_expression_test() {
            match parse_ask_agent("10 / 6 + A - B * 3 = 20 -> a().") {
                Err(_) => assert!(false),
                Ok((_, agent_enum)) => {
                    if let LexerAgent::AskAgent(agent) = agent_enum {
                        assert_eq!(
                            agent.ask_term,
                            LexerAskTerm::Vec(LexerAskTermVec {
                                first: LexerExpressions::TwoNumberCalc(
                                    Box::new(LexerExpressions::TwoNumberCalc(
                                        Box::new(LexerExpressions::AtomNumber(Rational64::new(
                                            10, 1
                                        ))),
                                        vec![(
                                            LexerTwoNumberCalcType::Divide,
                                            Box::new(LexerExpressions::AtomNumber(
                                                Rational64::new(6, 1)
                                            ))
                                        )]
                                    )),
                                    vec![
                                        (
                                            LexerTwoNumberCalcType::Plus,
                                            Box::new(LexerExpressions::Variable("A".to_string())),
                                        ),
                                        (
                                            LexerTwoNumberCalcType::Minus,
                                            Box::new(LexerExpressions::TwoNumberCalc(
                                                Box::new(LexerExpressions::Variable(
                                                    "B".to_string()
                                                )),
                                                vec![(
                                                    LexerTwoNumberCalcType::Multiply,
                                                    Box::new(LexerExpressions::AtomNumber(
                                                        Rational64::new(3, 1)
                                                    ))
                                                )]
                                            ))
                                        )
                                    ]
                                ),
                                remain: vec![(
                                    LexerAskTermOp::AEqualB,
                                    LexerExpressions::AtomNumber(Rational64::new(20, 1))
                                )]
                            })
                        );
                    } else {
                        assert!(false);
                    }
                }
            }
        }

        #[test]
        fn lexer_long_ask_term_test() {
            match parse_ask_agent("10 = 20 >= A <= B > A < C -> a().") {
                Err(_) => {
                    assert!(false);
                }
                Ok((_, agent_enum)) => {
                    if let LexerAgent::AskAgent(agent) = agent_enum {
                        assert_eq!(
                            agent.ask_term,
                            LexerAskTerm::Vec(LexerAskTermVec {
                                first: LexerExpressions::AtomNumber(10.into()),
                                remain: vec![
                                    (
                                        LexerAskTermOp::AEqualB,
                                        LexerExpressions::AtomNumber(20.into())
                                    ),
                                    (LexerAskTermOp::AGEB, LexerExpressions::Variable("A".into())),
                                    (LexerAskTermOp::ALEB, LexerExpressions::Variable("B".into())),
                                    (LexerAskTermOp::AGTB, LexerExpressions::Variable("A".into())),
                                    (LexerAskTermOp::ALTB, LexerExpressions::Variable("C".into())),
                                ],
                            })
                        )
                    } else {
                        assert!(false)
                    }
                }
            }
        }

        #[test]
        fn lexer_compound() {
            match parse_tell_agent("X = a((A), V).") {
                Ok((_, LexerAgent::TellAgent(agent))) => {
                    assert_eq!(
                        agent,
                        LexerTellAgent {
                            variable: "X".to_string(),
                            expression: LexerExpressions::Compound(
                                "a".to_string(),
                                vec![
                                    LexerCompoundArgument::Other(Box::new(
                                        LexerExpressions::Variable("A".to_string())
                                    )),
                                    LexerCompoundArgument::Variable("V".to_string())
                                ]
                            )
                        }
                    )
                }
                _ => {
                    assert!(false);
                }
            }
        }
    }
}
