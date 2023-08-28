pub mod expressions;
pub mod parser;

use expressions::expressions::Expressions;
use num_rational::Rational64;
use parser::parser::LexerExpressions;
use std::collections::{HashMap, HashSet};
use std::sync::{Arc, Condvar, Mutex};
use std::thread;

trait AskTerm: Send + Sync {
    fn prove(&self, constraints: &ExecuteEnvironment) -> ConstraintCheckResult;

    /** 上位に伝播する変数 */
    fn global_variables(&self) -> HashSet<String>;

    /** 上位に伝播しない変数 */
    fn local_variables(&self) -> HashSet<String>;
}

#[derive(Clone, Debug)]
pub enum Constraint {
    None,
    EqualTo(Atom),
}

#[derive(Clone, Debug)]
pub enum Atom {
    /** 真なるアトム (Stringは名前としての存在であってコードからは参照すべきではない) */
    Atom(String),
    /** 数値としてのアトム */
    Number(Rational64),
    /** 変数参照可 複合アトム */
    Compound(String, Vec<CompoundArg>),
}

impl Atom {
    fn eq(a: &Atom, b: &Atom) -> bool {
        let mut equal_vec = vec![(a.clone(), b.clone())];

        while let Some((a_cache, b_cache)) = equal_vec.pop() {
            match a_cache {
                Atom::Atom(a_str) => {
                    if let Atom::Atom(b_str) = b_cache {
                        if a_str == b_str {
                            continue;
                        }
                    }
                    return false;
                }
                Atom::Number(a_num) => {
                    if let Atom::Number(b_num) = b_cache {
                        //do nothing (passed test)
                        if a_num == b_num {
                            continue;
                        }
                    }
                    return false;
                }
                Atom::Compound(a_name, a_args) => {
                    if let Atom::Compound(b_name, b_args) = b_cache {
                        if a_name != b_name || a_args.len() != b_args.len() {
                            return false;
                        }

                        for (index, _) in a_args.iter().enumerate() {
                            let result = CompoundArg::eq_if_returned_thing_is_eq(
                                &a_args[index],
                                &b_args[index],
                            );
                            if let Some(r) = result {
                                equal_vec.extend(r);
                            } else {
                                return false;
                            }
                        }
                    } else {
                        return false;
                    }
                }
            }
        }

        true
    }
}

#[derive(Clone, Debug)]
pub enum CompoundArg {
    Atom(Atom),
    Variable(VariableRef),
}

impl CompoundArg {
    /**
     if return None, they cannot be equal.
    */
    fn eq_if_returned_thing_is_eq<'a, 'b>(
        a: &'a CompoundArg,
        b: &'a CompoundArg,
    ) -> Option<Vec<(Atom, Atom)>> {
        if let CompoundArg::Variable(a_ref) = a {
            if let CompoundArg::Variable(b_ref) = b {
                // どちらも変数の場合の処理
              match VariableRef::same_check(a_ref, b_ref) {
                VariableRefSameCheckResult::Equal => return Some(vec![]),
                VariableRefSameCheckResult::CanBeEqual => {},
                VariableRefSameCheckResult::Different => return None,
            }
                }
            }
        

        let func = |arg: &CompoundArg| match arg {
            CompoundArg::Atom(a_atom) => Some(a_atom.clone()),
            CompoundArg::Variable(v_ref) => {
              let Ok(Constraint::EqualTo(atom)) =     v_ref.wait_until_grounded() else {return None};
            
              Some(atom.clone())
        }
      };

        let a_expr: Option<Atom> = (func)(a);
        if let None = a_expr {
            return None;
        }
        let b_expr: Option<Atom> = (func)(b);
        if let None = b_expr {
            return None;
        }

        Some(vec![(a_expr.unwrap(), b_expr.unwrap())])
    }
}

impl ToString for Atom {
    fn to_string(&self) -> String {
        match self {
            Atom::Atom(atom) => format!("\'{}\'", atom),
            Atom::Number(rational) => rational.to_string(),
            Atom::Compound(comp, args) => {
                let arg_str: String = args
                    .iter()
                    .enumerate()
                    .map(|(idx, arg)| {
                        let target = match arg {
                            CompoundArg::Atom(atom) => atom.to_string(),
                            CompoundArg::Variable(v_ref) => {
                                if let VariableRight::Tellable = *v_ref.right.lock().unwrap() {
                                    "!".to_string()
                                } else {
                                    match v_ref.cons.0.lock().unwrap().clone() {
                                        Constraint::EqualTo(atom) => atom.to_string(),
                                        Constraint::None => "*".to_string(),
                                    }
                                }
                            }
                        };
                        if idx == 0 {
                            format!("{}", target)
                        } else {
                            format!(", {}", target)
                        }
                    })
                    .collect();

                format!("{}({})", comp, arg_str)
            }
        }
    }
}

pub struct Constraints {
    /** 変数名:制約*/
    constraints: HashMap<String, VariableRef>,
}

#[derive(Clone, Debug)]
pub struct VariableRef {
  right: Arc<Mutex<VariableRight>>,
  cons: Arc<(Mutex<Constraint>, Condvar)>,
}

impl VariableRef {
  fn tell(&self, told: Constraint) -> Result<(), ()> {
    let took_cons = self.get_tell_rights()?;

    let (original_cons, condvar) = &*took_cons.cons;
    *original_cons.lock().unwrap() = told;

    condvar.notify_all();

    Ok(())
  }

  /** 
 Err, if variable doesn't have ask-rights.
*/
fn get_constraint(&self) -> Result<Constraint, ()> {
  let took_ref = self.get_ask_rights()?;

  let (arc_constraints, _) = &*took_ref.cons;

  return Ok(arc_constraints.lock().unwrap().clone());
}

/** Wait until the variable's constraint is not None.
 Err, if variable doesn't have ask-rights.
*/
fn wait_until_grounded(&self) -> Result<Constraint, ()> {
    let took_ref = self.get_ask_rights()?;

    let (arc_constraints, condvar) = &*took_ref.cons;

    let mut the_constraint = arc_constraints.lock().unwrap();
    loop {
        if let Constraint::EqualTo(_) = *the_constraint {
            return Ok(the_constraint.clone());
        }

        the_constraint = condvar.wait(the_constraint).unwrap();
    }
}

fn took_ref(&self, right:VariableRight) -> VariableRef{
  VariableRef { right: Arc::new(Mutex::new(right)), cons: self.cons.clone()}
}

/**  private function
  - \<UnInitilized> | AskRight -> Set Tell-Right to self. Return (Ask-Right, data)
  - Others -> Return Err.
*/
fn get_ask_rights(&self) -> Result<VariableRef, ()> {
            let right_guard = self.right.lock().unwrap();
            match *right_guard {
                VariableRight::All | VariableRight::Askable => {
                    return Ok(self.took_ref(VariableRight::Askable));
                }
                _ => {
                    return Err(());
                }
            };
    }


/**  private function
  - \<UnInitilized> | AskRight -> Return (Ask-Right, data)
  - TellRight -> Set None-Right to self. Return (Tell-Right, data).
  - NoneRight -> Err(())
*/
fn get_ask_argument_rights(&self) -> Result<VariableRef, ()> {
            let mut right_guard = self.right.lock().unwrap();
            match *right_guard {
                VariableRight::All | VariableRight::Askable => {
                    return Ok(self.took_ref(VariableRight::Askable));
                }
                VariableRight::Tellable => {
                    *right_guard = VariableRight::None;
                    return Ok(self.took_ref(VariableRight::Tellable));
                }
                VariableRight::None => return Err(()),
            };
}

  /**  private function
    - \<UnInitilized> -> Set Ask-Right to self. Return (Tell-Right, data)
    - TellRight -> Return (Tell-Right, data).
    - Others -> Return Err.
  */
  fn get_tell_rights(&self) -> Result<VariableRef, ()> {
      let mut right_guard = self.right.lock().unwrap();
      match *right_guard {
          VariableRight::All => *right_guard = VariableRight::Askable,
          VariableRight::Tellable => *right_guard = VariableRight::None,
          _ => {
              return Err(());
          }
      };
      return Ok(self.took_ref(VariableRight::Tellable));
  }

  fn same_check(a_ref: &VariableRef,b_ref:&VariableRef) -> VariableRefSameCheckResult {
    if Arc::ptr_eq(&a_ref.cons, &b_ref.cons) {
    VariableRefSameCheckResult::Equal
} else {
  VariableRefSameCheckResult::CanBeEqual
}
  }
}

/*
  Remaining variable rights.
*/
#[derive(PartialEq, Eq, Clone, Debug)]
pub enum VariableRight {
    All,
    Askable,
    Tellable,
    None,
}

pub enum VariableRefSameCheckResult {
  Equal,
  CanBeEqual,
  Different
}

impl Constraints {
    pub fn new(variables: impl Iterator<Item = String>) -> Constraints {
        Constraints {
            constraints: variables
                .map(|variable_id| {
                    (
                        variable_id,
                        VariableRef {
                           right: Arc::new(Mutex::new(VariableRight::All)),
                           cons: Arc::new((Mutex::new(Constraint::None), Condvar::new())),
                        },
                    )
                })
                .collect(),
        }
    }

    fn tell(&self, variable_id: &String, told: Constraint) -> Result<(), ()> {
      let v_ref = self.get_tell_rights(variable_id)?;
        
      v_ref.tell(told)
    }

/** 
     Err, if variable doesn't have ask-rights.
    */
    pub fn get_constraint(&self, variable_id: &String) -> Result<Constraint, ()> {
      let v_ref = self.get_ask_rights(variable_id)?;
      
      v_ref.get_constraint()
  }

    /** Wait until the variable's constraint is not None.
     Err, if variable doesn't have ask-rights.
    */
    fn wait_until_grounded(&self, variable_id: &String) -> Result<Constraint, ()> {
        let v_ref = self.get_ask_rights(variable_id)?;
        
        v_ref.wait_until_grounded()
    }

    /**  private function
      - \<UnInitilized> | AskRight -> Set Tell-Right to self. Return (Ask-Right, data)
      - Others -> Return Err.
    */
    fn get_ask_rights(&self, variable_id: &String) -> Result<VariableRef, ()> {
      self.constraints.get(variable_id).and_then(|v_ref|v_ref.get_ask_rights().ok()).ok_or(())
    }

    /**  private function
      - \<UnInitilized> | AskRight -> Return (Ask-Right, data)
      - TellRight -> Set None-Right to self. Return (Tell-Right, data).
      - NoneRight -> Err(())
    */
    fn get_ask_argument_rights(&self, variable_id: &String) -> Result<VariableRef, ()> {
      self.constraints.get(variable_id).and_then(|v_ref|v_ref.get_ask_argument_rights().ok()).ok_or(())
    }

    /**  private function
      - \<UnInitilized> -> Set Ask-Right to self. Return (Tell-Right, data)
      - TellRight -> Return (Tell-Right, data).
      - Others -> Return Err.
    */
    fn get_tell_rights(&self, variable_id: &String) -> Result<VariableRef, ()> {
        self.constraints.get(variable_id).and_then(|v_ref|v_ref.get_tell_rights().ok()).ok_or(())
    }
}

enum ConstraintCheckResult {
    /**  constraints の一部を返す*/
    SUCCEED(HashMap<String, VariableRef>),
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

    pub fn clone_and_add_variable(&self, variables: HashMap<String, VariableRef>) -> ExecuteEnvironment {
        let mut new_constraint = self.key_store.constraints.clone();
        for (id, v_ref) in variables {
            if !new_constraint.contains_key(&id) {
                new_constraint.insert(
                    id,
                    v_ref,
                );
            }
        }

        ExecuteEnvironment {
            behaviors: self.behaviors,
            key_store: Constraints {
                constraints: new_constraint,
            },
        }
    }

    pub fn key_store(&self) -> &Constraints {
        &self.key_store
    }

    /**
     * this function blocks execution until conforming "guard",
     *
     * Both arguments' and params' length must be same.
     *
     * "initilize_variable" is used for "inner" variables.
     * If it's an element already in param, it is ignored.
     */
    fn create_and_copy_to_new_constraint(
        &self,
        arguments: &Vec<CallArgument>,
        param: &Vec<BehaviorParam>,
        initilize_variables: impl Iterator<Item = String>,
    ) -> Result<Constraints, ()> {
        let mut new_constraints = HashMap::new();

        for (index, argument) in arguments.iter().enumerate() {
            let p = param.get(index).ok_or(())?;
            argument.apply_to_new_constraint(self, &mut new_constraints, &param[index])?;
        }

        for init in initilize_variables {
            if !new_constraints.contains_key(&init) {
                new_constraints.insert(
                    init.clone(),
                    VariableRef {
                       right: Arc::new(Mutex::new(VariableRight::All)),
                       cons: Arc::new((Mutex::new(Constraint::None), Condvar::new())),
                    },
                );
            }
        }

        Ok(Constraints {
            constraints: new_constraints,
        })
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
    // このエージェントを呼ぶのに必要とする変数
    fn variables(&self) -> HashSet<String>;

    // 上層に伝播させる必要変数 (Askエージェントのみが使用する。残りはローカル変数。)
    fn global_variables(&self) -> HashSet<String> {
        self.variables()
    }
}

fn call(
    env: &ExecuteEnvironment,
    question: &Behavior,
    argument_variables: &Vec<CallArgument>,
) -> Result<(), ()> {
    let new_environment = ExecuteEnvironment {
        behaviors: env.behaviors,
        key_store: env.create_and_copy_to_new_constraint(
            argument_variables,
            &question.param_list,
            question.root.variables().into_iter(),
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
            .tell(&self.variable_id, Constraint::EqualTo(expr_solved))
    }

    fn variables(&self) -> HashSet<String> {
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
    argument_variables: Vec<CallArgument>,
}

enum CallArgument {
    Asker(String),
    Teller(String),
    Expression(Expressions),
}

impl CallArgument {
    fn variables(&self) -> HashSet<String> {
        match self {
            CallArgument::Asker(v) => {
                let mut ret = HashSet::new();
                ret.insert(v.clone());
                ret
            }
            CallArgument::Teller(v) => {
                let mut ret = HashSet::new();
                ret.insert(v.clone());
                ret
            }
            CallArgument::Expression(expr) => expr.variables(),
        }
    }

    fn apply_to_new_constraint(
        &self,
        now_env: &ExecuteEnvironment,
        creating_cons: &mut HashMap<String, VariableRef>,
        param: &BehaviorParam,
    ) -> Result<(), ()> {
        match self {
            CallArgument::Asker(v) => {
                let new_variable = now_env.key_store.get_ask_argument_rights(v)?;
                creating_cons.insert(param.get_variable_name().clone(), new_variable);
                Ok(())
            }
            CallArgument::Teller(v) => {
                let new_variable = now_env.key_store.get_tell_rights(v)?;
                creating_cons.insert(param.get_variable_name().clone(), new_variable);
                Ok(())
            }
            CallArgument::Expression(expr) => {
                let expr_solved = expr.solve(now_env)?;

                creating_cons.insert(
                    param.get_variable_name().clone(),
                    VariableRef {
                     right:   Arc::new(Mutex::new(VariableRight::Askable)),
                      cons:  Arc::new((Mutex::new(Constraint::EqualTo(expr_solved)), Condvar::new())),
                    },
                );
                Ok(())
            }
        }
    }
}

impl Agent for CallAgent {
    fn solve(&self, environment: &ExecuteEnvironment<'_>) -> Result<(), ()> {
        let a = environment.behaviors.get(&self.behavior_name).ok_or(())?;
        let call_result = call(environment, a, &self.argument_variables);
        if let Ok(_) = call_result {
            return Ok(());
        } else {
            return Err(());
        }
    }

    fn variables(&self) -> HashSet<String> {
        self.argument_variables
            .iter()
            .flat_map(|arg| arg.variables())
            .collect()
    }
}

fn create_call_agent(
    behavior_name: String,
    argument_variables: Vec<CallArgument>,
) -> Box<dyn Agent> {
    Box::new(CallAgent {
        behavior_name,
        argument_variables,
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
                        .spawn_scoped(scope, move || {element.solve(environment)})
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

    fn variables(&self) -> HashSet<String> {
        self.children
            .iter()
            .flat_map(|child| child.global_variables())
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
        let check_res = self.ask_term.prove(&environment);

        match check_res {
            ConstraintCheckResult::SUCCEED(map) => {
              let new_environment = environment.clone_and_add_variable(map);

              self.then.solve(&new_environment)
            },
            ConstraintCheckResult::CONTRADICTION => Ok(()),
        }
    }

    fn variables(&self) -> HashSet<String> {
        let mut ret = self.ask_term.global_variables();

        ret.extend(self.ask_term.local_variables());
        ret.extend(self.then.variables());

        ret
    }

    fn global_variables(&self) -> HashSet<String> {
        let mut ret = self.then.global_variables();
        ret.extend(self.ask_term.global_variables());

        for local in self.ask_term.local_variables(){
          ret.remove(&local);
        }

        ret
    }
}

fn create_ask_agent(ask_term: Box<dyn AskTerm>, then: Box<dyn Agent>) -> Box<dyn Agent> {
    Box::new(AskAgent { ask_term, then })
}

struct AskTermVec {
    first: Expressions,
    remain: Vec<(AskTermOp, Expressions)>,
}

enum AskTermOp {
    AEqualB,
    AGTB,
    ALTB,
    AGEB,
    ALEB,
}

impl AskTermOp {
    fn test(&self, a: &Atom, b: &Atom) -> bool {
        if let AskTermOp::AEqualB = self {
            Atom::eq(a, b)
        } else {
            if let Atom::Number(a_number) = a {
                if let Atom::Number(b_number) = b {
                    return match self {
                        AskTermOp::AGTB => a_number > b_number,
                        AskTermOp::ALTB => a_number < b_number,
                        AskTermOp::AGEB => a_number >= b_number,
                        AskTermOp::ALEB => a_number <= b_number,
                        AskTermOp::AEqualB => panic!(),
                    };
                }
            }
            panic!()
        }
    }
}

impl AskTerm for AskTermVec {
    fn prove(&self, env: &ExecuteEnvironment) -> ConstraintCheckResult {
        let mut expr_cache = if let Ok(a) = self.first.solve(env) {
            a
        } else {
            return ConstraintCheckResult::CONTRADICTION;
        };

        for (op, expr) in &self.remain {
            let expr_solved = if let Ok(a) = expr.solve(env) {
                a
            } else {
                return ConstraintCheckResult::CONTRADICTION;
            };

            if !op.test(&expr_cache, &expr_solved) {
                return ConstraintCheckResult::CONTRADICTION;
            }

            expr_cache = expr_solved;
        }

        ConstraintCheckResult::SUCCEED(HashMap::new())
    }

    fn global_variables(&self) -> HashSet<String> {
        let mut ret = HashSet::from(self.first.variables());
        for (_, expr) in &self.remain {
            ret.extend(expr.variables());
        }

        ret
    }

    fn local_variables(&self) -> HashSet<String> {
        HashSet::new()
    }
}

fn create_ask_term_vec(
    first: Expressions,
    remain: Vec<(AskTermOp, Expressions)>,
) -> Box<dyn AskTerm> {
    Box::new(AskTermVec { first, remain })
}

struct AskTermExists {
    expr: Expressions,
    exis_term: ExistTerm,
}

impl AskTerm for AskTermExists {
    fn prove(&self, constraints: &ExecuteEnvironment) -> ConstraintCheckResult {
        let solved_result = self.expr.solve(constraints);

        match solved_result {
            Ok(solved) => self.exis_term.unification(VariableRefOrAtom::Atom(solved), constraints),
            Err(()) => ConstraintCheckResult::CONTRADICTION,
        }
    }

    fn global_variables(&self) -> HashSet<String> {
        let mut ret = self.expr.variables();
        ret.extend(self.exis_term.global_variables());

        ret
    }

    fn local_variables(&self) -> HashSet<String> {
        self.exis_term.local_variables()
    }
}

enum ExistTerm {
    Variable(String),
    Compound(String, Vec<ExistTermCompoundArg>),
}
enum ExistTermCompoundArg {
    Expression(Expressions),
    Term(ExistTerm)
}

enum VariableRefOrAtom {
  VRef(VariableRef),
  Atom(Atom)
}

impl VariableRefOrAtom {
  fn get_atom(&self) -> Result<Atom, ()>{
    match self {
        VariableRefOrAtom::VRef(v_ref) => {
          let Constraint::EqualTo(atom) = v_ref.wait_until_grounded()? else {return Err(())};
          Ok(atom)
        },
        VariableRefOrAtom::Atom(atom) => Ok(atom.clone()),
    }
  }
  fn as_ref(&self) -> VariableRef {
    match self {
      VariableRefOrAtom::VRef(v_ref) => {v_ref.clone()},
      VariableRefOrAtom::Atom(atom) =>VariableRef { right: Arc::new(Mutex::new(VariableRight::Askable)), 
        cons: Arc::new((Mutex::new(Constraint::EqualTo(atom.clone())), Condvar::new()))
      },
    }
  }
}

impl ExistTerm {
    fn global_variables(&self) -> HashSet<String> {
        match self {
            ExistTerm::Variable(_)=>{HashSet::new()},
            ExistTerm::Compound(_, vec) => vec
                .iter()
                .flat_map(|arg| match arg {
                    ExistTermCompoundArg::Expression(expr) => {
                        expr.variables().into()
                    },
                    ExistTermCompoundArg::Term(term) =>{
                      term.global_variables()                      
                    }
                })
                .collect(),
        }
    }

    fn local_variables(&self) -> HashSet<String> {
        match self {
          ExistTerm::Variable(v) => HashSet::from([v.clone()]),
          ExistTerm::Compound(_, vec) => vec
                .iter()
                .flat_map(|arg| match arg {
                    ExistTermCompoundArg::Term(term) => {
                      term.local_variables()
                    },
                    _ => HashSet::new(),
                })
                .collect(),
        }
    }

    /** to と一致するような制約を返却。 */
    fn unification(&self, to: VariableRefOrAtom, env: &ExecuteEnvironment) -> ConstraintCheckResult {
        match self {
            ExistTerm::Variable(v) =>{
              ConstraintCheckResult::SUCCEED(HashMap::from([(v.clone(), to.as_ref())]))
            }
            ExistTerm::Compound(self_name, self_args) => {
                let Ok(Atom::Compound(to_name, to_args)) = 
                  to.get_atom() else{ return ConstraintCheckResult::CONTRADICTION;};

                if self_name != &to_name || self_args.len() != to_args.len(){
                  return ConstraintCheckResult::CONTRADICTION;
                }

                let mut will_tell_args = HashMap::new();

                for index in 0..self_args.len(){
                  let self_arg = &self_args[index];
                  let to_arg = &to_args[index];

                  match self_arg {
                    ExistTermCompoundArg::Expression(expr) => {
                      let Ok(expr_solved) = expr.solve(env) else {
                        return ConstraintCheckResult::CONTRADICTION
                      };
                      let to_solved  = match to_arg {
                        CompoundArg::Atom(atom) => atom.clone(),
                        CompoundArg::Variable(variable) => {
                         let Ok(Constraint::EqualTo(atom)) = variable.wait_until_grounded() else {
                          return ConstraintCheckResult::CONTRADICTION;
                         };
                         atom
                        }
                      };
    
                      if !Atom::eq(&expr_solved, &to_solved) {
                        return ConstraintCheckResult::CONTRADICTION;                        
                      }
                    },
                    ExistTermCompoundArg::Term(term) => {
                      let pass_to_arg_unification  = match to_arg {
                        CompoundArg::Atom(atom) => VariableRefOrAtom::Atom(atom.clone()),
                        CompoundArg::Variable(variable) => VariableRefOrAtom::VRef(variable.clone())
                      };
                      match term.unification(pass_to_arg_unification, env) {
                        ConstraintCheckResult::SUCCEED(map) => will_tell_args.extend(map),
                        ConstraintCheckResult::CONTRADICTION => return ConstraintCheckResult::CONTRADICTION,
                      };
                    },
                }
            }
            ConstraintCheckResult::SUCCEED(will_tell_args)
        }
    }
  }
}

fn create_ask_term_exists(
  exis_term: ExistTerm,
  expr: Expressions,
) -> Box<dyn AskTerm> {
  Box::new(AskTermExists { exis_term, expr })
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::{
        call, create_ask_agent, create_ask_term_vec, create_call_agent, create_linear_agent,
        create_tell_agent,
        expressions::expressions::{Expression, ExpressionCompoundArg, Expressions},
        parser::parser::{compile_one_behavior, ParseResult},
        AskTermOp, AskTermVec, Atom, Behavior, BehaviorParam, CallArgument, Constraint,
        ExecuteEnvironment, AskTermExists, ExistTerm, ExistTermCompoundArg,
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

        let env = ExecuteEnvironment::new(&behaviors, question.variables().into_iter());

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

        let env = ExecuteEnvironment::new(&behaviors, question.variables().into_iter());

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
                            create_ask_term_vec(
                                Expressions {
                                    exprs: vec![Expression::Variable("I".into())],
                                },
                                vec![(
                                    AskTermOp::AEqualB,
                                    Expressions {
                                        exprs: vec![Expression::Atom(Atom::Atom("AAAAA".into()))],
                                    },
                                )],
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

        let env = ExecuteEnvironment::new(&behaviors, question.variables().into_iter());

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

        let env = ExecuteEnvironment::new(&behaviors, question.variables().into_iter());

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
    fn compound() {
        /*
        Question
        - ?X=comp(atom)->O=ok, X=comp(Y), Y=atom.
        */

        let question = create_linear_agent(vec![
            create_ask_agent(
                Box::new(AskTermVec {
                    first: Expressions {
                        exprs: vec![Expression::Variable("X".to_string())],
                    },
                    remain: vec![(
                        AskTermOp::AEqualB,
                        Expressions {
                            exprs: vec![
                                Expression::Atom(Atom::Atom("atom".to_string())),
                                Expression::Compound(
                                    "comp".to_string(),
                                    vec![ExpressionCompoundArg::Expression],
                                ),
                            ],
                        },
                    )],
                }),
                create_tell_agent(
                    "O".to_string(),
                    Expressions {
                        exprs: vec![Expression::Atom(Atom::Atom("ok".to_string()))],
                    },
                ),
            ),
            create_tell_agent(
                "X".to_string(),
                Expressions {
                    exprs: vec![Expression::Compound(
                        "comp".to_string(),
                        vec![ExpressionCompoundArg::Variable("Y".to_string())],
                    )],
                },
            ),
            create_tell_agent(
                "Y".to_string(),
                Expressions {
                    exprs: vec![Expression::Atom(Atom::Atom("atom".to_string()))],
                },
            ),
        ]);

        let binding = HashMap::new();
        let env = ExecuteEnvironment::new(&binding, question.variables().into_iter());

        let call_result = question.solve(&env);

        if let Err(_) = call_result {
            assert!(false);
        }

        if let Ok(Constraint::EqualTo(Atom::Atom(s))) = env.key_store.get_constraint(&"O".into()) {
            assert_eq!(s, "ok");
        } else {
            assert!(false);
        };
    }

    #[test]
    fn compound_aborting() {
        /*
        Question
        - ?X=Y->O=ok, X=a(K), Y=a(K).
        */

        let question = create_linear_agent(vec![
            create_ask_agent(
                Box::new(AskTermVec {
                    first: Expressions {
                        exprs: vec![Expression::Variable("X".to_string())],
                    },
                    remain: vec![(
                        AskTermOp::AEqualB,
                        Expressions {
                            exprs: vec![Expression::Variable("Y".to_string())],
                        },
                    )],
                }),
                create_tell_agent(
                    "O".to_string(),
                    Expressions {
                        exprs: vec![Expression::Atom(Atom::Atom("ok".to_string()))],
                    },
                ),
            ),
            create_tell_agent(
                "X".to_string(),
                Expressions {
                    exprs: vec![Expression::Compound(
                        "comp".to_string(),
                        vec![ExpressionCompoundArg::Variable("K".to_string())],
                    )],
                },
            ),
            create_tell_agent(
                "Y".to_string(),
                Expressions {
                    exprs: vec![Expression::Compound(
                        "comp".to_string(),
                        vec![ExpressionCompoundArg::Variable("K".to_string())],
                    )],
                },
            ),
        ]);

        let binding = HashMap::new();
        let env = ExecuteEnvironment::new(&binding, question.variables().into_iter());

        let call_result = question.solve(&env);

        if let Err(_) = call_result {
            assert!(false);
        }

        if let Ok(Constraint::EqualTo(Atom::Atom(s))) = env.key_store.get_constraint(&"O".into()) {
            assert_eq!(s, "ok");
        } else {
            assert!(false);
        };
    }

    #[test]
    fn compound_exists() {
        /*
        Question
        - ?comp(comp2(E, 2), 3):=X->E=1->O=ok, X=comp(comp(1, 2), 3).
        */

        let question = create_linear_agent(vec![
            create_ask_agent(
                Box::new(AskTermExists { 
                  exis_term: ExistTerm::Compound("comp".to_string(),
                    vec![
                      ExistTermCompoundArg::Term(
                        ExistTerm::Compound("comp2".to_string(), vec![
                          ExistTermCompoundArg::Term(ExistTerm::Variable("E".to_string())),                           
                          ExistTermCompoundArg::Expression(Expressions { exprs: vec![Expression::Atom(Atom::Number(2.into()))] })
                          ])
                      ),
                      ExistTermCompoundArg::Expression(Expressions { exprs: vec![Expression::Atom(Atom::Number(3.into()))] })
                    ]) ,
                  expr: Expressions { exprs:vec![ Expression::Variable("X".to_string())] }
                }),
                create_ask_agent(
                  Box::new(AskTermVec { 
                    first: Expressions { exprs: vec![Expression::Variable("E".to_string())] }, 
                    remain: vec![(AskTermOp::AEqualB, Expressions{exprs:vec![Expression::Atom(Atom::Number(1.into()))] })]
                  }),
                  create_tell_agent(
                    "O".to_string(),
                    Expressions {
                        exprs: vec![Expression::Atom(Atom::Atom("ok".to_string()))],
                    },
                  )
                ),
            ),
            create_tell_agent(
                "X".to_string(),
                Expressions {
                    exprs: vec![
                      Expression::Atom(Atom::Number(3.into())),
                      Expression::Atom(Atom::Number(2.into())),
                      Expression::Atom(Atom::Number(1.into())),
                      Expression::Compound(
                        "comp2".to_string(),
                        vec![ExpressionCompoundArg::Expression, ExpressionCompoundArg::Expression],
                      ),
                      Expression::Compound(
                        "comp".to_string(),
                        vec![ExpressionCompoundArg::Expression, ExpressionCompoundArg::Expression],
                      )],
                },
            ),
        ]);

        let binding = HashMap::new();
        let env = ExecuteEnvironment::new(&binding, question.global_variables().into_iter());

        let call_result = question.solve(&env);

        if let Err(_) = call_result {
            assert!(false);
        }

        if let Ok(Constraint::EqualTo(Atom::Atom(s))) = env.key_store.get_constraint(&"O".into()) {
            assert_eq!(s, "ok");
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
                    ExecuteEnvironment::new(&behaviors, question.root.variables().into_iter());

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
                    ExecuteEnvironment::new(&behaviors, question.root.variables().into_iter());

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
            + " ? Die(knowledge, !O).";

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
                    ExecuteEnvironment::new(&behaviors, agent.variables().into_iter());

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
                    ExecuteEnvironment::new(&behaviors, agent.variables().into_iter());

                let res = agent.solve(&environment);

                match res {
                    Ok(_) => {
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
    #[test]
    fn compound_exists_test() {
        let code = "?a(a2(E, 2), 3):=X->E=1->O=ok, X=a(a2(1, 2), 3).";
        match compile_one_behavior(&code) {
            Ok((ParseResult::Question(agent), remain)) => {
                assert!(remain.is_empty());

                let behavior = HashMap::new();
                // 実行
                let environment =
                    ExecuteEnvironment::new(&behavior, agent.variables().into_iter());

                let res = agent.solve(&environment);

                match res {
                    Ok(_) => {
                        if let Ok(Constraint::EqualTo(Atom::Atom(s))) =
                            environment.key_store.get_constraint(&"O".into())
                        {
                            assert_eq!(s, "ok");
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
