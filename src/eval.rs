pub mod state;
pub mod val;

pub use state::State;
pub use val::{Number, Value};

use crate::ast::{expr, expr::*, stmt, stmt::*, Program};

pub trait Evaluator<T> {
    type Output;
    fn eval(&mut self, ast: &T) -> Self::Output;
}

impl<V: Evaluator<T>, T> Evaluator<Box<T>> for V {
    type Output = V::Output;
    fn eval(&mut self, ast: &Box<T>) -> Self::Output {
        self.eval(&**ast)
    }
}

pub trait Eval<T>: Sized {
    fn eval<U>(&self, evaluator: &mut U) -> T
    where
        U: Evaluator<Self, Output = T>,
    {
        evaluator.eval(self)
    }
}

impl<V: Eval<T>, T> Eval<T> for Box<V> {
    fn eval<U>(&self, evaluator: &mut U) -> T
    where
        U: Evaluator<Self, Output = T>,
    {
        evaluator.eval(self)
    }
}

#[derive(Debug)]
pub struct ObiwanEval {
    pub state: State,
    max_step: u32,
}

impl ObiwanEval {
    pub fn new() -> Self {
        Self {
            state: State::new(),
            max_step: 10000,
        }
    }
}

impl Evaluator<Program> for ObiwanEval {
    type Output = ();
    fn eval(&mut self, ast: &Program) -> Self::Output {
        match ast {
            Program::Script(stmts) => {
                for stmt in stmts {
                    stmt.eval(self);
                }
            }
            _ => unimplemented!(),
        }
    }
}

impl Evaluator<Stmt> for ObiwanEval {
    type Output = ();
    fn eval(&mut self, ast: &Stmt) -> Self::Output {
        match ast {
            Stmt::Block(a) => a.eval(self),
            Stmt::Empty(a) => a.eval(self),
            Stmt::Expr(a) => a.eval(self),
            Stmt::If(a) => a.eval(self),
            Stmt::While(a) => a.eval(self),
            Stmt::DoWhile(a) => a.eval(self),
            Stmt::For(a) => a.eval(self),
            Stmt::Switch(a) => a.eval(self),
            Stmt::Continue(a) => a.eval(self),
            Stmt::Break(a) => a.eval(self),
            Stmt::Return(a) => a.eval(self),
            Stmt::With(a) => a.eval(self),
            Stmt::Labeled(a) => a.eval(self),
            Stmt::Throw(a) => a.eval(self),
            Stmt::Try(a) => a.eval(self),
            Stmt::Debugger(a) => a.eval(self),
            Stmt::Var(a) => a.eval(self),
            Stmt::ClassDeclr(a) => a.eval(self),
            Stmt::FunctionDeclr(a) => a.eval(self),
            Stmt::GeneratorDeclr(a) => a.eval(self),
            Stmt::LexicalDeclr(a) => a.eval(self),
        }
    }
}

impl Evaluator<Block> for ObiwanEval {
    type Output = ();
    fn eval(&mut self, ast: &Block) -> Self::Output {
        for stmt in &ast.stmts {
            stmt.eval(self);
        }
    }
}

impl Evaluator<Empty> for ObiwanEval {
    type Output = ();
    fn eval(&mut self, ast: &Empty) -> Self::Output {}
}

impl Evaluator<stmt::Expr> for ObiwanEval {
    type Output = ();
    fn eval(&mut self, ast: &stmt::Expr) -> Self::Output {
        ast.0.eval(self);
    }
}

impl Evaluator<If> for ObiwanEval {
    type Output = ();
    fn eval(&mut self, ast: &If) -> Self::Output {
        if ast.cond.eval(self).into() {
            ast.body.eval(self);
        } else if let Some(b) = &ast.body_else {
            b.eval(self);
        }
    }
}

impl Evaluator<While> for ObiwanEval {
    type Output = ();
    fn eval(&mut self, ast: &While) -> Self::Output {
        while ast.cond.eval(self).into() {
            ast.body.eval(self);
        }
    }
}

impl Evaluator<DoWhile> for ObiwanEval {
    type Output = ();
    fn eval(&mut self, ast: &DoWhile) -> Self::Output {
        ast.body.eval(self);

        while ast.cond.eval(self).into() {
            ast.body.eval(self);
        }
    }
}

impl Evaluator<For> for ObiwanEval {
    type Output = ();
    fn eval(&mut self, ast: &For) -> Self::Output {
        unimplemented!();
    }
}

impl Evaluator<Switch> for ObiwanEval {
    type Output = ();
    fn eval(&mut self, ast: &Switch) -> Self::Output {
        unimplemented!();
    }
}

impl Evaluator<Continue> for ObiwanEval {
    type Output = ();
    fn eval(&mut self, ast: &Continue) -> Self::Output {
        unimplemented!();
    }
}

impl Evaluator<Break> for ObiwanEval {
    type Output = ();
    fn eval(&mut self, ast: &Break) -> Self::Output {
        unimplemented!();
    }
}

impl Evaluator<Return> for ObiwanEval {
    type Output = ();
    fn eval(&mut self, ast: &Return) -> Self::Output {
        unimplemented!();
    }
}

impl Evaluator<With> for ObiwanEval {
    type Output = ();
    fn eval(&mut self, ast: &With) -> Self::Output {
        unimplemented!();
    }
}

impl Evaluator<Labeled> for ObiwanEval {
    type Output = ();
    fn eval(&mut self, ast: &Labeled) -> Self::Output {
        unimplemented!();
    }
}

impl Evaluator<Throw> for ObiwanEval {
    type Output = ();
    fn eval(&mut self, ast: &Throw) -> Self::Output {
        unimplemented!();
    }
}

impl Evaluator<Try> for ObiwanEval {
    type Output = ();
    fn eval(&mut self, ast: &Try) -> Self::Output {
        unimplemented!();
    }
}

impl Evaluator<Debugger> for ObiwanEval {
    type Output = ();
    fn eval(&mut self, ast: &Debugger) -> Self::Output {
        unimplemented!();
    }
}

impl Evaluator<Var> for ObiwanEval {
    type Output = ();
    fn eval(&mut self, ast: &Var) -> Self::Output {
        unimplemented!();
    }
}

impl Evaluator<ClassDeclr> for ObiwanEval {
    type Output = ();
    fn eval(&mut self, ast: &ClassDeclr) -> Self::Output {
        unimplemented!();
    }
}

impl Evaluator<LexicalDeclr> for ObiwanEval {
    type Output = ();
    fn eval(&mut self, ast: &LexicalDeclr) -> Self::Output {
        for VariableDeclarator { ident, init } in &ast.declarations {
            if let expr::Expr::Ident(id) = ident {
                if let Some(v) = init {
                    let next = v.eval(self);
                    self.state.update(id, next);
                } else {
                    self.state.update(id, Value::Undefined(val::Undefined))
                }
            } else {
                panic!("Delcartion should have identifier as LHS")
            }
        }
    }
}

impl Evaluator<FunctionDeclr> for ObiwanEval {
    type Output = ();
    fn eval(&mut self, ast: &FunctionDeclr) -> Self::Output {
        unimplemented!();
    }
}

impl Evaluator<GeneratorDeclr> for ObiwanEval {
    type Output = ();
    fn eval(&mut self, ast: &GeneratorDeclr) -> Self::Output {
        unimplemented!();
    }
}

// Expression

impl Evaluator<expr::Expr> for ObiwanEval {
    type Output = Value;
    fn eval(&mut self, ast: &expr::Expr) -> Self::Output {
        match ast {
            expr::Expr::Assign(e) => e.eval(self),
            expr::Expr::AddAssign(e) => e.eval(self),
            expr::Expr::SubAssign(e) => e.eval(self),
            expr::Expr::MulAssign(e) => e.eval(self),
            expr::Expr::DivAssign(e) => e.eval(self),
            expr::Expr::ModAssign(e) => e.eval(self),

            expr::Expr::Equal(e) => e.eval(self),
            expr::Expr::StrictEq(e) => e.eval(self),
            expr::Expr::Neq(e) => e.eval(self),
            expr::Expr::StrictNeq(e) => e.eval(self),

            expr::Expr::InstanceOf(e) => e.eval(self),
            expr::Expr::In(e) => e.eval(self),
            expr::Expr::Less(e) => e.eval(self),
            expr::Expr::Greater(e) => e.eval(self),
            expr::Expr::LessEq(e) => e.eval(self),
            expr::Expr::GreaterEq(e) => e.eval(self),

            expr::Expr::Add(e) => e.eval(self),
            expr::Expr::Sub(e) => e.eval(self),
            expr::Expr::Mul(e) => e.eval(self),
            expr::Expr::Div(e) => e.eval(self),
            expr::Expr::Mod(e) => e.eval(self),

            expr::Expr::PreIncr(e) => e.eval(self),
            expr::Expr::PreDecr(e) => e.eval(self),

            expr::Expr::PostIncr(e) => e.eval(self),
            expr::Expr::PostDecr(e) => e.eval(self),

            expr::Expr::Computed(e) => e.eval(self),
            expr::Expr::Member(e) => e.eval(self),
            expr::Expr::Call(e) => e.eval(self),
            expr::Expr::New(e) => e.eval(self),

            expr::Expr::And(e) => e.eval(self),
            expr::Expr::Or(e) => e.eval(self),
            expr::Expr::Cond(e) => e.eval(self),

            expr::Expr::Not(e) => e.eval(self),
            expr::Expr::Neg(e) => e.eval(self),
            expr::Expr::Positive(e) => e.eval(self),
            expr::Expr::Delete(e) => e.eval(self),
            expr::Expr::Void(e) => e.eval(self),
            expr::Expr::Typeof(e) => e.eval(self),

            // primary values
            // literals
            expr::Expr::Literal(e) => e.eval(self),

            expr::Expr::Template(e) => e.eval(self),

            expr::Expr::Ident(e) => e.eval(self),

            expr::Expr::This(e) => e.eval(self),

            expr::Expr::Super(e) => e.eval(self),
        }
    }
}

impl Evaluator<Assign> for ObiwanEval {
    type Output = Value;
    fn eval(&mut self, ast: &Assign) -> Self::Output {
        // TODO: support all the LHS assign
        if let expr::Expr::Ident(id) = &*ast.0 {
            if self.state.is_declared(id) {
                let value = ast.1.eval(self);
                self.state.update(id, value.clone());
                value
            } else {
                panic!("Can't assign to undeclared values")
            }
        } else {
            panic!("expect Identifier as LHS")
        }
    }
}

impl Evaluator<AddAssign> for ObiwanEval {
    type Output = Value;
    fn eval(&mut self, ast: &AddAssign) -> Self::Output {
        unimplemented!();
    }
}

impl Evaluator<SubAssign> for ObiwanEval {
    type Output = Value;
    fn eval(&mut self, ast: &SubAssign) -> Self::Output {
        unimplemented!();
    }
}

impl Evaluator<MulAssign> for ObiwanEval {
    type Output = Value;
    fn eval(&mut self, ast: &MulAssign) -> Self::Output {
        unimplemented!();
    }
}

impl Evaluator<DivAssign> for ObiwanEval {
    type Output = Value;
    fn eval(&mut self, ast: &DivAssign) -> Self::Output {
        unimplemented!();
    }
}

impl Evaluator<ModAssign> for ObiwanEval {
    type Output = Value;
    fn eval(&mut self, ast: &ModAssign) -> Self::Output {
        unimplemented!();
    }
}

impl Evaluator<Equal> for ObiwanEval {
    type Output = Value;
    fn eval(&mut self, ast: &Equal) -> Self::Output {
        unimplemented!();
    }
}

impl Evaluator<StrictEq> for ObiwanEval {
    type Output = Value;
    fn eval(&mut self, ast: &StrictEq) -> Self::Output {
        unimplemented!();
    }
}

impl Evaluator<Neq> for ObiwanEval {
    type Output = Value;
    fn eval(&mut self, ast: &Neq) -> Self::Output {
        unimplemented!();
    }
}

impl Evaluator<StrictNeq> for ObiwanEval {
    type Output = Value;
    fn eval(&mut self, ast: &StrictNeq) -> Self::Output {
        unimplemented!();
    }
}

impl Evaluator<InstanceOf> for ObiwanEval {
    type Output = Value;
    fn eval(&mut self, ast: &InstanceOf) -> Self::Output {
        unimplemented!();
    }
}

impl Evaluator<In> for ObiwanEval {
    type Output = Value;
    fn eval(&mut self, ast: &In) -> Self::Output {
        unimplemented!();
    }
}

impl Evaluator<Less> for ObiwanEval {
    type Output = Value;
    fn eval(&mut self, ast: &Less) -> Self::Output {
        let a: Number = ast.0.eval(self).into();
        let b: Number = ast.1.eval(self).into();
        Value::Boolean(a < b)
    }
}

impl Evaluator<Greater> for ObiwanEval {
    type Output = Value;
    fn eval(&mut self, ast: &Greater) -> Self::Output {
        let a: Number = ast.0.eval(self).into();
        let b: Number = ast.1.eval(self).into();
        Value::Boolean(a > b)
    }
}

impl Evaluator<LessEq> for ObiwanEval {
    type Output = Value;
    fn eval(&mut self, ast: &LessEq) -> Self::Output {
        let a: Number = ast.0.eval(self).into();
        let b: Number = ast.1.eval(self).into();
        Value::Boolean(a <= b)
    }
}

impl Evaluator<GreaterEq> for ObiwanEval {
    type Output = Value;
    fn eval(&mut self, ast: &GreaterEq) -> Self::Output {
        let a: Number = ast.0.eval(self).into();
        let b: Number = ast.1.eval(self).into();
        Value::Boolean(a >= b)
    }
}

impl Evaluator<Add> for ObiwanEval {
    type Output = Value;
    fn eval(&mut self, ast: &Add) -> Self::Output {
        let a: Number = ast.0.eval(self).into();
        let b: Number = ast.1.eval(self).into();
        Value::Number(a + b)
    }
}

impl Evaluator<Sub> for ObiwanEval {
    type Output = Value;
    fn eval(&mut self, ast: &Sub) -> Self::Output {
        let a: Number = ast.0.eval(self).into();
        let b: Number = ast.1.eval(self).into();
        Value::Number(a - b)
    }
}

impl Evaluator<Mul> for ObiwanEval {
    type Output = Value;
    fn eval(&mut self, ast: &Mul) -> Self::Output {
        let a: Number = ast.0.eval(self).into();
        let b: Number = ast.1.eval(self).into();
        Value::Number(a * b)
    }
}

impl Evaluator<Div> for ObiwanEval {
    type Output = Value;
    fn eval(&mut self, ast: &Div) -> Self::Output {
        let a: Number = ast.0.eval(self).into();
        let b: Number = ast.1.eval(self).into();
        Value::Number(a / b)
    }
}

impl Evaluator<Mod> for ObiwanEval {
    type Output = Value;
    fn eval(&mut self, ast: &Mod) -> Self::Output {
        let a: Number = ast.0.eval(self).into();
        let b: Number = ast.1.eval(self).into();
        Value::Number(a % b)
    }
}

impl Evaluator<PreIncr> for ObiwanEval {
    type Output = Value;
    fn eval(&mut self, ast: &PreIncr) -> Self::Output {
        let a: Number = ast.0.eval(self).into();
        let new_val = Value::Number(a + 1);
        if let expr::Expr::Ident(id) = &*ast.0 {
            self.state.update(&id, new_val.clone())
        } else {
            panic!("expected identifier");
        }

        new_val
    }
}

impl Evaluator<PreDecr> for ObiwanEval {
    type Output = Value;
    fn eval(&mut self, ast: &PreDecr) -> Self::Output {
        let a: Number = ast.0.eval(self).into();
        let new_val = Value::Number(a - 1);
        if let expr::Expr::Ident(id) = &*ast.0 {
            self.state.update(&id, new_val.clone())
        } else {
            panic!("expected identifier");
        }

        new_val
    }
}

impl Evaluator<PostIncr> for ObiwanEval {
    type Output = Value;
    fn eval(&mut self, ast: &PostIncr) -> Self::Output {
        let a: Number = ast.0.eval(self).into();
        let new_val = Value::Number(a + 1);
        if let expr::Expr::Ident(id) = &*ast.0 {
            self.state.update(&id, new_val.clone())
        } else {
            panic!("expected identifier");
        }

        Value::Number(a)
    }
}

impl Evaluator<PostDecr> for ObiwanEval {
    type Output = Value;
    fn eval(&mut self, ast: &PostDecr) -> Self::Output {
        let a: Number = ast.0.eval(self).into();
        let new_val = Value::Number(a - 1);
        if let expr::Expr::Ident(id) = &*ast.0 {
            self.state.update(&id, new_val.clone())
        } else {
            panic!("expected identifier");
        }

        Value::Number(a)
    }
}

impl Evaluator<Computed> for ObiwanEval {
    type Output = Value;
    fn eval(&mut self, ast: &Computed) -> Self::Output {
        unimplemented!();
    }
}

impl Evaluator<Member> for ObiwanEval {
    type Output = Value;
    fn eval(&mut self, ast: &Member) -> Self::Output {
        unimplemented!();
    }
}

impl Evaluator<Call> for ObiwanEval {
    type Output = Value;
    fn eval(&mut self, ast: &Call) -> Self::Output {
        unimplemented!();
    }
}

impl Evaluator<New> for ObiwanEval {
    type Output = Value;
    fn eval(&mut self, ast: &New) -> Self::Output {
        unimplemented!();
    }
}

impl Evaluator<And> for ObiwanEval {
    type Output = Value;
    fn eval(&mut self, ast: &And) -> Self::Output {
        let a: bool = ast.0.eval(self).into();
        let b: bool = ast.1.eval(self).into();
        Value::Boolean(a && b)
    }
}

impl Evaluator<Or> for ObiwanEval {
    type Output = Value;
    fn eval(&mut self, ast: &Or) -> Self::Output {
        let a: bool = ast.0.eval(self).into();
        let b: bool = ast.1.eval(self).into();
        Value::Boolean(a || b)
    }
}

impl Evaluator<Cond> for ObiwanEval {
    type Output = Value;
    fn eval(&mut self, ast: &Cond) -> Self::Output {
        let a: bool = ast.0.eval(self).into();
        let b = ast.1.eval(self);
        let c = ast.2.eval(self);

        if a {
            b
        } else {
            c
        }
    }
}

impl Evaluator<Not> for ObiwanEval {
    type Output = Value;
    fn eval(&mut self, ast: &Not) -> Self::Output {
        let a: bool = ast.0.eval(self).into();
        Value::Boolean(!a)
    }
}

impl Evaluator<Neg> for ObiwanEval {
    type Output = Value;
    fn eval(&mut self, ast: &Neg) -> Self::Output {
        let a: Number = ast.0.eval(self).into();
        Value::Number(-a)
    }
}

impl Evaluator<Positive> for ObiwanEval {
    type Output = Value;
    fn eval(&mut self, ast: &Positive) -> Self::Output {
        let a: Number = ast.0.eval(self).into();
        Value::Number(a)
    }
}

impl Evaluator<Delete> for ObiwanEval {
    type Output = Value;
    fn eval(&mut self, ast: &Delete) -> Self::Output {
        if let expr::Expr::Ident(id) = &*ast.0 {
            self.state.remove(&id);
        } else {
            panic!("expected identifier");
        }

        // FIXME: what should be returned?
        Value::Boolean(false)
    }
}

impl Evaluator<Void> for ObiwanEval {
    type Output = Value;
    fn eval(&mut self, ast: &Void) -> Self::Output {
        Value::Undefined(val::Undefined)
    }
}

impl Evaluator<Typeof> for ObiwanEval {
    type Output = Value;
    fn eval(&mut self, ast: &Typeof) -> Self::Output {
        unimplemented!();
    }
}

impl Evaluator<Literal> for ObiwanEval {
    type Output = Value;
    fn eval(&mut self, ast: &Literal) -> Self::Output {
        match &ast.value {
            LiteralValue::String(s) => Value::String(s.clone()),
            LiteralValue::Number(i) => Value::Number(*i),
            LiteralValue::Boolean(b) => Value::Boolean(*b),
            LiteralValue::Null => Value::Null(val::Null),
        }
    }
}

impl Evaluator<Template> for ObiwanEval {
    type Output = Value;
    fn eval(&mut self, ast: &Template) -> Self::Output {
        unimplemented!();
    }
}

impl Evaluator<Ident> for ObiwanEval {
    type Output = Value;
    fn eval(&mut self, ast: &Ident) -> Self::Output {
        self.state.get(ast)
    }
}

impl Evaluator<This> for ObiwanEval {
    type Output = Value;
    fn eval(&mut self, ast: &This) -> Self::Output {
        unimplemented!();
    }
}

impl Evaluator<Super> for ObiwanEval {
    type Output = Value;
    fn eval(&mut self, ast: &Super) -> Self::Output {
        unimplemented!();
    }
}
