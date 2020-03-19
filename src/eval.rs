pub mod state;
pub mod val;

pub use state::State;
pub use val::{Number, Value};

use crate::ast::{expr, expr::*, stmt, stmt::*, ExprOrStmt, Program};

pub trait Evaluator<T> {
    fn eval(&mut self, ast: &T) -> Value;
}

impl<V: Evaluator<T>, T> Evaluator<Box<T>> for V {
    fn eval(&mut self, ast: &Box<T>) -> Value {
        self.eval(&**ast)
    }
}

pub trait Eval: Sized {
    fn eval<U>(&self, evaluator: &mut U) -> Value
    where
        U: Evaluator<Self>;
}

impl<V: Eval> Eval for Box<V> {
    fn eval<U>(&self, evaluator: &mut U) -> Value
    where
        U: Evaluator<Self>,
    {
        (&*self).eval(evaluator)
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

impl Evaluator<StmtList> for ObiwanEval {
    fn eval(&mut self, stmts: &StmtList) -> Value {
        for stmt in stmts {
            stmt.eval(self);
        }

        Value::None
    }
}

impl Evaluator<Program> for ObiwanEval {
    fn eval(&mut self, ast: &Program) -> Value {
        match ast {
            Program::Script(stmts) => {
                stmts.eval(self);
            }
            _ => unimplemented!(),
        }

        Value::None
    }
}

impl Evaluator<ExprOrStmt> for ObiwanEval {
    fn eval(&mut self, ast: &ExprOrStmt) -> Value {
        match ast {
            // TODO: we ignore the return value of the expression
            ExprOrStmt::Expr(a) => a.eval(self),
            ExprOrStmt::Stmt(a) => a.eval(self),
        };

        Value::None
    }
}

impl Evaluator<Stmt> for ObiwanEval {
    fn eval(&mut self, ast: &Stmt) -> Value {
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
            Stmt::ClassDeclr(a) => a.eval(self),
            Stmt::FunctionDeclr(a) => a.eval(self),
            Stmt::GeneratorDeclr(a) => a.eval(self),
            Stmt::LexicalDeclr(a) => a.eval(self),
        };

        Value::None
    }
}

impl Evaluator<Block> for ObiwanEval {
    fn eval(&mut self, ast: &Block) -> Value {
        for stmt in &ast.stmts {
            stmt.eval(self);
        }

        Value::None
    }
}

impl Evaluator<Empty> for ObiwanEval {
    fn eval(&mut self, ast: &Empty) -> Value {
        Value::None
    }
}

impl Evaluator<stmt::Expr> for ObiwanEval {
    fn eval(&mut self, ast: &stmt::Expr) -> Value {
        ast.0.eval(self);

        Value::None
    }
}

impl Evaluator<If> for ObiwanEval {
    fn eval(&mut self, ast: &If) -> Value {
        if ast.cond.eval(self).into() {
            ast.body.eval(self);
        } else if let Some(b) = &ast.body_else {
            b.eval(self);
        }

        Value::None
    }
}

impl Evaluator<While> for ObiwanEval {
    fn eval(&mut self, ast: &While) -> Value {
        while ast.cond.eval(self).into() {
            ast.body.eval(self);
        }

        Value::None
    }
}

impl Evaluator<DoWhile> for ObiwanEval {
    fn eval(&mut self, ast: &DoWhile) -> Value {
        ast.body.eval(self);

        while ast.cond.eval(self).into() {
            ast.body.eval(self);
        }

        Value::None
    }
}

impl Evaluator<For> for ObiwanEval {
    fn eval(&mut self, ast: &For) -> Value {
        unimplemented!();
    }
}

impl Evaluator<Switch> for ObiwanEval {
    fn eval(&mut self, ast: &Switch) -> Value {
        unimplemented!();
    }
}

impl Evaluator<Continue> for ObiwanEval {
    fn eval(&mut self, ast: &Continue) -> Value {
        unimplemented!();
    }
}

impl Evaluator<Break> for ObiwanEval {
    fn eval(&mut self, ast: &Break) -> Value {
        unimplemented!();
    }
}

impl Evaluator<Return> for ObiwanEval {
    fn eval(&mut self, ast: &Return) -> Value {
        unimplemented!();
    }
}

impl Evaluator<With> for ObiwanEval {
    fn eval(&mut self, ast: &With) -> Value {
        unimplemented!();
    }
}

impl Evaluator<Labeled> for ObiwanEval {
    fn eval(&mut self, ast: &Labeled) -> Value {
        unimplemented!();
    }
}

impl Evaluator<Throw> for ObiwanEval {
    fn eval(&mut self, ast: &Throw) -> Value {
        unimplemented!();
    }
}

impl Evaluator<Try> for ObiwanEval {
    fn eval(&mut self, ast: &Try) -> Value {
        unimplemented!();
    }
}

impl Evaluator<Debugger> for ObiwanEval {
    fn eval(&mut self, ast: &Debugger) -> Value {
        unimplemented!();
    }
}

impl Evaluator<ClassDeclr> for ObiwanEval {
    fn eval(&mut self, ast: &ClassDeclr) -> Value {
        unimplemented!();
    }
}

impl Evaluator<LexicalDeclr> for ObiwanEval {
    fn eval(&mut self, ast: &LexicalDeclr) -> Value {
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

        Value::None
    }
}

impl Evaluator<FunctionDeclr> for ObiwanEval {
    fn eval(&mut self, ast: &FunctionDeclr) -> Value {
        unimplemented!();
    }
}

impl Evaluator<GeneratorDeclr> for ObiwanEval {
    fn eval(&mut self, ast: &GeneratorDeclr) -> Value {
        unimplemented!();
    }
}

// Expression

impl Evaluator<expr::Expr> for ObiwanEval {
    fn eval(&mut self, ast: &expr::Expr) -> Value {
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
    fn eval(&mut self, ast: &Assign) -> Value {
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
    fn eval(&mut self, ast: &AddAssign) -> Value {
        unimplemented!();
    }
}

impl Evaluator<SubAssign> for ObiwanEval {
    fn eval(&mut self, ast: &SubAssign) -> Value {
        unimplemented!();
    }
}

impl Evaluator<MulAssign> for ObiwanEval {
    fn eval(&mut self, ast: &MulAssign) -> Value {
        unimplemented!();
    }
}

impl Evaluator<DivAssign> for ObiwanEval {
    fn eval(&mut self, ast: &DivAssign) -> Value {
        unimplemented!();
    }
}

impl Evaluator<ModAssign> for ObiwanEval {
    fn eval(&mut self, ast: &ModAssign) -> Value {
        unimplemented!();
    }
}

impl Evaluator<Equal> for ObiwanEval {
    fn eval(&mut self, ast: &Equal) -> Value {
        unimplemented!();
    }
}

impl Evaluator<StrictEq> for ObiwanEval {
    fn eval(&mut self, ast: &StrictEq) -> Value {
        unimplemented!();
    }
}

impl Evaluator<Neq> for ObiwanEval {
    fn eval(&mut self, ast: &Neq) -> Value {
        unimplemented!();
    }
}

impl Evaluator<StrictNeq> for ObiwanEval {
    fn eval(&mut self, ast: &StrictNeq) -> Value {
        unimplemented!();
    }
}

impl Evaluator<InstanceOf> for ObiwanEval {
    fn eval(&mut self, ast: &InstanceOf) -> Value {
        unimplemented!();
    }
}

impl Evaluator<In> for ObiwanEval {
    fn eval(&mut self, ast: &In) -> Value {
        unimplemented!();
    }
}

impl Evaluator<Less> for ObiwanEval {
    fn eval(&mut self, ast: &Less) -> Value {
        let a: Number = ast.0.eval(self).into();
        let b: Number = ast.1.eval(self).into();
        Value::Boolean(a < b)
    }
}

impl Evaluator<Greater> for ObiwanEval {
    fn eval(&mut self, ast: &Greater) -> Value {
        let a: Number = ast.0.eval(self).into();
        let b: Number = ast.1.eval(self).into();
        Value::Boolean(a > b)
    }
}

impl Evaluator<LessEq> for ObiwanEval {
    fn eval(&mut self, ast: &LessEq) -> Value {
        let a: Number = ast.0.eval(self).into();
        let b: Number = ast.1.eval(self).into();
        Value::Boolean(a <= b)
    }
}

impl Evaluator<GreaterEq> for ObiwanEval {
    fn eval(&mut self, ast: &GreaterEq) -> Value {
        let a: Number = ast.0.eval(self).into();
        let b: Number = ast.1.eval(self).into();
        Value::Boolean(a >= b)
    }
}

impl Evaluator<Add> for ObiwanEval {
    fn eval(&mut self, ast: &Add) -> Value {
        let a: Number = ast.0.eval(self).into();
        let b: Number = ast.1.eval(self).into();
        Value::Number(a + b)
    }
}

impl Evaluator<Sub> for ObiwanEval {
    fn eval(&mut self, ast: &Sub) -> Value {
        let a: Number = ast.0.eval(self).into();
        let b: Number = ast.1.eval(self).into();
        Value::Number(a - b)
    }
}

impl Evaluator<Mul> for ObiwanEval {
    fn eval(&mut self, ast: &Mul) -> Value {
        let a: Number = ast.0.eval(self).into();
        let b: Number = ast.1.eval(self).into();
        Value::Number(a * b)
    }
}

impl Evaluator<Div> for ObiwanEval {
    fn eval(&mut self, ast: &Div) -> Value {
        let a: Number = ast.0.eval(self).into();
        let b: Number = ast.1.eval(self).into();
        Value::Number(a / b)
    }
}

impl Evaluator<Mod> for ObiwanEval {
    fn eval(&mut self, ast: &Mod) -> Value {
        let a: Number = ast.0.eval(self).into();
        let b: Number = ast.1.eval(self).into();
        Value::Number(a % b)
    }
}

impl Evaluator<PreIncr> for ObiwanEval {
    fn eval(&mut self, ast: &PreIncr) -> Value {
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
    fn eval(&mut self, ast: &PreDecr) -> Value {
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
    fn eval(&mut self, ast: &PostIncr) -> Value {
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
    fn eval(&mut self, ast: &PostDecr) -> Value {
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
    fn eval(&mut self, ast: &Computed) -> Value {
        unimplemented!();
    }
}

impl Evaluator<Member> for ObiwanEval {
    fn eval(&mut self, ast: &Member) -> Value {
        unimplemented!();
    }
}

impl Evaluator<Call> for ObiwanEval {
    fn eval(&mut self, ast: &Call) -> Value {
        unimplemented!();
    }
}

impl Evaluator<New> for ObiwanEval {
    fn eval(&mut self, ast: &New) -> Value {
        unimplemented!();
    }
}

impl Evaluator<And> for ObiwanEval {
    fn eval(&mut self, ast: &And) -> Value {
        let a: bool = ast.0.eval(self).into();
        let b: bool = ast.1.eval(self).into();
        Value::Boolean(a && b)
    }
}

impl Evaluator<Or> for ObiwanEval {
    fn eval(&mut self, ast: &Or) -> Value {
        let a: bool = ast.0.eval(self).into();
        let b: bool = ast.1.eval(self).into();
        Value::Boolean(a || b)
    }
}

impl Evaluator<Cond> for ObiwanEval {
    fn eval(&mut self, ast: &Cond) -> Value {
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
    fn eval(&mut self, ast: &Not) -> Value {
        let a: bool = ast.0.eval(self).into();
        Value::Boolean(!a)
    }
}

impl Evaluator<Neg> for ObiwanEval {
    fn eval(&mut self, ast: &Neg) -> Value {
        let a: Number = ast.0.eval(self).into();
        Value::Number(-a)
    }
}

impl Evaluator<Positive> for ObiwanEval {
    fn eval(&mut self, ast: &Positive) -> Value {
        let a: Number = ast.0.eval(self).into();
        Value::Number(a)
    }
}

impl Evaluator<Delete> for ObiwanEval {
    fn eval(&mut self, ast: &Delete) -> Value {
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
    fn eval(&mut self, ast: &Void) -> Value {
        Value::Undefined(val::Undefined)
    }
}

impl Evaluator<Typeof> for ObiwanEval {
    fn eval(&mut self, ast: &Typeof) -> Value {
        unimplemented!();
    }
}

impl Evaluator<Literal> for ObiwanEval {
    fn eval(&mut self, ast: &Literal) -> Value {
        match &ast.value {
            LiteralValue::String(s) => Value::String(s.clone()),
            LiteralValue::Number(i) => Value::Number(*i),
            LiteralValue::Boolean(b) => Value::Boolean(*b),
            LiteralValue::Null => Value::Null(val::Null),
        }
    }
}

impl Evaluator<Template> for ObiwanEval {
    fn eval(&mut self, ast: &Template) -> Value {
        unimplemented!();
    }
}

impl Evaluator<Ident> for ObiwanEval {
    fn eval(&mut self, ast: &Ident) -> Value {
        self.state.get(ast)
    }
}

impl Evaluator<This> for ObiwanEval {
    fn eval(&mut self, ast: &This) -> Value {
        unimplemented!();
    }
}

impl Evaluator<Super> for ObiwanEval {
    fn eval(&mut self, ast: &Super) -> Value {
        unimplemented!();
    }
}
