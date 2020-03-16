use crate::error::Error;
use crate::pos::Position;

#[derive(Clone, Eq, Debug)]
pub struct Token {
    pub tt: TokenType,
    pub pos: Option<[Position; 2]>,
    pub literal: Option<String>,
    pub src: Option<String>,
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        self.tt == other.tt && self.literal == other.literal
    }
}

impl Token {
    pub fn new(
        tt: TokenType,
        pos: Option<[Position; 2]>,
        src: Option<&str>,
        literal: Option<&str>,
    ) -> Self {
        Token {
            tt: tt,
            pos: pos,
            src: src.map(String::from),
            literal: literal.map(String::from),
        }
        .into_reserve()
    }

    pub fn new_lit(tt: TokenType, s: &str) -> Self {
        Self::new(tt, None, None, Some(s))
    }

    pub fn from_str(s: &str) -> Result<Self, Error> {
        Ok(Self::new(TokenType::from_str(s)?, None, None, None))
    }

    pub fn from_vec(vec: Vec<&str>) -> Result<Vec<Self>, Error> {
        let mut ret = vec![];

        for s in vec {
            ret.push(Self::from_str(s)?)
        }

        Ok(ret)
    }

    // Check is the token exactly the same
    pub fn is_same(&self, other: &Self) -> bool {
        self == other && self.pos == other.pos && self.src == other.src
    }

    // Turn into reserve type
    fn into_reserve(mut self) -> Self {
        match self.tt {
            TokenType::Identifier => match TokenType::from_str(self.src.as_ref().unwrap()) {
                Ok(tt) => {
                    self.tt = tt;
                    self
                }
                _ => self,
            },
            _ => self,
        }
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum TokenType {
    // reserved words
    Identifier,
    Break,
    Do,
    In,
    Typeof,
    Case,
    Else,
    Instanceof,
    Var,
    Catch,
    Export,
    New,
    Void,
    Class,
    Extends,
    Return,
    While,
    Const,
    Finally,
    Super,
    With,
    Continue,
    For,
    Switch,
    Yield,
    Debugger,
    Function,
    This,
    Default,
    If,
    Throw,
    Delete,
    Import,
    Try,
    // Boolean lit and null lit
    False,
    True,
    Null,
    // Future
    Enum,
    Await,

    // Punctuator
    LBrace,
    RBrace,
    LParen,
    RParen,
    LBracket,
    RBracket,
    Dot,
    TriDot,
    Semicolon,
    Coma,
    Less,
    LessEq,
    Greater,
    GreaterEq,
    DualEq,
    BangEq,
    TriEq,
    BangDualEq,
    Plus,
    Minus,
    Star,
    Percent,
    DualPlus,
    DualMinus,
    DualGreater,
    DualLess,
    TriGreater,
    Amp,
    Bar,
    Caret,
    Bang,
    Tide,
    DualAmp,
    DualBar,
    Question,
    Colon,
    Eq,
    PlusEq,
    MinusEq,
    StarEq,
    PercentEq,
    DualLessEq,
    DualGreaterEq,
    TriGreaterEq,
    AmpEq,
    BarEq,
    CaretEq,
    Arrow,
    Slash,
    SlashEq,

    // Literals
    String,
    Template,
    Decimal,

    // TODO: support these
    Binary,
    Hex,
    Octal,
}

impl TokenType {
    pub fn from_bytes(b: &[u8]) -> Result<Self, Error> {
        match std::str::from_utf8(b) {
            Ok(s) => Self::from_str(s),
            Err(_) => Err(Error::TokenConvertError),
        }
    }

    pub fn from_str(s: &str) -> Result<Self, Error> {
        match s {
            "break" => Ok(TokenType::Break),
            "do" => Ok(TokenType::Do),
            "in" => Ok(TokenType::In),
            "typeof" => Ok(TokenType::Typeof),
            "case" => Ok(TokenType::Case),
            "Else" => Ok(TokenType::Else),
            "Instanceof" => Ok(TokenType::Instanceof),
            "var" => Ok(TokenType::Var),
            "catch" => Ok(TokenType::Catch),
            "export" => Ok(TokenType::Export),
            "new" => Ok(TokenType::New),
            "void" => Ok(TokenType::Void),
            "class" => Ok(TokenType::Class),
            "extends" => Ok(TokenType::Extends),
            "return" => Ok(TokenType::Return),
            "while" => Ok(TokenType::While),
            "const" => Ok(TokenType::Const),
            "finally" => Ok(TokenType::Finally),
            "super" => Ok(TokenType::Super),
            "with" => Ok(TokenType::With),
            "continue" => Ok(TokenType::Continue),
            "for" => Ok(TokenType::For),
            "switch" => Ok(TokenType::Switch),
            "yield" => Ok(TokenType::Yield),
            "debugger" => Ok(TokenType::Debugger),
            "function" => Ok(TokenType::Function),
            "this" => Ok(TokenType::This),
            "default" => Ok(TokenType::Default),
            "if" => Ok(TokenType::If),
            "delete" => Ok(TokenType::Delete),
            "import" => Ok(TokenType::Import),
            "try" => Ok(TokenType::Try),
            "false" => Ok(TokenType::False),
            "true" => Ok(TokenType::True),
            "null" => Ok(TokenType::Null),
            "enum" => Ok(TokenType::Enum),
            "await" => Ok(TokenType::Await),
            // Punctuator
            "{" => Ok(TokenType::LBrace),
            "}" => Ok(TokenType::RBrace),
            "(" => Ok(TokenType::LParen),
            ")" => Ok(TokenType::RParen),
            "[" => Ok(TokenType::LBracket),
            "]" => Ok(TokenType::RBracket),
            "." => Ok(TokenType::Dot),
            "..." => Ok(TokenType::TriDot),
            ";" => Ok(TokenType::Semicolon),
            "," => Ok(TokenType::Coma),
            "<" => Ok(TokenType::Less),
            "<=" => Ok(TokenType::LessEq),
            ">" => Ok(TokenType::Greater),
            ">=" => Ok(TokenType::GreaterEq),
            "==" => Ok(TokenType::DualEq),
            "!=" => Ok(TokenType::BangEq),
            "===" => Ok(TokenType::TriEq),
            "!==" => Ok(TokenType::BangDualEq),
            "+" => Ok(TokenType::Plus),
            "-" => Ok(TokenType::Minus),
            "*" => Ok(TokenType::Star),
            "%" => Ok(TokenType::Percent),
            "++" => Ok(TokenType::DualPlus),
            "--" => Ok(TokenType::DualMinus),
            ">>" => Ok(TokenType::DualGreater),
            "<<" => Ok(TokenType::DualLess),
            ">>>" => Ok(TokenType::TriGreater),
            "&" => Ok(TokenType::Amp),
            "|" => Ok(TokenType::Bar),
            "^" => Ok(TokenType::Caret),
            "!" => Ok(TokenType::Bang),
            "~" => Ok(TokenType::Tide),
            "&&" => Ok(TokenType::DualAmp),
            "||" => Ok(TokenType::DualBar),
            "?" => Ok(TokenType::Question),
            ":" => Ok(TokenType::Colon),
            "=" => Ok(TokenType::Eq),
            "+=" => Ok(TokenType::PlusEq),
            "-=" => Ok(TokenType::MinusEq),
            "*=" => Ok(TokenType::StarEq),
            "%=" => Ok(TokenType::PercentEq),
            "<<=" => Ok(TokenType::DualLessEq),
            ">>=" => Ok(TokenType::DualGreaterEq),
            ">>>=" => Ok(TokenType::TriGreaterEq),
            "&=" => Ok(TokenType::AmpEq),
            "|=" => Ok(TokenType::BarEq),
            "^=" => Ok(TokenType::CaretEq),
            "=>" => Ok(TokenType::Arrow),
            "/" => Ok(TokenType::Slash),
            "/=" => Ok(TokenType::SlashEq),
            _ => Err(Error::TokenConvertError),
        }
    }
}
