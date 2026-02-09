use crate::source::Span;

#[derive(Clone, Debug, PartialEq)]
pub struct Program {
    pub items: Vec<Item>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Item {
    Function(FnItem),
    Struct(StructItem),
    Enum(EnumItem),
    Interface(InterfaceItem),
    Impl(ImplItem),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Ident {
    pub name: String,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Path {
    pub segments: Vec<Ident>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnItem {
    pub name: Ident,
    pub generics: Vec<GenericParam>,
    pub params: Vec<Param>,
    pub ret: TypeExpr,
    pub body: Block,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Param {
    pub name: Ident,
    pub ty: TypeExpr,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct StructItem {
    pub name: Ident,
    pub generics: Vec<GenericParam>,
    pub fields: Vec<FieldDecl>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FieldDecl {
    pub name: Ident,
    pub ty: TypeExpr,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct EnumItem {
    pub name: Ident,
    pub generics: Vec<GenericParam>,
    pub variants: Vec<EnumVariant>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct EnumVariant {
    pub name: Ident,
    pub fields: Vec<TypeExpr>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct InterfaceItem {
    pub name: Ident,
    pub generics: Vec<GenericParam>,
    pub members: Vec<InterfaceMember>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct InterfaceMember {
    pub name: Ident,
    pub generics: Vec<GenericParam>,
    pub params: Vec<Param>,
    pub ret: TypeExpr,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ImplItem {
    pub generics: Vec<GenericParam>,
    pub header: ImplHeader,
    pub members: Vec<FnItem>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ImplHeader {
    Inherent {
        ty: PathType,
        span: Span,
    },
    InterfaceForType {
        interface: PathType,
        ty: PathType,
        span: Span,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct GenericParam {
    pub name: Ident,
    /// The arity of this type parameter.
    ///
    /// - `0` means a regular type parameter (`T : Type`)
    /// - `n>0` means a type constructor parameter of kind `Type^n -> Type` (`F<_, ...>`)
    pub arity: usize,
    /// Optional interface constraint (`T: Logger`).
    ///
    /// Constraints are currently limited to a single interface path.
    pub constraint: Option<PathType>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeExpr {
    Readonly {
        inner: Box<TypeExpr>,
        span: Span,
    },
    Prim {
        prim: PrimType,
        span: Span,
    },
    Array {
        elem: Box<TypeExpr>,
        span: Span,
    },
    Fn {
        params: Vec<TypeExpr>,
        ret: Box<TypeExpr>,
        span: Span,
    },
    Path(PathType),
}

impl TypeExpr {
    pub fn span(&self) -> Span {
        match self {
            TypeExpr::Readonly { span, .. }
            | TypeExpr::Prim { span, .. }
            | TypeExpr::Array { span, .. }
            | TypeExpr::Fn { span, .. } => *span,
            TypeExpr::Path(p) => p.span,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PrimType {
    Unit,
    Bool,
    Int,
    Float,
    String,
    Bytes,
}

#[derive(Clone, Debug, PartialEq)]
pub struct PathType {
    pub segments: Vec<PathTypeSegment>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct PathTypeSegment {
    pub name: Ident,
    pub args: Vec<TypeExpr>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub tail: Option<Box<Expr>>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Let {
        kind: BindingKind,
        name: Ident,
        ty: Option<TypeExpr>,
        init: Option<Expr>,
        span: Span,
    },
    Return {
        value: Option<Expr>,
        span: Span,
    },
    Break {
        span: Span,
    },
    Continue {
        span: Span,
    },
    Expr {
        expr: Expr,
        span: Span,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BindingKind {
    Let,
    Const,
    Readonly,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Unit {
        span: Span,
    },
    Bool {
        value: bool,
        span: Span,
    },
    Int {
        value: i64,
        span: Span,
    },
    Float {
        value: f64,
        span: Span,
    },
    String {
        value: String,
        span: Span,
    },
    Bytes {
        value: Vec<u8>,
        span: Span,
    },

    Path {
        path: Path,
        span: Span,
    },
    Array {
        items: Vec<Expr>,
        span: Span,
    },
    StructLit {
        type_name: Ident,
        fields: Vec<(Ident, Expr)>,
        span: Span,
    },
    EffectCall {
        interface: Path,
        method: Ident,
        args: Vec<Expr>,
        span: Span,
    },

    Lambda {
        params: Vec<LambdaParam>,
        body: Block,
        span: Span,
    },
    If {
        cond: Box<Expr>,
        then_block: Block,
        else_branch: Option<Box<Expr>>,
        span: Span,
    },
    Match {
        scrutinee: Box<Expr>,
        arms: Vec<MatchArm>,
        span: Span,
    },
    Loop {
        body: Block,
        span: Span,
    },
    While {
        cond: Box<Expr>,
        body: Block,
        span: Span,
    },
    For {
        binding: Ident,
        iter: Box<Expr>,
        body: Block,
        span: Span,
    },
    Block {
        block: Block,
        span: Span,
    },

    Call {
        callee: Box<Expr>,
        args: Vec<Expr>,
        span: Span,
    },
    Field {
        base: Box<Expr>,
        name: Ident,
        span: Span,
    },
    Index {
        base: Box<Expr>,
        index: Box<Expr>,
        span: Span,
    },
    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
        span: Span,
    },
    Binary {
        op: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>,
        span: Span,
    },
    Assign {
        target: Box<Expr>,
        value: Box<Expr>,
        span: Span,
    },
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Unit { span }
            | Expr::Bool { span, .. }
            | Expr::Int { span, .. }
            | Expr::Float { span, .. }
            | Expr::String { span, .. }
            | Expr::Bytes { span, .. }
            | Expr::Path { span, .. }
            | Expr::Array { span, .. }
            | Expr::StructLit { span, .. }
            | Expr::EffectCall { span, .. }
            | Expr::Lambda { span, .. }
            | Expr::If { span, .. }
            | Expr::Match { span, .. }
            | Expr::Loop { span, .. }
            | Expr::While { span, .. }
            | Expr::For { span, .. }
            | Expr::Block { span, .. }
            | Expr::Call { span, .. }
            | Expr::Field { span, .. }
            | Expr::Index { span, .. }
            | Expr::Unary { span, .. }
            | Expr::Binary { span, .. }
            | Expr::Assign { span, .. } => *span,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnaryOp {
    Not,
    Neg,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    And,
    Or,
}

#[derive(Clone, Debug, PartialEq)]
pub struct LambdaParam {
    pub name: Ident,
    pub ty: Option<TypeExpr>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct MatchArm {
    pub pat: MatchPat,
    pub body: Expr,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub enum MatchPat {
    Value(Pattern),
    Effect(EffectPattern),
}

#[derive(Clone, Debug, PartialEq)]
pub struct EffectPattern {
    pub interface: Path,
    pub method: Ident,
    pub args: Vec<Pattern>,
    /// Optional binder name for the captured continuation (effect handler context).
    ///
    /// If absent, the continuation is bound to the predefined name `resume` for
    /// backwards compatibility.
    pub cont: Option<Ident>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Pattern {
    Wildcard {
        span: Span,
    },
    Bind {
        name: Ident,
        span: Span,
    },
    Literal {
        lit: PatLiteral,
        span: Span,
    },
    Enum {
        enum_name: Ident,
        variant: Ident,
        fields: Vec<Pattern>,
        span: Span,
    },
    Struct {
        type_name: Ident,
        fields: Vec<(Ident, Pattern)>,
        span: Span,
    },
    ArrayPrefix {
        items: Vec<Pattern>,
        has_rest: bool,
        span: Span,
    },
}

impl Pattern {
    pub fn span(&self) -> Span {
        match self {
            Pattern::Wildcard { span }
            | Pattern::Bind { span, .. }
            | Pattern::Literal { span, .. }
            | Pattern::Enum { span, .. }
            | Pattern::Struct { span, .. }
            | Pattern::ArrayPrefix { span, .. } => *span,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum PatLiteral {
    Unit,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
    Bytes(Vec<u8>),
}
