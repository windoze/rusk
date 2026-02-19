use crate::source::Span;

#[derive(Clone, Debug, PartialEq)]
pub struct Program {
    pub items: Vec<Item>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Visibility {
    Private,
    Public { span: Span },
}

impl Visibility {
    pub fn is_public(&self) -> bool {
        matches!(self, Visibility::Public { .. })
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Item {
    Function(FnItem),
    IntrinsicFn(IntrinsicFnItem),
    Struct(StructItem),
    Enum(EnumItem),
    Interface(InterfaceItem),
    Impl(ImplItem),
    Mod(ModItem),
    Use(UseItem),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Ident {
    pub name: String,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub enum FieldName {
    Named(Ident),
    Index { index: usize, span: Span },
}

impl FieldName {
    pub fn span(&self) -> Span {
        match self {
            FieldName::Named(ident) => ident.span,
            FieldName::Index { span, .. } => *span,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Path {
    pub segments: Vec<Ident>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnItem {
    pub vis: Visibility,
    pub kind: FnItemKind,
    pub name: Ident,
    pub generics: Vec<GenericParam>,
    pub params: Vec<Param>,
    pub ret: TypeExpr,
    pub body: Block,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct IntrinsicFnItem {
    pub vis: Visibility,
    pub name: Ident,
    pub generics: Vec<GenericParam>,
    pub params: Vec<Param>,
    pub ret: TypeExpr,
    pub span: Span,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FnItemKind {
    /// A normal, top-level function item: `fn f(...) { ... }`.
    Function,
    /// A method inside an `impl` block.
    Method { receiver: MethodReceiverKind },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MethodReceiverKind {
    /// An instance method with an implicit `self` receiver.
    Instance { readonly: bool },
    /// A `static fn` method with no receiver.
    Static,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Param {
    pub pat: Pattern,
    pub ty: TypeExpr,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct StructItem {
    pub vis: Visibility,
    pub name: Ident,
    pub generics: Vec<GenericParam>,
    pub body: StructBody,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub enum StructBody {
    Named { fields: Vec<FieldDecl> },
    NewType { inner: TypeExpr },
}

#[derive(Clone, Debug, PartialEq)]
pub struct FieldDecl {
    pub name: Ident,
    pub ty: TypeExpr,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct EnumItem {
    pub vis: Visibility,
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
    pub vis: Visibility,
    pub name: Ident,
    pub generics: Vec<GenericParam>,
    /// Super-interfaces declared in `interface X: A + B { ... }`.
    pub supers: Vec<PathType>,
    pub members: Vec<InterfaceMember>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ModItem {
    pub vis: Visibility,
    pub name: Ident,
    pub kind: ModKind,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ModKind {
    /// Inline module: `mod foo { ... }`.
    Inline { items: Vec<Item> },
    /// File or directory module: `mod foo;` (loaded by the module loader).
    File,
}

#[derive(Clone, Debug, PartialEq)]
pub struct UseItem {
    pub vis: Visibility,
    pub path: Path,
    pub alias: Option<Ident>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub enum InterfaceMember {
    AssocType(AssocTypeDecl),
    Method(InterfaceMethodMember),
}

#[derive(Clone, Debug, PartialEq)]
pub struct AssocTypeDecl {
    pub name: Ident,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct InterfaceMethodMember {
    pub readonly: bool,
    pub name: Ident,
    pub generics: Vec<GenericParam>,
    pub params: Vec<Param>,
    pub ret: TypeExpr,
    /// Optional default implementation body.
    pub body: Option<Block>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ImplItem {
    pub generics: Vec<GenericParam>,
    pub header: ImplHeader,
    pub members: Vec<ImplMember>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ImplMember {
    AssocType(AssocTypeDef),
    Method(FnItem),
}

#[derive(Clone, Debug, PartialEq)]
pub struct AssocTypeDef {
    pub name: Ident,
    pub ty: TypeExpr,
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
    /// Bounds are currently limited to interface types.
    pub bounds: Vec<PathType>,
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
    Tuple {
        items: Vec<TypeExpr>,
        span: Span,
    },
    Fn {
        params: Vec<TypeExpr>,
        ret: Box<TypeExpr>,
        span: Span,
    },
    Cont {
        param: Box<TypeExpr>,
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
            | TypeExpr::Tuple { span, .. }
            | TypeExpr::Fn { span, .. }
            | TypeExpr::Cont { span, .. } => *span,
            TypeExpr::Path(p) => p.span,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PrimType {
    Unit,
    Never,
    Bool,
    Int,
    Float,
    Byte,
    Char,
    String,
    Bytes,
}

#[derive(Clone, Debug, PartialEq)]
pub struct PathType {
    pub segments: Vec<PathTypeSegment>,
    /// Optional associated type bindings (interface types only).
    ///
    /// Syntax: `I{Item = int, ...}` or `I<T>{Item = int, ...}`.
    pub assoc_bindings: Vec<AssocTypeBinding>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct PathTypeSegment {
    pub name: Ident,
    pub args: Vec<TypeExpr>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct AssocTypeBinding {
    pub name: Ident,
    pub ty: TypeExpr,
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
        pat: Pattern,
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
    Tuple {
        items: Vec<Expr>,
        span: Span,
    },
    StructLit {
        type_path: Path,
        fields: Vec<(Ident, Expr)>,
        span: Span,
    },
    EffectCall {
        interface: PathType,
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
        /// Explicit generic type arguments supplied at the call site (Rust-like turbofish).
        ///
        /// Empty means no explicit type arguments; type arguments may still be inferred.
        type_args: Vec<TypeExpr>,
        args: Vec<Expr>,
        span: Span,
    },
    Field {
        base: Box<Expr>,
        name: FieldName,
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
    As {
        expr: Box<Expr>,
        ty: TypeExpr,
        span: Span,
    },
    AsQuestion {
        expr: Box<Expr>,
        ty: TypeExpr,
        span: Span,
    },
    Is {
        expr: Box<Expr>,
        ty: TypeExpr,
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
            | Expr::Tuple { span, .. }
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
            | Expr::Assign { span, .. }
            | Expr::As { span, .. }
            | Expr::AsQuestion { span, .. }
            | Expr::Is { span, .. } => *span,
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
    pub interface: PathType,
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
    Tuple {
        prefix: Vec<Pattern>,
        rest: Option<RestPat>,
        suffix: Vec<Pattern>,
        span: Span,
    },
    Enum {
        enum_path: Path,
        variant: Ident,
        fields: Vec<Pattern>,
        span: Span,
    },
    /// Constructor-like destructuring for enum variants and new-type structs.
    ///
    /// This is resolved during typechecking based on what `path` refers to.
    Ctor {
        path: Path,
        args: Vec<Pattern>,
        span: Span,
    },
    Struct {
        type_path: Path,
        fields: Vec<(Ident, Pattern)>,
        has_rest: bool,
        span: Span,
    },
    Array {
        prefix: Vec<Pattern>,
        rest: Option<RestPat>,
        suffix: Vec<Pattern>,
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
            | Pattern::Ctor { span, .. }
            | Pattern::Struct { span, .. }
            | Pattern::Tuple { span, .. }
            | Pattern::Array { span, .. } => *span,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct RestPat {
    /// Binder name for `..name`, or `None` for plain `..` (ignored).
    pub binding: Option<Ident>,
    pub span: Span,
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
