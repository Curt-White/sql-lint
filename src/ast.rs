
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Operator {
    // Arithmetic Ops
    UnaryPlus,
    UnaryMinus,
    Add,
    Substract,
    Multiply,
    Subtract,

    // Boolean Ops
    Equal,
    NotEqual,
    GreaterThan,
    LessThan,
    
    // Other Ops
    FieldAccess,
}

#[derive(Debug)]
pub enum QuoteType {
    Single,
    Double
}

#[derive(Debug)]
pub struct StringAST {
    pub value: String,
    pub quote_type: QuoteType,
}

#[derive(Debug)]
pub struct IntAST {
    pub value: i64,
}

#[derive(Debug)]
pub struct FloatAST {
    pub value: f64,
}

#[derive(Debug)]
pub enum LiteralAST {
    String(StringAST),
    Float(FloatAST),
    Integer(IntAST),
}

#[derive(Debug)]
pub enum SQLType {
    String,
    Float,
    Integer,
    Unresolved,
}

#[derive(Debug)]
pub struct ColumnAST {
    pub expression: ExpressionAST,
    pub as_kw: Option<String>,
    pub alias: Option<String>
}

#[derive(Debug)]
pub enum FunctionSourceAST {
    Name(String),
    Path(PathAST)
}

#[derive(Debug)]
pub struct FunctionCallAST {
    pub name: FunctionSourceAST,
    pub args: Vec<Box<ExpressionAST>>,
    pub is_safe: bool
}

#[derive(Debug)]
pub struct PathAST {
    pub is_quoted: bool,
    pub project: Option<String>,
    pub dataset: String,
    pub table: String,
}

#[derive(Debug)]
pub enum SourceAST {
    Name(String),
    Path(PathAST),
    SubQuery(Box<SelectAST>),
}

#[derive(Debug)]
pub struct SelectAST {
    pub select_kw: String,
    pub columns: Vec<ColumnAST>,
    pub from: Option<FromAST>,
}

#[derive(Debug)]
pub struct FromAST {
    pub from_kw: String,
    pub source: SourceAST,
}

#[derive(Debug)]
pub enum ExpressionType {
    Function(FunctionSourceAST),
    SubQuery(Box<SelectAST>),
    Identifier(String),
    Literal(LiteralAST),
    BinaryOperation(Box<ExpressionAST>, Operator, Box<ExpressionAST>),
    UnaryOperation(Operator, Box<ExpressionAST>)
}

#[derive(Debug)]
pub struct ExpressionAST {
    pub expression: ExpressionType,
    pub return_type: SQLType
}

#[derive(Debug)]
pub enum OperationAST {
    Select(SelectAST),
}
