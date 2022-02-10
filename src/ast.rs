pub mod ast {
    use std::fmt::{self, Display, Formatter};

    use std::{collections::HashMap, ops::Range};

    use inkwell::{
        context::Context,
        types::{BasicType, BasicTypeEnum},
    };

    #[derive(Clone, Debug, PartialEq)]
    pub enum Symbol {
        Data(RawData),
        Identifier(String),
    }

    #[derive(Clone, Debug, PartialEq)]
    pub enum RawData {
        Str(String),
        Bool(bool),
        Int(i32),
        Float(f32),
        Func(Function),
        Null,
    }

    #[derive(Clone, Debug, PartialEq)]
    pub enum Expression {
        Addition(Box<Expression>, Box<Expression>, Range<usize>),
        Subtraction(Box<Expression>, Box<Expression>, Range<usize>),
        Multiplication(Box<Expression>, Box<Expression>, Range<usize>),
        Division(Box<Expression>, Box<Expression>, Range<usize>),
        Equal(Box<Expression>, Box<Expression>, Range<usize>),
        LessThan(Box<Expression>, Box<Expression>, Range<usize>),
        Terminal(Symbol, Range<usize>),
        FuncCall(String, Vec<Expression>, Range<usize>),
        Block(Vec<Expression>, Range<usize>),
        IfElse(Box<Expression>, Vec<Instr>, Vec<Instr>, Range<usize>),
    }
    impl Expression {
        pub fn get_range(&self) -> Range<usize> {
            use Expression::*;
            match &self {
                Addition(_, _, range)
                | Subtraction(_, _, range)
                | Multiplication(_, _, range)
                | Division(_, _, range)
                | LessThan(_, _, range)
                | Equal(_, _, range)
                | FuncCall(_, _, range)
                | IfElse(_, _, _, range)
                | Block(_, range)
                | Terminal(_, range) => range.clone(),
            }
        }
    }
    #[derive(Clone, PartialEq)]
    pub struct Function {
        pub args: Vec<(String, Vec<String>)>,
        pub body: Option<Vec<Instr>>,
        pub return_type: Vec<String>,
    }
    impl std::fmt::Debug for Function {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            write!(f, "Function({:?})", self.args)
        }
    }
    #[derive(Clone, Debug, PartialEq)]
    pub enum Instr {
        Invalid(String),
        Assign(String, Expression, Range<usize>),
        InitAssign(bool, bool, String, Option<Vec<String>>, Expression),
        LoneExpression(Expression, Range<usize>),
        Loop(Expression, Vec<Self>, Range<usize>),
        TypeDeclaration(String, CustomType, Range<usize>),
    }

    #[derive(Debug, Clone, PartialEq)]
    pub enum CustomType {
        Callible(Vec<Self>, Box<Self>),
        // Only have union, can map singular types / aliases to a union then flaten them later
        Union(Vec<String>),
    }

    #[derive(Debug, PartialEq, Clone)]
    pub struct NewVariable {
        pub name: String,
        pub typing: Option<CompType>,
        pub constant: bool,
        pub initialised: bool,
        pub external: bool,
    }
    impl NewVariable {
        fn get_final(&self) -> Result<CompVariable, String> {
            if self.typing == None || !self.initialised {
                println!("{:?}", self);
                Err(format!("Cannot use uninitialised variable '{}'", self.name))
            } else {
                Ok(CompVariable {
                    name: self.name.clone(),
                    typing: self.typing.clone().unwrap(),
                    constant: self.constant,
                    external: self.external,
                })
            }
        }
    }
    #[derive(Debug, PartialEq, Clone)]
    pub struct CompVariable {
        pub name: String,
        pub typing: CompType,
        pub constant: bool,
        pub external: bool,
    }

    #[derive(Debug, PartialEq, Clone)]
    pub struct FunctionAst {
        pub arguments: Vec<CompVariable>,
        pub return_type: CompType,
        pub body: Option<Box<Program>>,
    }
    impl FunctionAst {
        pub fn as_type(&self) -> CompType {
            CompType::Callible(
                self.arguments.iter().map(|x| x.typing.clone()).collect(),
                Box::new(self.return_type.clone()),
            )
        }
    }

    #[derive(Debug, PartialEq, Clone)]
    pub enum Op {
        Add,
        Sub,
        Div,
        Mult,
        Eq,
        Le,
    }
    impl Op {
        pub fn get_str(&self) -> String {
            format!("{}", self)
        }
        pub fn resulting_type(&self, a: &CompType, b: &CompType) -> Result<CompType, String> {
            use CompType::*;
            use Op::*;
            match self {
                Eq => Ok(Bool),
                Le => match (a, b) {
                    (Float, Float) | (Int, Int) => Ok(Bool),
                    _ => Err(self.invalid_comparison_msg(a, b)),
                },
                Add => {
                    if let (Str(a), Str(b)) = (a, b) {
                        Ok(Str(*a + *b))
                    } else if a == b {
                        match a {
                            Bool | Null => Err(self.invalid_comparison_msg(a, b)),
                            x => Ok(x.clone()),
                        }
                    } else {
                        Err(self.invalid_comparison_msg(a, b))
                    }
                }
                Sub | Div | Mult => {
                    if a == b {
                        match a {
                            Str(_) | Bool | Null => Err(self.invalid_comparison_msg(a, b)),
                            x => Ok(x.clone()),
                        }
                    } else {
                        Err(self.invalid_comparison_msg(a, b))
                    }
                }
            }
        }
        fn invalid_comparison_msg(&self, a: &CompType, b: &CompType) -> String {
            format!(
                "Invalid operation '{}' for types '{}' and '{}' ",
                self.get_str(),
                a.get_str(),
                b.get_str()
            )
        }
    }
    impl Display for Op {
        fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
            use Op::*;
            write!(
                f,
                "{}",
                match self {
                    Add => "+",
                    Sub => "-",
                    Mult => "*",
                    Div => "/",
                    Eq => "==",
                    Le => "<",
                }
            )
        }
    }
    #[derive(Debug, PartialEq, Clone)]
    pub enum Prefix {
        Neg,
    }
    impl Prefix {
        pub fn get_str(&self) -> &str {
            match self {
                Prefix::Neg => "-",
            }
        }
    }

    #[derive(Debug, PartialEq, Clone)]
    pub enum CompExpression {
        Value(CompData),
        BinOp(Op, Box<CompExpression>, Box<CompExpression>),
        Read(CompVariable),
        OneOp(Prefix, Box<CompExpression>),
        Call(CompVariable, Vec<CompExpression>),
        Assign(CompVariable, Box<CompExpression>),
        IfOnly {
            cond: Box<CompExpression>,
            then: Box<CompExpression>,
        },
        IfElse {
            cond: Box<CompExpression>,
            then: Box<CompExpression>,
            otherwise: Box<CompExpression>,
        },
        WhileLoop {
            cond: Box<CompExpression>,
            body: Box<CompExpression>,
        },
        List(Vec<CompExpression>),
        Prog(Box<Program>),
    }
    impl CompExpression {}

    #[derive(Debug, PartialEq, Clone)]
    pub enum CompData {
        Null,
        Bool(bool),
        Int(i32),
        Float(f32),
        Str(String),
        Func(FunctionAst),
    }
    impl CompData {
        pub fn get_type(&self) -> CompType {
            use CompData::*;
            match self {
                Null => CompType::Null,
                Bool(_) => CompType::Bool,
                Int(_) => CompType::Int,
                Float(_) => CompType::Float,
                Str(content) => CompType::Str(content.len() as u32),
                Func(ast) => ast.as_type(),
            }
        }
    }
    #[derive(Debug, PartialEq, Clone)]
    pub struct Program {
        pub scope: CompScope,
        pub body: CompExpression,
    }

    #[derive(Debug, PartialEq, Clone)]
    pub struct CompScope {
        pub types: HashMap<String, CompType>,
        pub variables: HashMap<String, CompVariable>,
        pub parent: Option<Box<CompScope>>,
    }
    impl CompScope {
        fn get_variable(&self, name: &String) -> Result<CompVariable, String> {
            if let Some(var) = self.variables.get(name) {
                Ok(var.clone())
            } else {
                match &self.parent {
                    Some(parent) => (*parent).get_variable(name),
                    None => Err(format!("Cannot find variable '{}'", name)),
                }
            }
        }

        pub fn get_type(&self, name: &String) -> Result<CompType, String> {
            if let Some(ty) = self.types.get(name) {
                Ok(ty.clone())
            } else {
                match &self.parent {
                    Some(parent) => (*parent).get_type(name),
                    None => Err(format!("Cannot find type '{}'", name)),
                }
            }
        }

        pub fn variable_exists(&self, name: &String) -> bool {
            if self.variables.contains_key(name) {
                true
            } else {
                match &self.parent {
                    Some(parent) => (*parent).variable_exists(name),
                    None => false,
                }
            }
        }

        pub fn constant_exists(&self, name: &String) -> bool {
            if let Some(var) = self.variables.get(name) {
                var.constant
            } else {
                match &self.parent {
                    Some(parent) => (*parent).constant_exists(name),
                    None => false,
                }
            }
        }

        pub fn is_global(&self) -> bool {
            self.parent.is_none()
        }
    }

    pub struct TempScope {
        pub types: HashMap<String, CompType>,
        pub variables: HashMap<String, NewVariable>,
        pub preset_variables: HashMap<String, CompVariable>,
        pub parent: Option<Box<CompScope>>,
    }
    impl TempScope {
        pub fn set_variable_initialised(&mut self, name: &String) {
            self.variables.get_mut(name).map(|v| v.initialised = true);
        }

        pub fn set_variable_type<'a>(
            &'a mut self,
            name: &String,
            ty: &CompType,
        ) -> &'a mut TempScope {
            self.variables
                .get_mut(name)
                .map(|v| v.typing = Some(ty.clone()));
            self
        }

        fn to_comp_scope(&self) -> Result<CompScope, Vec<String>> {
            let variables = self
                .variables
                .iter()
                .map(|v| v.1.get_final())
                .chain(self.preset_variables.values().map(|x| Ok(x.clone())));
            if variables.clone().any(|x| x.is_err()) {
                Err(variables.filter_map(|x| x.err()).collect())
            } else {
                Ok(CompScope {
                    variables: variables
                        .map(|x| x.map(|x| (x.name.clone(), x)))
                        .collect::<Result<HashMap<_, _>, _>>()
                        .expect("Something went wrong"),
                    types: self.types.clone(),
                    parent: self.parent.clone(),
                })
            }
        }

        pub fn to_comp_scope_so_far(&self) -> CompScope {
            let variables = self
                .variables
                .iter()
                .filter(|v| v.1.initialised)
                .map(|v| (v.0.clone(), v.1.get_final().unwrap()))
                .chain(self.preset_variables.clone().into_iter())
                .collect::<HashMap<String, CompVariable>>();

            CompScope {
                variables,
                types: self.types.clone(),
                parent: self.parent.clone(),
            }
        }

        pub fn get_variable(&self, name: &String) -> Result<CompVariable, String> {
            if let Some(var) = self.variables.get(name) {
                var.get_final()
            } else if let Some(var) = self.preset_variables.get(name) {
                Ok(var.clone())
            } else {
                match &self.parent {
                    Some(parent) => (*parent).get_variable(name),
                    None => Err(format!("Cannot find variable '{}'", name)),
                }
            }
        }

        fn get_type(&self, name: &String) -> Result<CompType, String> {
            if let Some(ty) = self.types.get(name) {
                Ok(ty.clone())
            } else {
                match &self.parent {
                    Some(parent) => (*parent).get_type(name),
                    None => Err(format!("Cannot find type '{}'", name)),
                }
            }
        }

        pub fn constant_exists(&self, name: &String) -> bool {
            if let Some(var) = self.variables.get(name) {
                var.constant
            } else {
                match &self.parent {
                    Some(parent) => (*parent).constant_exists(name),
                    None => false,
                }
            }
        }

        pub fn variable_initialised(&self, name: &String) -> bool {
            if let Some(var) = self.variables.get(name) {
                var.initialised
            } else {
                match &self.parent {
                    Some(parent) => (*parent).constant_exists(name),
                    None => false,
                }
            }
        }
        pub fn variable_has_type(&self, name: &String) -> bool {
            if let Some(var) = self.variables.get(name) {
                var.typing != None
            } else {
                match &self.parent {
                    Some(parent) => (*parent).constant_exists(name),
                    None => false,
                }
            }
        }
    }
    #[derive(Debug, PartialEq, Clone)]
    pub enum CompType {
        Callible(Vec<Self>, Box<Self>),
        Union(Vec<Self>),
        Null,
        Bool,
        Int,
        Float,
        Str(u32),
        Ptr,
    }
    impl CompType {
        pub fn get_str(&self) -> String {
            format!("{}", self)
        }
        pub fn get_compiler_type<'ctx>(&self, context: &'ctx Context) -> BasicTypeEnum<'ctx> {
            use CompType::*;
            match self.clone() {
                Int => context.i32_type().as_basic_type_enum(),
                Float => context.f32_type().as_basic_type_enum(),
                Null => context.custom_width_int_type(1).as_basic_type_enum(),
                Bool => context.custom_width_int_type(1).as_basic_type_enum(),
                Str(len) => context.i8_type().array_type(len + 1).as_basic_type_enum(),
                Ptr => context
                    .i8_type()
                    .ptr_type(inkwell::AddressSpace::Generic)
                    .as_basic_type_enum(),
                _ => panic!(
                    "get_compiler_type not implemented for type '{}'",
                    self.get_str()
                ),
            }
        }
        pub fn super_of(&self, ty: &CompType) -> bool {
            if self == ty {
                true
            } else if self == &CompType::Ptr && ty.is_str() {
                true
            } else {
                false
            }
        }
        pub fn is_bool(&self) -> bool {
            self == &CompType::Bool
        }
        fn is_null(&self) -> bool {
            self == &CompType::Null
        }

        fn is_int(&self) -> bool {
            *self == CompType::Int
        }

        pub fn is_str(&self) -> bool {
            match self {
                CompType::Str(_) => true,
                _ => false,
            }
        }
        pub fn is_callable(&self) -> bool {
            match self {
                CompType::Callible(_, _) => true,
                _ => false,
            }
        }
        pub fn flatten(&self) -> CompType {
            use CompType::*;
            match self {
                Ptr => Ptr,
                Int => Int,
                Float => Float,
                Str(len) => Str(*len),
                Bool => Bool,
                Null => Null,
                Callible(args, ret) => Callible(
                    args.iter().map(|x| x.flatten()).collect(),
                    Box::new(ret.flatten()),
                ),
                Union(types) => {
                    let types = types.iter().map(|x| x.flatten()).collect::<Vec<_>>();
                    match types.len() {
                        0 => Null,
                        1 => types[0].clone(),
                        _ => Union(types),
                    }
                }
            }
        }
        fn get_discriminant(&self) -> i8 {
            use CompType::*;
            match self {
                Ptr => 8,
                Null => 0,
                Bool => 1,
                Int => 2,
                Float => 3,
                Str(_) => 4,
                Callible(_, _) => 5,
                Union(_) => 7,
            }
        }
    }

    impl Display for CompType {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            use CompType::*;
            match self {
                Ptr => write!(f, "Ptr"),
                Int => write!(f, "Int"),
                Null => write!(f, "Null"),
                Str(len) => write!(f, "Str<{}>", len),
                Bool => write!(f, "Bool"),
                Float => write!(f, "Float"),
                Union(types) => write!(
                    f,
                    "{}",
                    types
                        .iter()
                        .map(CompType::get_str)
                        .reduce(|a, b| format!("{} | {}", a, b))
                        .unwrap()
                ),
                Callible(args, ret) => write!(
                    f,
                    "({}): {}",
                    args.iter()
                        .map(CompType::get_str)
                        .reduce(|a, b| format!("{}, {}", a, b))
                        .unwrap_or_else(String::new),
                    ret.get_str()
                ),
            }
        }
    }
}
