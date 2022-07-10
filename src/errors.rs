use crate::ast::*;
use std::fmt::{Display, Formatter, Result};
use std::ops::Range;

macro_rules! errors {
    ($(($code:tt, $Name:ident $(($($arg_name:ident: $arg_type:ty),*))?, $msg:tt)),*) => {
	#[derive(Debug, PartialEq, Clone)]
	pub enum CompError {
            $($Name(
		$($($arg_type,)*)?
		    Range<usize>
	    ),)*
        }
        impl CompError {
	    fn get_code(&self)->i32{
		match self {
		    $(CompError::$Name($($( $arg_name,)*)? _) =>{
			// We have to pretend to use the variables otherwise the compiler complains, also we can't use _s
			let _=($($($arg_name,)*)?);
			$code
		    },)*
		}
	    }
            pub fn get_msg(&self, lines:&Vec<i32>) -> String {
                match self {
		    $(CompError::$Name($($($arg_name,)*)? loc) =>{
			let pos = get_pos(loc.start as i32, lines);
			format!("Error [{}]: {}, at {}", self.get_code(), format!($msg, $($($arg_name,)*)?), pos)
		    })*
                }
            }

            pub fn get_pos(&self, lines:&Vec<i32>) -> (FilePosition, FilePosition) {
                match self {
		    $(CompError::$Name($($($arg_name,)*)? loc) =>{
			// We have to pretend to use the variables otherwise the compiler complains, also we can't use _s
			let _=($($($arg_name,)*)?);
			(get_pos(loc.start as i32, lines) ,get_pos(loc.end as i32, lines))
		    })*
                }
            }
        }
    };
}
pub struct FilePosition {
    pub line: i32,
    pub column: i32,
}
impl Display for FilePosition {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}: {}", self.line, self.column)
    }
}
pub fn get_pos(pos: i32, lines: &[i32]) -> FilePosition {
    let line = lines.iter().position(|x| *x > pos).unwrap() as usize - 1;
    let column = pos - lines[line];
    FilePosition {
        line: line as i32,
        column,
    }
}
errors!(
    (
        1,
        ConstReassign(name:String),
        "Cannot reassign to already declared constant '{}'"
    ),
    (
        2,
        GlobalReassign(name:String),
        "Cannot reassign variable '{}' in the global scope"
    ),
    (
        3,
        UseUninitialisedVariable(name:String),
        "Cannot use uninitialised variable '{}'"
    ),
    (
        4,
        InvalidComparison(op:Op, l:CompType, r:CompType),
        "Invalid operation '{}' for types '{}' and '{}' "
    ),
    (5, EmptyType, "Cannot have empty types"),
    (
        6,
        CannotFindVariable(name:String),
        "Cannot find variable '{}'"
    ),
    (7, CannotFindType(ty:String), "Cannot find type '{}'"),
    (
        8,
        BoolInWhile(ty:CompType),
        "The comparison expression in a while loop must resolve to type 'Bool', found '{}' instead"
    ),
    (9, BoolInIf(ty:CompType), "The comparison expression in an if expression must resolve to type  'Bool', found '{}' instead"),
    (10, InvalidAssignment(attempted:CompType, allowed:CompType), "Type '{}' is not assignable to type '{}'"),
    (11, NonfunctionCall(name:String, var:CompType), "Cannot call variable '{}' of type '{}'"),
    (12, UntypedExternal(name:String), "External variable '{}' must be declared with a type"),
    (13, RedeclareInSameScope(name:String), "Cannot redeclare variable '{}' in the same scope"),
    (14, TypeAlreadyDefined(name:String), "Type '{}' is already defined"),
    (15, CannotIndexType(ty:CompType), "Cannot access elements of type '{}'"),
    (16, InvalidIndexType(ty:CompType), "Cannot index array with '{}'"),
    (17, InvalidLeftHandForAssignment(found:Expression), "Cannot assign to expression {:?}, can only assign to variables"),
    (18, MismatchedTypeInArray(expected:CompType, found:Vec<CompType>), "Found multiple types in array, array starts with {} but also contains the following types: {:?}"),
    (19, MissingPropertyInUnion(key:String, ty:CompType), "The property '{}' does not exist for all variants of {}"),
    (20, PropertyDoesNotExistOnType(key:String, ty:CompType), "The property '{}' does not exist on type '{}'"),
    (21, ModuleNotFound(path:String), "Could not find a module at '{}'"),
    (22, NotImplemented(custom_msg:String), " Not implemented: {}"),
    (23, WrongArgumentsCount(name:String, recieved:usize, expected:usize), " Attempted to call function '{}' with '{}' arguments but expected '{}' arguments"),
    (23, NotEnoughGenerics, " Need more generics"),
    (24, MismatchedGenericConstraint(provided:CompType, super_ty:CompType), " Invalid generic argument provided, '{}' does not extend '{}'")
);
