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
		    $(CompError::$Name($($($arg_name,)*)? _) =>$code,)*
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
        }
    };
}
struct FilePosition {
    line: i32,
    column: i32,
}
impl Display for FilePosition {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}: {}", self.line, self.column)
    }
}
fn get_pos(pos: i32, lines: &Vec<i32>) -> FilePosition {
    let mut line_number: i32 = 0;
    for line in lines {
        if *line > pos {
            break;
        };
        line_number += 1
    }
    if lines.len() == 0 {
        FilePosition {
            line: 0,
            column: pos,
        }
    } else {
        FilePosition {
            line: line_number as i32,
            column: pos - lines[line_number as usize - 1] + 1,
        }
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
    (10, InvalidAssignment(allowed:CompType, attempted:CompType), "Type '{}' is not assignable to type '{}'"),
    (11, NonfunctionCall(name:String, var:CompType), "Cannot call variable '{}' of type '{}'"),
    (12, UntypedExternal(name:String), "External variable '{}' must be declared with a type"),
    (13, RedeclareInSameScope(name:String), "Cannot redeclare variable '{}' in the same scope"),
    (14, TypeAlreadyDefined(name:String), "Type '{}' is already defined"),
    (15, CannotIndexType(ty:CompType), "Cannot access elements of type '{}'"),
    (16, InvalidIndexType(ty:CompType), "Cannot index array with '{}'"),
    (17, InvalidLeftHandForAssignment(found:Expression), "Cannot assign to expression {:?}, can only assign to variables"),
    (18, MismatchedTypeInArray(expected:CompType, found:Vec<CompType>), "Found multiple types in array, array starts with {} but also contains the following types: {:?}")
);
