pub mod errors {
    use std::fmt::{Display, Formatter, Result};
    use std::ops::Range;

    macro_rules! errors {
        ($(($code:tt, $Name:ident $(($($args:ident)*))?, $msg:tt)),*) => {
	    pub enum CompError {
                $($Name(
		    $($($args,)*)?
			Range<usize>
		),)*
            }
            impl CompError {
		fn get_code(&self)->i32{
		    match self {
			$(CompError::$Name($($($args,)*)? _) =>$code,)*
		    }
		}
                fn get_msg(&self, lines:&Vec<i32>) -> String {
                    match self {
			$(CompError::$Name($($($args,)*)? loc) =>{
			    let pos = get_pos(loc.start as i32, lines);
			    format!($msg, $($($args,)*)? pos)
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
        let mut line_number: usize = 0;
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
                column: pos - lines[line_number],
            }
        }
    }
    errors!(
        (
            1,
            ConstReassign(String),
            "Cannot reassign to already declared constant {} at {}"
        ),
        (
            2,
            GlobalReassign(String),
            "Cannot reassign variable in the global scope: '{}' at {}"
        )
    );
}
