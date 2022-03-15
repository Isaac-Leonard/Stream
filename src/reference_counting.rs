/*
use crate::ast::*;
fn generate_reference_tree(prog: &Program) -> SymbolTable {
    let table = SymbolTable::new();
    let global_scope = PlainScopeRef::new();
    table
}

fn calc_references(exp: &ExpEnvironment, table: &SymbolTable, scope: &PlainScopeRef) {
    use CompExpression::*;
    match exp.expression.as_ref() {
        Assign(lhs, exp) => {
            left_hand_assignment(lhs, table, scope);
            calc_references(exp, table, scope);
        }
        Read(var) => table.read(&var.name, scope),
    }
}

fn left_hand_assignment(exp: &ExpEnvironment, table: &SymbolTable, scope: &PlainScopeRef) {
    use CompExpression::*;
    match exp.expression.as_ref() {
        Read(var) => table.read(&var.name, scope),
        Index(arr, ind) => {}
        _ => unreachable!(),
    };
}
*/
