use super::{ClauseAdder, Param, ParamArgs};
use crate::Syntax;
use crate::writers::NextParam;
use std::sync::Arc;

/// Représente une condition SQL complète (version runtime)
#[derive(Clone)]
pub enum ConditionInfo<'req> {
    /// Opération AND entre deux conditions
    LogicalAnd(Box<ConditionInfo<'req>>, Box<ConditionInfo<'req>>),
    /// Opération OR entre deux conditions
    LogicalOr(Box<ConditionInfo<'req>>, Box<ConditionInfo<'req>>),
    /// Comparaison entre une colonne et une valeur
    BinaryCondition {
        column: String,
        operator: BinaryOperator,
        value: Arc<dyn Param + Send+ Sync+'req >,
    },
    /// Condition unaire (ex: NOT)
    UnaryCondition {
        operator: UnaryOperator,
        column: String,
    },
}

/// Opérateurs de comparaison SQL
#[derive(Debug, Clone, Copy)]
pub enum BinaryOperator {
    Equal,
    NotEqual,
    GreaterThan,
    LessThan,
    GreaterThanOrEqual,
    LessThanOrEqual,
    In,
    NotIn,
    Like,
    NotLike,
}

impl BinaryOperator {
    pub fn as_str(&self) -> &'static str {
        match self {
            BinaryOperator::Equal => "=",
            BinaryOperator::NotEqual => "!=",
            BinaryOperator::GreaterThan => ">",
            BinaryOperator::LessThan => "<",
            BinaryOperator::GreaterThanOrEqual => ">=",
            BinaryOperator::LessThanOrEqual => "<=",
            BinaryOperator::In => "IN",
            BinaryOperator::NotIn => "NOT IN",
            BinaryOperator::Like => "LIKE",
            BinaryOperator::NotLike => "NOT LIKE",
        }
    }
}

/// Opérateurs unaires SQL
#[derive(Debug, Clone, Copy)]
pub enum UnaryOperator {
    IsNone,
    IsSome,
}

impl UnaryOperator {
    pub fn as_str(&self) -> &'static str {
        match self {
            UnaryOperator::IsNone => "IS NULL",
            UnaryOperator::IsSome => "IS NOT NULL",
        }
    }
}

impl<'req> ClauseAdder for ConditionInfo<'req> {
    fn bind<'lam, 'args, 'p>(&'lam self, args: &'args mut ParamArgs<'p>)
    where
        'lam: 'p,
    {
        match self {
            ConditionInfo::LogicalAnd(left, right) | ConditionInfo::LogicalOr(left, right) => {
                left.bind(args);
                right.bind(args);
            }
            ConditionInfo::BinaryCondition { value, .. } => {
                args.push(value.as_ref());
            }
            ConditionInfo::UnaryCondition { .. } => {
                // Les conditions unaires n'ont pas de paramètres
            }
        }
    }

    fn clause(&self, syntax: Syntax, alias: &str, next_params: &NextParam) -> Option<String> {
        match self {
            ConditionInfo::LogicalAnd(left, right) => {
                let left_clause = left.clause(syntax, alias, next_params)?;
                let right_clause = right.clause(syntax, alias, next_params)?;
                Some(format!("({} AND {})", left_clause, right_clause))
            }
            ConditionInfo::LogicalOr(left, right) => {
                let left_clause = left.clause(syntax, alias, next_params)?;
                let right_clause = right.clause(syntax, alias, next_params)?;
                Some(format!("({} OR {})", left_clause, right_clause))
            }
            ConditionInfo::BinaryCondition {
                column,
                operator,
                ..
            } => {
                let col = format!("{}.{}", alias, column);
                let op = operator.as_str();
                let param = next_params.next();
                Some(format!("{} {} {}", col, op, param))
            }
            ConditionInfo::UnaryCondition { operator, column } => {
                let col = format!("{}.{}", alias, column);
                let op = operator.as_str();
                Some(format!("{} {}",col , op ))
            }
        }
    }
}

impl<'req> ConditionInfo<'req> {
    /// Crée une condition de comparaison simple
    pub fn binary<T>(column: impl Into<String>, operator: BinaryOperator, value: T) -> Self
    where
        T: Param + Send + Sync + 'req,
    {
        ConditionInfo::BinaryCondition {
            column: column.into(),
            operator,
            value: Arc::new(value),
        }
    }

    /// Crée une condition unaire
    pub fn unary(operator: UnaryOperator, column: impl Into<String>) -> Self {
        ConditionInfo::UnaryCondition {
            operator,
            column: column.into(),
        }
    }

    /// Combine deux conditions avec AND
    pub fn and(left: Self, other: Self) -> Self {
        ConditionInfo::LogicalAnd(Box::new(left), Box::new(other))
    }

    /// Combine deux conditions avec OR
    pub fn or(right: Self, other: Self) -> Self {
        ConditionInfo::LogicalOr(Box::new(right), Box::new(other))
    }
}

#[cfg(test)]
mod tests {
    use welds_macros::condition;
use welds_macros::WeldsModel;
    use welds::query::clause::*;
    use welds::Syntax;
    use welds::writers::NextParam;

    #[derive(WeldsModel)]
    #[allow(dead_code)] // Used in tests only but not constructed
    pub struct TestModel{
        #[welds(rename="id")]
        pub identifier: i32,
        pub price: f64,
        pub name: String,
        pub is_active: bool,
        pub description: Option<String>,
    }

    #[test]
    fn test_simple_condition_macro(){
        let mut args: ParamArgs = Vec::new();
        let next_params = NextParam::new(Syntax::Postgres);

        // Create condition using the macro
        let condition_creator = condition!(|a: TestModel| a.is_active == true );
        let condition = condition_creator(TestModelSchema::default());

        condition.bind(&mut args);
        assert_eq!(args.len(), 1);

        // Test clause
        let clause = condition.clause(Syntax::Postgres, "t1", &next_params).unwrap();

        assert_eq!(clause, "t1.is_active = $1");
    }

    #[test]
    fn test_contains_condition_macro(){
        let mut args: ParamArgs = Vec::new();
        let next_params = NextParam::new(Syntax::Postgres);

        // Create condition using the macro
        let condition_creator = condition!(|a: TestModel| a.name.contains("abc")  );
        let condition = condition_creator(TestModelSchema::default());

        condition.bind(&mut args);
        assert_eq!(args.len(), 1);
        
        // Test clause
        let clause = condition.clause(Syntax::Postgres, "t1", &next_params).unwrap();

        assert_eq!(clause, "t1.name like $1");
    }

    #[test]
    fn test_is_some_condition_macro(){
        let mut args: ParamArgs = Vec::new();
        let next_params = NextParam::new(Syntax::Postgres);

        // Create condition using the macro
        let condition_creator = condition!(|a: TestModel| a.description.is_some() );
        let condition = condition_creator(TestModelSchema::default());

        condition.bind(&mut args);
        assert_eq!(args.len(), 0);

        // Test clause
        let clause = condition.clause(Syntax::Postgres, "t1", &next_params).unwrap();

        assert_eq!(clause, "t1.description IS NOT NULL");
    }

    #[test]
    fn test_is_none_condition_macro(){
        let mut args: ParamArgs = Vec::new();
        let next_params = NextParam::new(Syntax::Postgres);

        // Create condition using the macro
        let condition_creator = condition!(|a: TestModel| a.description.is_none() );
        let condition = condition_creator(TestModelSchema::default());

        condition.bind(&mut args);
        assert_eq!(args.len(), 0);

        // Test clause
        let clause = condition.clause(Syntax::Postgres, "t1", &next_params).unwrap();

        assert_eq!(clause, "t1.description IS NULL");
    }

    #[test]
    fn test_or_condition_macro(){
        let mut args: ParamArgs = Vec::new();
        let next_params = NextParam::new(Syntax::Postgres);

        // Create condition using the macro
        let condition_creator = condition!(|a: TestModel| a.is_active == true || a.price < 100.0 );
        let condition = condition_creator(TestModelSchema::default());

        condition.bind(&mut args);
        assert_eq!(args.len(), 2);

        // Test clause
        let clause = condition.clause(Syntax::Postgres, "t1", &next_params).unwrap();

        assert_eq!(clause, "(t1.is_active = $1 OR t1.price < $2)");
    }

    #[test]
    fn test_and_condition_macro(){
        let mut args: ParamArgs = Vec::new();
        let next_params = NextParam::new(Syntax::Postgres);

        // Create condition using the macro
        let condition_creator = condition!(|a: TestModel| a.is_active == true && a.identifier == 42 );
        let condition = condition_creator(TestModelSchema::default());

        condition.bind(&mut args);
        assert_eq!(args.len(), 2);

        // Test clause
        let clause = condition.clause(Syntax::Postgres, "t1", &next_params).unwrap();

        assert_eq!(clause, "(t1.is_active = $1 AND t1.id = $2)");
    }

    #[test]
    fn test_binary_condition_clause() {
        let condition = ConditionInfo::binary("age", BinaryOperator::GreaterThan, 18i32);
        let next_params = NextParam::new(Syntax::Postgres);
        let mut args: ParamArgs = Vec::new();

        // Test bind
        condition.bind(&mut args);
        assert_eq!(args.len(), 1);

        // Test clause
        let clause = condition.clause(Syntax::Postgres, "t1", &next_params).unwrap();
        assert_eq!(clause, "t1.age > $1");
    }

    #[test]
    fn test_logical_and_clause() {
        let cond1 = ConditionInfo::binary("age", BinaryOperator::GreaterThan, 18i32);
        let cond2 = ConditionInfo::binary("active", BinaryOperator::Equal, true);
        let condition = ConditionInfo::and(cond1,cond2);

        let next_params = NextParam::new(Syntax::Postgres);
        let mut args: ParamArgs = Vec::new();

        // Test bind
        condition.bind(&mut args);
        assert_eq!(args.len(), 2);

        // Test clause
        let clause = condition.clause(Syntax::Postgres, "t1", &next_params).unwrap();
        assert_eq!(clause, "(t1.age > $1 AND t1.active = $2)");
    }

    #[test]
    fn test_logical_or_clause() {
        let cond1 = ConditionInfo::binary("age", BinaryOperator::LessThan, 18i32);
        let cond2 = ConditionInfo::binary("age", BinaryOperator::GreaterThan, 65i32);
        let condition = ConditionInfo::or(cond1,cond2);

        let next_params = NextParam::new(Syntax::Postgres);
        let mut args: ParamArgs = Vec::new();

        // Test bind
        condition.bind(&mut args);
        assert_eq!(args.len(), 2);

        // Test clause
        let clause = condition.clause(Syntax::Postgres, "t1", &next_params).unwrap();
        assert_eq!(clause, "(t1.age < $1 OR t1.age > $2)");
    }

    #[test]
    fn test_complex_nested_clause() {
        // (age > 18 AND active = true) OR admin = true
        let cond1 = ConditionInfo::binary("age", BinaryOperator::GreaterThan, 18i32);
        let cond2 = ConditionInfo::binary("active", BinaryOperator::Equal, true);
        let cond3 = ConditionInfo::binary("admin", BinaryOperator::Equal, true);
        let condition = ConditionInfo::or(ConditionInfo::and(cond1,cond2),cond3);

        let next_params = NextParam::new(Syntax::Postgres);
        let mut args: ParamArgs = Vec::new();

        // Test bind
        condition.bind(&mut args);
        assert_eq!(args.len(), 3);

        // Test clause
        let clause = condition.clause(Syntax::Postgres, "t1", &next_params).unwrap();
        assert_eq!(clause, "((t1.age > $1 AND t1.active = $2) OR t1.admin = $3)");
    }


    #[test]
    fn test_all_operators() {
        let operators = vec![
            (BinaryOperator::Equal, "="),
            (BinaryOperator::NotEqual, "!="),
            (BinaryOperator::GreaterThan, ">"),
            (BinaryOperator::LessThan, "<"),
            (BinaryOperator::GreaterThanOrEqual, ">="),
            (BinaryOperator::LessThanOrEqual, "<="),
        ];

        for (op, expected) in operators {
            let condition = ConditionInfo::binary("col", op, 42i32);
            let next_params = NextParam::new(Syntax::Postgres);
            let clause = condition.clause(Syntax::Postgres, "t1", &next_params).unwrap();
            assert!(clause.contains(expected), "Expected {} in {}", expected, clause);
        }
    }

    #[test]
    fn test_sqlite_syntax() {
        let condition = ConditionInfo::binary("name", BinaryOperator::Equal, "test".to_string());
        let next_params = NextParam::new(Syntax::Sqlite);
        let clause = condition.clause(Syntax::Sqlite, "t1", &next_params).unwrap();
        assert_eq!(clause, "t1.name = ?");
    }

    #[test]
    fn test_mysql_syntax() {
        let condition = ConditionInfo::binary("id", BinaryOperator::GreaterThan, 10i32);
        let next_params = NextParam::new(Syntax::Mysql);
        let clause = condition.clause(Syntax::Mysql, "t1", &next_params).unwrap();
        assert_eq!(clause, "t1.id > ?");
    }

    #[test]
    fn test_mssql_syntax() {
        let condition = ConditionInfo::binary("price", BinaryOperator::LessThan, 100.0f64);
        let next_params = NextParam::new(Syntax::Mssql);
        let clause = condition.clause(Syntax::Mssql, "t1", &next_params).unwrap();
        assert_eq!(clause, "t1.price < @p1");
    }
}

