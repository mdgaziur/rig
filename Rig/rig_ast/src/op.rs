use std::str::FromStr;

#[derive(Debug, Clone)]
pub enum LogicalOperator {
    And,
    Or,
    Equal,
    NotEqual,
    Greater,
    Less,
    GreaterEq,
    LessEq,
}

impl ToString for LogicalOperator {
    fn to_string(&self) -> String {
        String::from(match self {
            LogicalOperator::And => "&&",
            LogicalOperator::Or => "||",
            LogicalOperator::Equal => "==",
            LogicalOperator::NotEqual => "!=",
            LogicalOperator::Greater => ">",
            LogicalOperator::Less => "<",
            LogicalOperator::GreaterEq => ">=",
            LogicalOperator::LessEq => "<=",
        })
    }
}

impl FromStr for LogicalOperator {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "&&" => Ok(Self::And),
            "||" => Ok(Self::Or),
            "==" => Ok(Self::Equal),
            "!=" => Ok(Self::NotEqual),
            ">" => Ok(Self::Greater),
            "<" => Ok(Self::Less),
            ">=" => Ok(Self::GreaterEq),
            "<=" => Ok(Self::LessEq),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone)]
pub enum BinaryOperator {
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulus,
    Or,
    Xor,
    And,
    LeftShift,
    RightShift,
}

impl ToString for BinaryOperator {
    fn to_string(&self) -> String {
        String::from(match self {
            BinaryOperator::Plus => "+",
            BinaryOperator::Minus => "-",
            BinaryOperator::Multiply => "*",
            BinaryOperator::Divide => "/",
            BinaryOperator::Modulus => "%",
            BinaryOperator::Or => "|",
            BinaryOperator::Xor => "^",
            BinaryOperator::And => "&",
            BinaryOperator::LeftShift => "<<",
            BinaryOperator::RightShift => ">>",
        })
    }
}

impl FromStr for BinaryOperator {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "+" => Ok(Self::Plus),
            "-" => Ok(Self::Minus),
            "*" => Ok(Self::Multiply),
            "/" => Ok(Self::Divide),
            "%" => Ok(Self::Modulus),
            "|" => Ok(Self::Or),
            "^" => Ok(Self::Xor),
            "&" => Ok(Self::And),
            "<<" => Ok(Self::LeftShift),
            ">>" => Ok(Self::RightShift),
            _ => Err(()),
        }?)
    }
}

#[derive(Debug, Clone)]
pub enum UnaryOperator {
    Not,
    Negate,
}

impl ToString for UnaryOperator {
    fn to_string(&self) -> String {
        String::from(match self {
            UnaryOperator::Not => "!",
            UnaryOperator::Negate => "-",
        })
    }
}

impl FromStr for UnaryOperator {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "!" => Ok(Self::Not),
            "-" => Ok(Self::Negate),
            _ => Err(()),
        }?)
    }
}
