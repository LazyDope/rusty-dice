use std::{num::ParseIntError, str::FromStr, sync::Arc};

use thiserror::Error;

use lrlex::{lrlex_mod, DefaultLexeme};
use lrpar::lrpar_mod;

lrlex_mod!("dice.l");
lrpar_mod!("dice.y");

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum ASTNode {
    BinOp {
        lhs: Box<ASTNode>,
        rhs: Box<ASTNode>,
        op: BinOp,
    },
    UnOp {
        val: Box<ASTNode>,
        op: UnOp,
    },
    Dice {
        count: usize,
        size: u64,
        modifiers: Modifiers,
    },
    Literal {
        val: u64,
    },
    Group {
        items: Vec<ASTNode>,
        modifiers: Modifiers,
    },
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    IntDiv,
    Mod,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum UnOp {
    // Pos variant exists so that the original dice can be recreated
    Pos,
    Neg,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Modifiers {
    inner: Vec<Modifier>,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Modifier {
    mod_type: ModifierType,
    selector: Selector,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum ModifierType {
    Keep,
    Drop,
    Explode,
    Reroll,
    RerollOnce,
    Maximum,
    Minimum,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Selector {
    Equal(u64),
    LessThan(u64),
    GreaterThan(u64),
    Lowest(usize),
    Highest(usize),
}

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("invalid lexeme: `{0}`")]
    LexemeError(#[from] DefaultLexeme),
    #[error("int parse error: `{0}`")]
    ParseIntError(#[from] ParseIntError),
    #[error("invalid modifier: `{0}`")]
    ModifierTypeError(Arc<str>),
    #[error("invalid selector: `{0}`")]
    SelectorError(Arc<str>),
}

impl ModifierType {
    fn from_chars(chars: &mut impl Iterator<Item = char>) -> Result<Option<Self>, ParseError> {
        Ok(Some(match chars.next() {
            Some('k') => ModifierType::Keep,
            Some('p') => ModifierType::Drop,
            Some('e') => ModifierType::Explode,
            Some('r') => match chars.next() {
                Some('r') => ModifierType::Reroll,
                Some('o') => ModifierType::RerollOnce,
                Some(c) => return Err(ParseError::ModifierTypeError(format!("r{}", c).into())),
                None => return Err(ParseError::ModifierTypeError("r".into())),
            },
            Some('m') => match chars.next() {
                Some('i') => ModifierType::Minimum,
                Some('a') => ModifierType::Maximum,
                Some(c) => return Err(ParseError::ModifierTypeError(format!("m{}", c).into())),
                None => return Err(ParseError::ModifierTypeError("m".into())),
            },
            Some(c) => return Err(ParseError::ModifierTypeError(c.to_string().into())),
            None => return Ok(None),
        }))
    }
}

impl Selector {
    fn from_char_and_nums(selector_char: char, numbers: &str) -> Result<Self, ParseError> {
        Ok(match selector_char {
            '<' => Selector::LessThan(numbers.parse()?),
            '>' => Selector::GreaterThan(numbers.parse()?),
            'h' => Selector::Highest(numbers.parse()?),
            'l' => Selector::Lowest(numbers.parse()?),
            '0'..='9' => Selector::Equal(format!("{}{}", selector_char, numbers).parse()?),
            _ => return Err(ParseError::SelectorError(selector_char.to_string().into())),
        })
    }
}

impl FromStr for Modifiers {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut mods = vec![];
        let mut chars = s.chars().peekable();
        while let Some(mod_type) = ModifierType::from_chars(&mut chars)? {
            let selector_char = chars
                .next()
                .ok_or_else(|| ParseError::SelectorError("".into()))?;

            // A take_while here would allocate an empty string anyways,
            // and this way we can avoid moving chars up to the end later
            let mut numbers: String = String::new();
            while let Some(num) = chars.next_if(|val| ('0'..='9').contains(val)) {
                numbers.push(num)
            }

            let selector = Selector::from_char_and_nums(selector_char, &numbers)?;

            mods.push(Modifier { mod_type, selector })
        }
        Ok(mods.into())
    }
}

impl From<Vec<Modifier>> for Modifiers {
    fn from(value: Vec<Modifier>) -> Self {
        Modifiers { inner: value }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_dice_parse() {
        let lexerdef = dice_l::lexerdef();
        let lex = lexerdef.lexer("4d6kh3e6mi1");
        let result = match dice_y::parse(&lex) {
            (Some(Ok(result)), errs) => {
                for e in errs {
                    eprintln!("{:?}", e);
                }
                result
            }
            (res, errs) => {
                panic!("{:?}: {:?}", res, errs);
            }
        };
        let expected_result = ASTNode::Dice {
            count: 4,
            size: 6,
            modifiers: vec![
                Modifier {
                    mod_type: ModifierType::Keep,
                    selector: Selector::Highest(3),
                },
                Modifier {
                    mod_type: ModifierType::Explode,
                    selector: Selector::Equal(6),
                },
                Modifier {
                    mod_type: ModifierType::Minimum,
                    selector: Selector::Equal(1),
                },
            ]
            .into(),
        };
        assert_eq!(result, expected_result);
    }

    #[test]
    fn test_order_of_ops() {
        let lexerdef = dice_l::lexerdef();
        let lex = lexerdef.lexer("+-++1+2*3%4");
        let result = match dice_y::parse(&lex) {
            (Some(Ok(result)), errs) => {
                for e in errs {
                    eprintln!("{:?}", e);
                }
                result
            }
            (res, errs) => {
                panic!("{:?}: {:?}", res, errs);
            }
        };
        let expected_result = ASTNode::BinOp {
            lhs: Box::new(ASTNode::UnOp {
                val: Box::new(ASTNode::UnOp {
                    val: Box::new(ASTNode::UnOp {
                        val: Box::new(ASTNode::UnOp {
                            val: Box::new(ASTNode::Literal { val: 1 }),
                            op: UnOp::Pos,
                        }),
                        op: UnOp::Pos,
                    }),
                    op: UnOp::Neg,
                }),
                op: UnOp::Pos,
            }),
            rhs: Box::new(ASTNode::BinOp {
                lhs: Box::new(ASTNode::BinOp {
                    lhs: Box::new(ASTNode::Literal { val: 2 }),
                    rhs: Box::new(ASTNode::Literal { val: 3 }),
                    op: BinOp::Mul,
                }),
                rhs: Box::new(ASTNode::Literal { val: 4 }),
                op: BinOp::Mod,
            }),
            op: BinOp::Add,
        };
        assert_eq!(result, expected_result);
    }
}
