use std::{fmt::Display, iter::Peekable, num::ParseIntError, str::FromStr, sync::Arc};

use thiserror::Error;

use lrlex::{lrlex_mod, DefaultLexeme};
use lrpar::lrpar_mod;

lrlex_mod!("dice.l");
lrpar_mod!("dice.y");

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum ASTNode {
    BinOp {
        lhs: Arc<ASTNode>,
        rhs: Arc<ASTNode>,
        op: BinOp,
    },
    UnOp {
        expr: Arc<ASTNode>,
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
        items: Vec<Arc<ASTNode>>,
        modifiers: Modifiers,
    },
}

#[derive(PartialEq, Eq, Clone, Debug, Copy)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    IntDiv,
    Mod,
}

#[derive(PartialEq, Eq, Clone, Debug, Copy)]
pub enum UnOp {
    // Pos variant exists so that the original dice can be recreated
    Pos,
    Neg,
}

#[derive(PartialEq, Eq, Clone, Debug, Default)]
pub struct Modifiers {
    inner: Vec<Modifier>,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Modifier {
    MinMax {
        mod_type: MinMaxModifier,
        val: u64,
    },
    Conditional {
        mod_type: ConditionalModifier,
        selector: Selector,
    },
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum MinMaxModifier {
    Min,
    Max,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum ConditionalModifier {
    Keep,
    Drop,
    Explode,
    Reroll,
    RerollOnce,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Selector {
    Ordered(OrderedSelector),
    Unordered(UnorderedSelector),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum OrderedSelector {
    Lowest(usize),
    Highest(usize),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum UnorderedSelector {
    Equal(u64),
    LessThan(u64),
    GreaterThan(u64),
}

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("invalid lexeme: `{0}`")]
    LexemeError(#[from] DefaultLexeme),
    #[error("int parse error: `{0}`")]
    ParseIntError(#[from] ParseIntError),
    #[error("invalid modifier: `{0}`")]
    ModifierError(Arc<str>),
    #[error("invalid selector: `{0}`")]
    SelectorError(Arc<str>),
}

impl Modifiers {
    pub fn new() -> Modifiers {
        Modifiers::default()
    }
}

impl Modifier {
    fn from_chars(
        chars: &mut Peekable<impl Iterator<Item = char>>,
    ) -> Result<Option<Self>, ParseError> {
        Ok(match chars.next() {
            Some('m') => {
                let mod_type = MinMaxModifier::from_chars(chars)?;

                Some(Self::MinMax {
                    mod_type,
                    val: collect_numbers(chars).parse()?,
                })
            }
            Some(c) => {
                let mod_type = ConditionalModifier::from_chars(c, chars)?;

                let selector = Selector::from_chars(chars)?;

                Some(Self::Conditional { mod_type, selector })
            }
            None => return Ok(None),
        })
    }
}

fn collect_numbers(chars: &mut Peekable<impl Iterator<Item = char>>) -> String {
    // A take_while here would allocate an empty string anyways,
    // and this way we can avoid moving chars up to the end later
    let mut numbers: String = String::new();
    while let Some(num) = chars.next_if(|val| ('0'..='9').contains(val)) {
        numbers.push(num)
    }

    numbers
}

impl MinMaxModifier {
    fn from_chars(chars: &mut impl Iterator<Item = char>) -> Result<Self, ParseError> {
        Ok(match chars.next() {
            Some('i') => MinMaxModifier::Min,
            Some('a') => MinMaxModifier::Max,
            Some(c) => return Err(ParseError::ModifierError(format!("m{}", c).into())),
            None => return Err(ParseError::ModifierError("m".into())),
        })
    }
}

impl ConditionalModifier {
    fn from_chars(
        initial: char,
        chars: &mut impl Iterator<Item = char>,
    ) -> Result<Self, ParseError> {
        Ok(match initial {
            'k' => ConditionalModifier::Keep,
            'p' => ConditionalModifier::Drop,
            'e' => ConditionalModifier::Explode,
            'r' => match chars.next() {
                Some('r') => ConditionalModifier::Reroll,
                Some('o') => ConditionalModifier::RerollOnce,
                Some(c) => return Err(ParseError::ModifierError(format!("r{}", c).into())),
                None => return Err(ParseError::ModifierError("r".into())),
            },
            c => return Err(ParseError::ModifierError(c.to_string().into())),
        })
    }
}

impl Selector {
    fn from_chars(chars: &mut Peekable<impl Iterator<Item = char>>) -> Result<Self, ParseError> {
        use OrderedSelector::*;
        use Selector::*;
        use UnorderedSelector::*;
        let selector_char = chars
            .next()
            .ok_or_else(|| ParseError::SelectorError("".into()))?;

        let numbers = collect_numbers(chars);

        Ok(match selector_char {
            '<' => Unordered(LessThan(numbers.parse()?)),
            '>' => Unordered(GreaterThan(numbers.parse()?)),
            'h' => Ordered(Highest(numbers.parse()?)),
            'l' => Ordered(Lowest(numbers.parse()?)),
            '0'..='9' => Unordered(Equal(format!("{}{}", selector_char, numbers).parse()?)),
            _ => return Err(ParseError::SelectorError(selector_char.to_string().into())),
        })
    }
}

impl FromStr for Modifiers {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut mods = vec![];
        let mut chars = s.chars().peekable();
        while let Some(modifier) = Modifier::from_chars(&mut chars)? {
            mods.push(modifier)
        }
        Ok(mods.into())
    }
}

impl Display for Modifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Modifier::MinMax { mod_type, val } => write!(f, "{}{}", mod_type, val),
            Modifier::Conditional { mod_type, selector } => write!(f, "{}{}", mod_type, selector),
        }
    }
}

impl Display for ConditionalModifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                ConditionalModifier::Keep => "k",
                ConditionalModifier::Drop => "p",
                ConditionalModifier::Explode => "e",
                ConditionalModifier::Reroll => "rr",
                ConditionalModifier::RerollOnce => "ro",
            }
        )
    }
}

impl Display for Selector {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Selector::Ordered(sel) => match sel {
                OrderedSelector::Lowest(val) => write!(f, "l{}", val),
                OrderedSelector::Highest(val) => write!(f, "h{}", val),
            },
            Selector::Unordered(sel) => match sel {
                UnorderedSelector::Equal(val) => write!(f, "{}", val),
                UnorderedSelector::LessThan(val) => write!(f, "<{}", val),
                UnorderedSelector::GreaterThan(val) => write!(f, ">{}", val),
            },
        }
    }
}

impl Display for MinMaxModifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                MinMaxModifier::Min => "mi",
                MinMaxModifier::Max => "ma",
            }
        )
    }
}

impl Modifiers {
    pub fn inner(&self) -> &[Modifier] {
        self.inner.as_slice()
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

    fn parse(value: &str) -> Result<ASTNode, ParseError> {
        let lexerdef = dice_l::lexerdef();
        let lex = lexerdef.lexer(value);
        Ok(match dice_y::parse(&lex) {
            (Some(Ok(result)), errs) => {
                for e in errs {
                    eprintln!("{:?}", e);
                }
                result
            }
            (res, errs) => {
                panic!("{:?}: {:?}", res, errs);
            }
        })
    }

    #[test]
    fn test_basic_dice() {
        let result = parse("1d8").unwrap();
        let expected_result = ASTNode::Dice {
            count: 1,
            size: 8,
            modifiers: vec![].into(),
        };
        assert_eq!(result, expected_result);
    }

    #[test]
    fn test_dice_modifiers() {
        use OrderedSelector::*;
        use Selector::*;
        use UnorderedSelector::*;

        let result = parse("4d6kh3e6mi1").unwrap();
        let expected_result = ASTNode::Dice {
            count: 4,
            size: 6,
            modifiers: vec![
                Modifier::Conditional {
                    mod_type: ConditionalModifier::Keep,
                    selector: Ordered(Highest(3)),
                },
                Modifier::Conditional {
                    mod_type: ConditionalModifier::Explode,
                    selector: Unordered(Equal(6)),
                },
                Modifier::MinMax {
                    mod_type: MinMaxModifier::Min,
                    val: 1,
                },
            ]
            .into(),
        };
        assert_eq!(result, expected_result);
    }

    #[test]
    fn test_order_of_ops() {
        let result = parse("+-++1+2*3%4").unwrap();
        let expected_result = ASTNode::BinOp {
            lhs: Arc::new(ASTNode::UnOp {
                expr: Arc::new(ASTNode::UnOp {
                    expr: Arc::new(ASTNode::UnOp {
                        expr: Arc::new(ASTNode::UnOp {
                            expr: Arc::new(ASTNode::Literal { val: 1 }),
                            op: UnOp::Pos,
                        }),
                        op: UnOp::Pos,
                    }),
                    op: UnOp::Neg,
                }),
                op: UnOp::Pos,
            }),
            rhs: Arc::new(ASTNode::BinOp {
                lhs: Arc::new(ASTNode::BinOp {
                    lhs: Arc::new(ASTNode::Literal { val: 2 }),
                    rhs: Arc::new(ASTNode::Literal { val: 3 }),
                    op: BinOp::Mul,
                }),
                rhs: Arc::new(ASTNode::Literal { val: 4 }),
                op: BinOp::Mod,
            }),
            op: BinOp::Add,
        };
        assert_eq!(result, expected_result);
    }

    #[test]
    fn test_grouped_dice() {
        let result = parse("(1d8, 2d6, 5)").unwrap();
        let expected_result = ASTNode::Group {
            items: vec![
                Arc::new(ASTNode::Dice {
                    count: 1,
                    size: 8,
                    modifiers: Default::default(),
                }),
                Arc::new(ASTNode::Dice {
                    count: 2,
                    size: 6,
                    modifiers: Default::default(),
                }),
                Arc::new(ASTNode::Literal { val: 5 }),
            ],
            modifiers: Default::default(),
        };
        assert_eq!(result, expected_result);
    }
}
