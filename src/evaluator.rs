use rand;
use std::sync::Arc;

use crate::ast::{self, ASTNode, Modifier};

pub struct EvaluatedNode {
    ast_node: Arc<ASTNode>,
    result: EvaluatedResult,
}

enum EvaluatedResult {
    BinOp {
        lhs: Box<EvaluatedNode>,
        rhs: Box<EvaluatedNode>,
        op: ast::BinOp,
    },
    UnOp {
        expr: Box<EvaluatedNode>,
        op: ast::UnOp,
    },
    Literal(Literal),
    List {
        items: Vec<EvaluatedNode>,
    },
    Dice {
        results: Vec<Literal>,
    },
}

pub struct Literal {
    val: u64,
    discarded: bool,
}

impl From<ASTNode> for EvaluatedNode {
    fn from(value: ASTNode) -> Self {
        Arc::new(value).into()
    }
}

fn roll_dice(
    count: usize,
    size: u64,
    modifiers: &[Modifier],
    rng: &mut impl rand::Rng,
) -> Vec<Literal> {
    let initial_results: Vec<_> = (0..count)
        .map(|_| Literal {
            val: rng.gen_range(1..=size),
            discarded: false,
        })
        .collect();

    let mut results = Vec::with_capacity(initial_results.len());

    for result in initial_results {
        results.push(result);
        let len = results.len();
        let result = results.get_mut(len).unwrap();
        let mut additional = Vec::new();
        for mod_type in modifiers {
            match &mod_type {
                Modifier::Conditional { mod_type, selector } => {
                    let matches = match selector {
                        ast::Selector::Ordered(_) => continue,
                        ast::Selector::Unordered(sel) => match sel {
                            ast::UnorderedSelector::Equal(val) => result.val == *val,
                            ast::UnorderedSelector::LessThan(val) => result.val < *val,
                            ast::UnorderedSelector::GreaterThan(val) => result.val > *val,
                        },
                    };
                    additional.extend(match mod_type {
                        ast::ConditionalModifier::Keep => {
                            if !matches {
                                result.discarded = true
                            };
                            vec![]
                        }
                        ast::ConditionalModifier::Drop => {
                            if matches {
                                result.discarded = true
                            };
                            vec![]
                        }
                        ast::ConditionalModifier::Explode => {
                            if matches {
                                roll_dice(1, size, modifiers, rng)
                            } else {
                                vec![]
                            }
                        }
                        ast::ConditionalModifier::Reroll => {
                            if matches {
                                result.discarded = true;
                                roll_dice(1, size, modifiers, rng)
                            } else {
                                vec![]
                            }
                        }
                        ast::ConditionalModifier::RerollOnce => {
                            if matches {
                                result.discarded = true;
                                let new_modifiers: Vec<Modifier> = modifiers
                                    .iter()
                                    .filter_map(|val| match val {
                                        Modifier::Conditional {
                                            mod_type: ast::ConditionalModifier::RerollOnce,
                                            ..
                                        } => None,
                                        _ => Some(val.clone()),
                                    })
                                    .collect();
                                roll_dice(1, size, &new_modifiers, rng)
                            } else {
                                vec![]
                            }
                        }
                    });
                }
                Modifier::MinMax { mod_type, val } => match mod_type {
                    ast::MinMaxModifier::Min => {
                        if result.val < *val {
                            result.val = *val
                        }
                    }
                    ast::MinMaxModifier::Max => {
                        if result.val > *val {
                            result.val = *val
                        }
                    }
                },
            }
        }
        results.extend(additional);
    }

    results
}

impl From<Arc<ASTNode>> for EvaluatedNode {
    fn from(ast_node: Arc<ASTNode>) -> Self {
        let mut thread_rng = rand::thread_rng();
        let result = match ast_node.as_ref() {
            ASTNode::BinOp { lhs, rhs, op } => EvaluatedResult::BinOp {
                lhs: Box::new(lhs.clone().into()),
                rhs: Box::new(rhs.clone().into()),
                op: *op,
            },
            ASTNode::UnOp { expr: val, op } => EvaluatedResult::UnOp {
                expr: Box::new(val.clone().into()),
                op: *op,
            },
            ASTNode::Dice {
                count,
                size,
                modifiers,
            } => EvaluatedResult::Dice {
                results: roll_dice(*count, *size, modifiers.inner(), &mut thread_rng),
            },
            ASTNode::Literal { val } => EvaluatedResult::Literal(Literal {
                val: *val,
                discarded: false,
            }),
            ASTNode::Group { items, modifiers } => todo!(),
        };

        Self { ast_node, result }
    }
}
