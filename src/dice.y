%start Sub
%%

Sub -> Result<ASTNode, ParseError>:
	  Sub 'SUB' Add { Ok(ASTNode::BinOp { lhs: Box::new($1?), rhs: Box::new($3?), op: BinOp::Sub}) }
	| Add { $1 }
	;

Add -> Result<ASTNode, ParseError>:
	  Add 'ADD' MulDiv { Ok(ASTNode::BinOp { lhs: Box::new($1?), rhs: Box::new($3?), op: BinOp::Add}) }
	| MulDiv { $1 }
	;
	  
MulDiv -> Result<ASTNode, ParseError>:
	  MulDiv 'DIV' UnOp { Ok(ASTNode::BinOp { lhs: Box::new($1?), rhs: Box::new($3?), op: BinOp::Div}) }
	| MulDiv 'INTDIV' UnOp { Ok(ASTNode::BinOp { lhs: Box::new($1?), rhs: Box::new($3?), op: BinOp::IntDiv}) }
	| MulDiv 'MOD' UnOp { Ok(ASTNode::BinOp { lhs: Box::new($1?), rhs: Box::new($3?), op: BinOp::Mod}) }
	| MulDiv 'MUL' UnOp { Ok(ASTNode::BinOp { lhs: Box::new($1?), rhs: Box::new($3?), op: BinOp::Mul}) }
	| UnOp { $1 }
	;

UnOp -> Result<ASTNode, ParseError>:
	  'SUB' UnOp { Ok(ASTNode::UnOp { val: Box::new($2?), op: UnOp::Neg}) }
	| 'ADD' UnOp { Ok(ASTNode::UnOp { val: Box::new($2?), op: UnOp::Pos}) }
	| Dice { $1 }
	;

Dice -> Result<ASTNode, ParseError>:
	  'INTEGER' 'DICE' 'INTEGER' 'POSTFIXOPS' { Ok(ASTNode::Dice { count: $lexer.span_str($1?.span()).parse()?, size: $lexer.span_str($3?.span()).parse()?, modifiers: $lexer.span_str($4?.span()).parse()? }) }
	| Literal { $1 }
	;

Literal -> Result<ASTNode, ParseError>:
	  'INTEGER' { Ok(ASTNode::Literal { val: $lexer.span_str($1?.span()).parse()? }) }
	| Inner { $1 }
	;

Inner -> Result<ASTNode, ParseError>:
	  'LPAR' Group 'RPAR' 'POSTFIXOPS' { Ok(ASTNode::Group { modifiers: $lexer.span_str($4?.span()).parse()?, items: $2? }) }
	;

Group -> Result<Vec<ASTNode>, ParseError>:
	  Group 'COMMA' Sub { Ok(push_and_return($1?, $3?)) }
	| Sub { Ok(vec![$1?]) }
	;
%%
use crate::*;

fn push_and_return(mut vec: Vec<ASTNode>, value: ASTNode) -> Vec<ASTNode> {
    vec.push(value);
    vec
}
