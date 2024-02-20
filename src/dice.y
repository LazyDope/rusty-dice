%start Sub
%%

Sub -> Result<ASTNode, ParseError>:
	  Sub 'SUB' Add { Ok(ASTNode::BinOp { lhs: Arc::new($1?), rhs: Arc::new($3?), op: BinOp::Sub}) }
	| Add { $1 }
	;

Add -> Result<ASTNode, ParseError>:
	  Add 'ADD' MulDiv { Ok(ASTNode::BinOp { lhs: Arc::new($1?), rhs: Arc::new($3?), op: BinOp::Add}) }
	| MulDiv { $1 }
	;
	  
MulDiv -> Result<ASTNode, ParseError>:
	  MulDiv 'DIV' UnOp { Ok(ASTNode::BinOp { lhs: Arc::new($1?), rhs: Arc::new($3?), op: BinOp::Div}) }
	| MulDiv 'INTDIV' UnOp { Ok(ASTNode::BinOp { lhs: Arc::new($1?), rhs: Arc::new($3?), op: BinOp::IntDiv}) }
	| MulDiv 'MOD' UnOp { Ok(ASTNode::BinOp { lhs: Arc::new($1?), rhs: Arc::new($3?), op: BinOp::Mod}) }
	| MulDiv 'MUL' UnOp { Ok(ASTNode::BinOp { lhs: Arc::new($1?), rhs: Arc::new($3?), op: BinOp::Mul}) }
	| UnOp { $1 }
	;

UnOp -> Result<ASTNode, ParseError>:
	  'SUB' UnOp { Ok(ASTNode::UnOp { expr: Arc::new($2?), op: UnOp::Neg}) }
	| 'ADD' UnOp { Ok(ASTNode::UnOp { expr: Arc::new($2?), op: UnOp::Pos}) }
	| Dice { $1 }
	;

Dice -> Result<ASTNode, ParseError>:
	  'INTEGER' 'DICE' 'INTEGER' 'POSTFIXOPS' {
		Ok(ASTNode::Dice {
			count: $lexer.span_str($1?.span()).parse()?,
			size: $lexer.span_str($3?.span()).parse()?,
			modifiers: $lexer.span_str($4?.span()).parse()?
		})
	  }
	| 'INTEGER' 'DICE' 'INTEGER' {
		Ok(ASTNode::Dice {
			count: $lexer.span_str($1?.span()).parse()?,
			size: $lexer.span_str($3?.span()).parse()?,
			modifiers: Default::default(),
		})
	  }
	| Literal { $1 }
	;

Literal -> Result<ASTNode, ParseError>:
	  'INTEGER' { Ok(ASTNode::Literal { val: $lexer.span_str($1?.span()).parse()? }) }
	| Inner { $1 }
	;

Inner -> Result<ASTNode, ParseError>:
	  'LPAR' Group 'RPAR' 'POSTFIXOPS' { Ok(ASTNode::Group { modifiers: $lexer.span_str($4?.span()).parse()?, items: $2? }) }
	| 'LPAR' Group 'RPAR' { Ok(ASTNode::Group { modifiers: Default::default(), items: $2? }) }
	;

Group -> Result<Vec<Arc<ASTNode>>, ParseError>:
	  Group 'COMMA' Sub { Ok(push_and_return($1?, $3?)) }
	| Sub { Ok(vec![Arc::new($1?)]) }
	;
%%
use crate::*;
use std::sync::Arc;

fn push_and_return(mut vec: Vec<Arc<ASTNode>>, value: ASTNode) -> Vec<Arc<ASTNode>> {
    vec.push(Arc::new(value));
    vec
}
