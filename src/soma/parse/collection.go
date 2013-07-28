package parse

import (
	"soma/ast"
	"soma/rt"
	"soma/scan"
)

// array :=
//  '[' [expressions] ']'
//
func (p *Parser) parseCollection() rt.Expr {
	p.expect(scan.LBRACK)

	var l = &ast.List{Exprs: p.parseListExprs([]rt.Expr{})}

	p.expect(scan.RBRACK)
	return l
}

// expressions :=
//  '[' [expression [',' expression]*] ']'
//
func (p *Parser) parseListExprs(exprs []rt.Expr) []rt.Expr {
	if p.tok != scan.RBRACK {
		exprs = append(exprs, p.parseExpr())
	}

	switch p.tok {
	case scan.RBRACK:
		return exprs
	case scan.COMMA:
		p.next()
		exprs = p.parseListExprs(exprs)
	default:
		p.error(p.pos, "expected expression, ',', or ']', found '%s'", p.lit)
		p.next()
	}
	return exprs
}
