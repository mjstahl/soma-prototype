package parse

import (
	"soma/ast"
	"soma/rt"
	"soma/scan"
)

// array :=
//  '[' [expressions] ']'
//
func (p *Parser) parseArray() (a *ast.Array) {
	p.expect(scan.LBRACK)

	a = &ast.Array{Exprs: p.parseArrayExprs([]rt.Expr{})}

	p.expect(scan.RBRACK)
	return
}

// expressions :=
//  '[' [expression [. expression]]* ']'
//
func (p *Parser) parseArrayExprs(exprs []rt.Expr) []rt.Expr {
	if p.tok != scan.RBRACK {
		exprs = append(exprs, p.parseExpr())
	}

	switch p.tok {
	case scan.RBRACK:
		return exprs
	case scan.PERIOD:
		p.next()
		exprs = p.parseArrayExprs(exprs)
	default:
		p.error(p.pos, "expected expression, '.', or ']', found '%s'", p.lit)
		p.next()
	}
	return exprs
}
