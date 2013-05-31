package parse

import (
	"soma/ast"
	"soma/rt"
	"soma/scan"
)

// assignment :=
//   targets ':=' expressions
//
func (p *Parser) parseAssignment(first string) *ast.Assign {
	assign := &ast.Assign{}

	targets := []string{first}
	assign.Targets = p.parseAssignTargets(targets)

	p.expect(scan.ASSIGN)

	exprs := []rt.Expr{p.parseExpr()}
	assign.Exprs = p.parseAssignExprs(exprs)

	return assign
}

// targets :=
//   IDENT (, IDENT)*
func (p *Parser) parseAssignTargets(targets []string) []string {
	if p.tok != scan.COMMA {
		return targets
	}
	p.expect(scan.COMMA)
	targets = append(targets, p.expect(scan.IDENT))

	return p.parseAssignTargets(targets)
}

// expressions :=
//   expression (, expression)*
func (p *Parser) parseAssignExprs(exprs []rt.Expr) []rt.Expr {
	if p.tok != scan.COMMA {
		return exprs
	}
	p.expect(scan.COMMA)
	exprs = append(exprs, p.parseExpr())

	return p.parseAssignExprs(exprs)
}
