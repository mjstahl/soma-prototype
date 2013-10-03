package parse

import (
	"soma/ast"
	"soma/scan"
)

// assignment :=
//   IDENT ':=' expression
//
func (p *Parser) parseAssignment(first string) *ast.Assign {
	assign := &ast.Assign{}
	assign.Target = first

	p.expect(scan.ASSIGN)

	assign.Expr = p.parseExpr()
	return assign
}
