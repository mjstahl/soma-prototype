package parse

import (
	"soma/ast"
	"soma/rt"
	"soma/scan"
)

// block :=
//	'{' [arguments] [statements] '}'
//
func (p *Parser) parseBlock() (b *ast.Block) {
	p.expect(scan.LBRACE)

	b = &ast.Block{}
	bargs := []string{"self", "this"}
	if p.tok == scan.BINARY && p.lit == "|" {
		args := p.parseBlockArguments()
		b.Args = append(bargs, args...)
	} else {
		b.Args = bargs
	}
	var stmts []rt.Expr
	b.Statements = p.parseStatements(stmts)

	p.expect(scan.RBRACE)
	return
}

// arguments :=
//   '|' IDENT (',' IDENT)* '|'
//
func (p *Parser) parseBlockArguments() []string {
	p.expect(scan.BINARY)

	args := []string{p.expect(scan.IDENT)}
	for p.tok != scan.BINARY && p.lit != "|" {
		period := p.expect(scan.COMMA)
		if period != "," {
			break
		}
		args = append(args, p.expect(scan.IDENT))
	}
	p.expect(scan.BINARY)
	return args
}

// statements :=
//     [comment statements]
// |   [expression ('.' statements)*]
//
func (p *Parser) parseStatements(stmts []rt.Expr) []rt.Expr {
	expr := p.parseExpr()
	if _, ok := expr.(*ast.Comment); ok {
		stmts = p.parseStatements(append(stmts, expr))
	} else {
		stmts = append(stmts, expr)
		switch p.tok {
		case scan.RBRACE:
			return stmts
		case scan.PERIOD:
			p.next()
			stmts = p.parseStatements(stmts)
		default:
			p.error(p.pos, "expected expression, '.', or '}', found '%s'", p.lit)
			p.next()
		}
	}
	return stmts
}
