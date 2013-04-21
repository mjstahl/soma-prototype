// Copyright (C) 2013 Mark Stahl

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.

// You should have received a copy of the GNU Affero General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

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
//   '|' IDENT (. IDENT)* '|'
//
func (p *Parser) parseBlockArguments() []string {
	p.expect(scan.BINARY)

	args := []string{p.expect(scan.IDENT)}
	for p.tok != scan.BINARY && p.lit != "|" {
		period := p.expect(scan.PERIOD)
		if period != "." {
			break
		}
		args = append(args, p.expect(scan.IDENT))
	}
	p.expect(scan.BINARY)
	return args
}

// statements :=
//	[expression ('.' statements)*]
//
func (p *Parser) parseStatements(stmts []rt.Expr) []rt.Expr {
	stmts = append(stmts, p.parseExpr())
	
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
	return stmts
}
