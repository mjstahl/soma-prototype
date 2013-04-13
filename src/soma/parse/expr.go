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

// expression :=
//	primary [messages cascaded_messages]
//    | assignment
//
func (p *Parser) parseExpr() rt.Expr {
	recv := p.parsePrimary()

	switch {
	case p.isMessageStart():
		msg := p.parseMessages(recv)
		if p.tok == scan.CASCADE {
			cascade := p.parseCascadeMessages(recv, []rt.Expr{msg})
			return &ast.Cascade{Messages: cascade}
		}
		return msg
	case p.isEndExpression():
		return recv
	default:
		p.error(p.pos, "expected unary, binary, or keyword message, found %s (%s)", p.tok, p.lit)
	}

	return recv
}

// primary :=
//	IDENT | GLOBAL | block | paren
//
func (p *Parser) parsePrimary() (recv rt.Expr) {
	switch p.tok {
	case scan.IDENT:
		name := p.expect(scan.IDENT)
		if p.tok == scan.COMMA || p.tok == scan.ASSIGN {
			recv = p.parseAssignment(name)
		} else {
			recv = &ast.Local{Value: name}
		}
	case scan.GLOBAL:
		name := p.expect(scan.GLOBAL)
		recv = &ast.Global{Value: name}
	case scan.LBRACE:
		recv = p.parseBlock()
	case scan.LPAREN:
		recv = p.parseParenExpr()
	default:
		p.error(p.pos, "expected an identifier, a '(', or a '}', found %s ('%s')", p.tok, p.lit)
		p.next()
	}
	return
}

func (p *Parser) isPrimary() bool {
	return p.tok == scan.IDENT || p.tok == scan.GLOBAL || p.tok == scan.LBRACE || p.tok == scan.LPAREN
}

// paren :=
// 	'(' expression ')'
//
func (p *Parser) parseParenExpr() (recv rt.Expr) {
	p.expect(scan.LPAREN)

	recv = p.parseExpr()

	p.expect(scan.RPAREN)

	return
}

func (p *Parser) isEndExpression() bool {
	return p.tok == scan.PERIOD || p.tok == scan.EOF || p.tok == scan.RBRACE || p.tok == scan.RPAREN
}
