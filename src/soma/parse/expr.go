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
//	primary [messages]
//    | assignment
//
func (p *Parser) parseExpr() rt.Expr {
	recv := p.parsePrimary()
	return p.parseMessages(recv)
}

// primary :=
//	IDENT | GLOBAL | block | paren
//
func (p *Parser) parsePrimary() (recv rt.Expr) {
	switch p.tok {
	case scan.IDENT:
		name := p.expect(scan.IDENT)
		recv = &ast.Local{Value: name}
	case scan.GLOBAL:
		name := p.expect(scan.GLOBAL)
		recv = &ast.Global{Value: name}
	case scan.LBRACE:
		recv = p.parseBlock()
	case scan.LPAREN:
		recv = p.parseParenExpr()
    default:
        p.error(p.pos, "expected IDENT, GLOBAL, (, or }, found %s (%s)", p.tok, p.lit)
        p.next()
	}

	return
}

func (p *Parser) isPrimary() bool {
	if p.tok == scan.IDENT || p.tok == scan.GLOBAL || p.tok == scan.LBRACE || p.tok == scan.LPAREN {
		return true
	}

	return false
}

// assignment :=
//  target ':=' expression
//
func (p *Parser) parseAssignment() *ast.Assign {
    return nil    
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

// messages := 
//	unary_message+ binary_message* [keyword_message]
//    |	binary_message+ [keyword_message]
//    | keyword_message
//	
func (p *Parser) parseMessages(recv rt.Expr) rt.Expr {
	switch {
	case p.tok == scan.GLOBAL || p.tok == scan.LBRACE:
		p.error(p.pos, "expected Unary, Binary, or Keyword message, found  %s (%s)", p.tok, p.lit)
	case p.tok == scan.IDENT:
		um := p.parseUnaryMessage(recv)
		return p.parseMessages(um)
	case p.tok == scan.BINARY:
		bm := p.parseBinaryMessage(recv)
		return p.parseMessages(bm)
	case p.tok == scan.KEYWORD:
		km := p.parseKeywordMessage(recv)
		return p.parseMessages(km)
	}

	return recv
}

// unary_message :=
//	IDENT
//
func (p *Parser) parseUnaryMessage(recv rt.Expr) (msg rt.Expr) {
	name := p.expect(scan.IDENT)
	msg = &ast.UnaryMessage{recv, name}

	return
}

// binary_message :=
//	BINARY binary_argument
func (p *Parser) parseBinaryMessage(recv rt.Expr) rt.Expr {
	name := p.expect(scan.BINARY)
	bm := &ast.BinaryMessage{recv, name, p.parseBinaryArgument()}

	return bm
}

// binary_argument :=
//	primary unary_message*
//
func (p *Parser) parseBinaryArgument() rt.Expr {
	primary := p.parsePrimary()
	return p.parseUnaryMessages(primary)
}

func (p *Parser) parseUnaryMessages(recv rt.Expr) rt.Expr {
	if p.tok != scan.IDENT {
		return recv
	}

	name := p.expect(scan.IDENT)
	msg := &ast.UnaryMessage{recv, name}

	return p.parseUnaryMessages(msg)
}

// keyword_message :=
//	(KEYWORD keyword_argument)+
//
func (p *Parser) parseKeywordMessage(recv rt.Expr) rt.Expr {
	km := &ast.KeywordMessage{Receiver: recv}

	for p.tok == scan.KEYWORD {
		km.Behavior = km.Behavior + p.expect(scan.KEYWORD)
		km.Args = append(km.Args, p.parseKeywordArgument())
	}

	return km
}

// keyword_argument :=
//	primary unary_message* binary_message*
//
func (p *Parser) parseKeywordArgument() rt.Expr {
	primary := p.parsePrimary()
	um := p.parseUnaryMessages(primary)
	bm := p.parseBinaryMessages(um)

	return bm
}

func (p *Parser) parseBinaryMessages(recv rt.Expr) rt.Expr {
	if p.tok != scan.BINARY {
		return recv
	}

	behavior := p.expect(scan.BINARY)
	msg := &ast.BinaryMessage{recv, behavior, p.parseBinaryArgument()}

	return p.parseBinaryMessages(msg)
}
