// Copyright 2013 Mark Stahl. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the BSD-LICENSE file.

package parse

import (
	"disco/ast"
	"disco/rt"
	"disco/scan"
)

// expression :=
//	primary [messages]
//
func (p *Parser) parseExpr() rt.Expr {
	primary := p.parsePrimary()
	return p.parseMessages(primary)
}

// primary :=
//	IDENT | NAME | block
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
	}

	return
}

func (p *Parser) isPrimary() bool {
	if p.tok == scan.IDENT || p.tok == scan.GLOBAL || p.tok == scan.LBRACE {
		return true
	}

	return false
}

// messages := 
//	unary_message+ binary_message* [keyword_message]
//    |	binary_message+ [keyword_message]
//    | keyword_message
//	
func (p *Parser) parseMessages(recv rt.Expr) rt.Expr {
	switch {
	case p.tok == scan.GLOBAL || p.tok == scan.LBRACE:
		p.error(p.pos, "expected IDENT, GLOBAL, or KEYWORD, found  %s (%s)", p.tok, p.lit)
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
	km := &ast.KeywordMessage{Recvr: recv}

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
