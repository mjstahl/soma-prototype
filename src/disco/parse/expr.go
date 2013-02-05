// Copyright 2013 Mark Stahl. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the BSD-LICENSE file.

package parse

import (
	"disco/ast"
	"disco/scan"
	"fmt"
)

// expression :=
//	primary [messages]
//
func (p *Parser) parseExpr() ast.Expr {
	primary := p.parsePrimary()
	return p.parseMessages(primary)
}

// primary :=
//	IDENT | NAME | block
//
func (p *Parser) parsePrimary() (recv ast.Expr) {
	switch p.tok {
	case scan.IDENT:
		recv = &ast.Noun{Name: p.expect(scan.IDENT)}
	case scan.NAME:
		recv = &ast.Name{Name: p.expect(scan.NAME)}
	case scan.LBRACE:
		recv = p.parseBlock()
	}

	return
}

// messages := 
//	unary_message+ binary_message* [keyword_message]
//    |	binary_message+ [keyword_message]
//    | keyword_message
//	
func (p *Parser) parseMessages(recv ast.Expr) ast.Expr {
	fmt.Printf("%#v\n", recv)

	switch p.tok {
	case scan.IDENT:
		um := p.parseUnaryMessage(recv)
		return p.parseMessages(um)
	case scan.BINARY:
		bm := p.parseBinaryMessage(recv)
		return p.parseMessages(bm)
	case scan.KEYWORD:
		km := p.parseKeywordMessage(recv)
		return p.parseMessages(km)
	}

	return recv
}

// unary_message :=
//	IDENT
//
func (p *Parser) parseUnaryMessage(recv ast.Expr) (msg ast.Expr) {
	name := p.expect(scan.IDENT)
	msg = &ast.UnaryMessage{Receiver: recv, Behavior: name}

	return
}

// binary_message :=
//	BINARY binary_argument
func (p *Parser) parseBinaryMessage(recv ast.Expr) ast.Expr {
	name := p.expect(scan.BINARY)

	bm := &ast.BinaryMessage{Receiver: recv, Behavior: name}
	bm.Arg = p.parseBinaryArgument()

	return bm
}

// binary_argument :=
//	primary unary_message*
//
func (p *Parser) parseBinaryArgument() ast.Expr {
	primary := p.parsePrimary()
	return p.parseUnaryMessages(primary)
}

func (p *Parser) parseUnaryMessages(recv ast.Expr) ast.Expr {
	if p.tok != scan.IDENT {
		return recv
	}

	name := p.expect(scan.IDENT)
	msg := &ast.UnaryMessage{Receiver: recv, Behavior: name}

	return p.parseUnaryMessages(msg)
}

// keyword_message :=
//	(KEYWORD keyword_argument)+
//
func (p *Parser) parseKeywordMessage(recv ast.Expr) ast.Expr {
	km := &ast.KeywordMessage{Receiver: recv}
	km = p.parseKeywordMessageParts(km)

	for p.tok == scan.KEYWORD {
		km = p.parseKeywordMessageParts(km)
	}

	return km
}

func (p *Parser) parseKeywordMessageParts(km *ast.KeywordMessage) *ast.KeywordMessage {
	km.Behavior = km.Behavior + p.expect(scan.KEYWORD)
	km.Args = append(km.Args, p.parseKeywordArgument())

	return km
}

// keyword_argument :=
//	primary unary_message* binary_message*
//
func (p *Parser) parseKeywordArgument() ast.Expr {
	primary := p.parsePrimary()
	um := p.parseUnaryMessages(primary)
	bm := p.parseBinaryMessages(um)

	return bm
}

func (p *Parser) parseBinaryMessages(recv ast.Expr) ast.Expr {
	if p.tok != scan.BINARY {
		return recv
	}

	name := p.expect(scan.BINARY)
	msg := &ast.BinaryMessage{Receiver: recv, Behavior: name, Arg: p.parseBinaryArgument()}

	return p.parseBinaryMessages(msg)
}
