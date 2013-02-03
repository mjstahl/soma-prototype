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
func (p *Parser) parseExpr() (expr ast.Expr) {
	recv := p.parsePrimary()
	expr = p.parseMessages(recv)

	return
}

// primary :=
//	IDENT | NAME | block
//
func (p *Parser) parsePrimary() (recv ast.Expr) {
	switch p.tok {
	case scan.IDENT:
		recv = &ast.Variable{Name: p.lit}
		p.next()
	case scan.NAME:
		recv = &ast.Object{Name: p.lit}
		p.next()
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
	fmt.Printf("%s\n", recv)

	switch p.tok {
	case scan.IDENT:
		um := p.parseUnaryMessage(recv)
		return p.parseMessages(um)
	}

	return recv
}

// unary_message :=
//	IDENT
//
func (p *Parser) parseUnaryMessage(recv ast.Expr) (msg ast.Message) {
	lit := p.lit

	p.expect(scan.IDENT)
	msg = &ast.UnaryMessage{Behavior: lit}

	msg.SetReceiver(recv)
	return
}
