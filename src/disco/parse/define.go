// Copyright 2013 Mark Stahl. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the BSD-LICENSE file.

package parse

import (
	"disco/ast"
	"disco/scan"
//	"fmt"
)

func (p *Parser) parseDefine(def *ast.Define) *ast.Define {
	p.next()

	lit := p.expect(scan.NAME)
	if lit != "" {
		def.Receiver = lit
	}

	switch {
	case p.tok == scan.IDENT:
		def.Behavior = p.parseUnaryDef()
	case p.tok == scan.BINARY:
		def.Behavior, def.Args = p.parseBinaryDef()
	case p.tok == scan.KEYWORD:
		def.Behavior, def.Args = p.parseKeywordDef()
	}

	p.expect(scan.DEFINE)
	
	for p.tok != scan.PERIOD || p.tok != scan.EOF {
		switch {
		case p.tok == scan.NAME || p.tok == scan.IDENT:
			l := &ast.Literal{Name: p.lit}
			def.Exprs = append(def.Exprs, p.parseLiteral(l))
		case p.tok == scan.LBRACK:
			b := &ast.Block{}
			def.Exprs = append(def.Exprs, p.parseBlock(b))
		default:
			return def
		}

		p.next()
	}

	return def
}

func (p *Parser) parseUnaryDef() string {
	lit := p.lit
	p.next()

	return lit
}

func (p *Parser) parseBinaryDef() (string, []string) {
	var args []string
	lit := p.lit

	p.next()

	arg := p.expect(scan.IDENT)
	if arg != "" {
		args = append(args, arg)
	}

	return lit, args
}

func (p *Parser) parseKeywordDef() (string, []string) {
	var lit string
	var args []string
	for p.tok == scan.KEYWORD {
		lit = lit + p.lit

		p.next()

		arg := p.expect(scan.IDENT)
		if arg != "" {
			args = append(args, arg)
		}
	}

	return lit, args
}
