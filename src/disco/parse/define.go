// Copyright 2013 Mark Stahl. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the BSD-LICENSE file.

package parse

import (
	"disco/ast"
	"disco/scan"
)

// define := 
//	"+" NAME message_pattern DEFINE block
// message_pattern := 
//	unary_define | binary_define | keyword_define
// 
func (p *Parser) parseDefine() *ast.Define {
	p.expect(scan.BINARY)
	lit := p.expect(scan.NAME)

	var (
		behavior string
		args     []string
	)

	switch {
	case p.tok == scan.IDENT:
		behavior = p.parseUnaryDef()
	case p.tok == scan.BINARY:
		behavior, args = p.parseBinaryDef()
	case p.tok == scan.KEYWORD:
		behavior, args = p.parseKeywordDef()
	}

	p.expect(scan.DEFINE)
	body := p.parseBlock()

	bargs := []string{"self"}
	body.Args = append(bargs, args...)

	return &ast.Define{lit, behavior, args, body}
}

func (p *Parser) isExternalDefine() bool {
	return p.tok == scan.BINARY && p.lit == "+"
}

// unary_define :=
//	IDENT
//
func (p *Parser) parseUnaryDef() string {
	return p.expect(scan.IDENT)
}

// binary_define :=
//	BINARY IDENT
//
func (p *Parser) parseBinaryDef() (lit string, args []string) {
	lit = p.expect(scan.BINARY)
	args = append(args, p.expect(scan.IDENT))

	return
}

// keyword_define :=
//	(KEYWORD IDENT)+
//
func (p *Parser) parseKeywordDef() (lit string, args []string) {
	for p.tok == scan.KEYWORD {
		lit = lit + p.expect(scan.KEYWORD)

		arg := p.expect(scan.IDENT)
		if arg != "" {
			args = append(args, arg)
		}
	}

	return
}
