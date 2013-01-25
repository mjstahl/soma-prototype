// Copyright 2013 Mark Stahl. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the BSD-LICENSE file.

package parse

import (
	"disco/ast"
	"disco/scan"
//	"fmt"
)

func (p *Parser) parseExtDefine() (def *ast.Define) {
	def = &ast.Define{Start: p.pos, Type: ast.EXT}

	p.next()
	lit := p.expect(scan.NAME)
	if lit != "" {
		def.Receiver = lit
	}

	switch {
	case p.tok == scan.IDENT:
		p.parseUnaryDef(def)
	case p.tok == scan.BINARY:
		p.parseBinaryDef(def)
	case p.tok == scan.KEYWORD:
		p.parseKeywordDef(def)
	}

	p.expect(scan.DEFINE)

	return
}

func (p *Parser) parseUnaryDef(def *ast.Define) {
	def.Behavior = p.lit
	p.next()
}

func (p *Parser) parseBinaryDef(def *ast.Define) {
	var args []string
	def.Behavior = p.lit

	p.next()
	
	lit := p.expect(scan.IDENT)
	if lit != "" {
		args = append(args, lit)
	}

	def.Args = args
}

func (p *Parser) parseKeywordDef(def *ast.Define) {
	var args []string
	for p.tok == scan.KEYWORD {
		def.Receiver = def.Receiver + p.lit
		
		p.next()

		lit := p.expect(scan.IDENT) 
		if lit != "" {
			args = append(args, lit)
		}
	}

	def.Args = args
}
