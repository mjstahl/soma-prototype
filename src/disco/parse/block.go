// Copyright 2013 Mark Stahl. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can found in the BSD-LICENSE file.

package parse

import (
	"disco/ast"
	"disco/scan"
)

func (p *Parser) parseBlock(b *ast.Block) *ast.Block {
	p.next()

	var expr ast.Expression
	for p.tok != scan.RBRACK {
		switch {
		case p.tok == scan.BINARY || p.tok == scan.KEYWORD:
			// error condition
		case p.tok == scan.COMMENT:
			expr = &ast.Comment{Text: p.lit}
		case p.tok == scan.LBRACK:
			b := &ast.Block{}
			expr = p.parseBlock(b)
		case p.tok == scan.NAME || p.tok == scan.IDENT:
			l := &ast.Literal{Name: p.lit}
			expr = p.parseLiteral(l)
		}

		b.Exprs = append(b.Exprs, expr)
		p.next()
	}

	return b
}
