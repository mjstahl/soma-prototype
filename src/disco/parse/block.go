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
		default:
			// error case
		case p.tok == scan.COMMENT:
			expr = &ast.Comment{Text: p.lit}
		case p.tok == scan.NAME || p.tok == scan.IDENT:
			e := &ast.Expr{Receiver: &ast.Expr{}}
			expr = p.parseExpr(e)
		}

		b.Exprs = append(b.Exprs, expr)
		p.next()
	}

	return b
}
