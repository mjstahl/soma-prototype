// Copyright 2013 Mark Stahl. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can found in the BSD-LICENSE file.

package parse

import (
	"disco/ast"
	"disco/scan"
)

func (p *Parser) parseLiteral(l *ast.Literal) ast.Expression {
	p.next()

	var expr ast.Expression = l
	for p.tok != scan.PERIOD {
		switch {
		default:
			return nil // error case
		case p.tok == scan.IDENT:
			expr = &ast.Expr{Receiver: expr, Behavior: p.lit}
		case p.tok == scan.BINARY:

		case p.tok == scan.KEYWORD:

		}

		p.next()
	}

	return expr
}
