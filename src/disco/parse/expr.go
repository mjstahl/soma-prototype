// Copyright 2013 Mark Stahl. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the BSD-LICENSE file.

package parse

import (
	"disco/ast"
	"disco/scan"
)

func (p *Parser) parseExpr(exp ast.Expression) (expr ast.Expression) {
	switch p.tok {
	default:
		// bad expression, expected a identifier, binary, keyword, or period 
	case scan.PERIOD:
		return exp
	case scan.RBRACK:
		//p.parseBlock()
	case scan.IDENT:
		
	case scan.KEYWORD:

	case scan.BINARY:
	}

	p.next()
	return //p.parseExpr(e)
}
