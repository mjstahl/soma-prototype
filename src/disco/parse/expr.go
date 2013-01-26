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
	case scan.PERIOD:
		return
	case scan.RBRACK:
		return
	case scan.IDENT:
	case scan.NAME:
	case scan.LBRACK:
	}

	return
}
