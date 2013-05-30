// Copyright (C) 2013 Mark Stahl

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.

// You should have received a copy of the GNU Affero General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

package parse

import (
	"soma/ast"
	"soma/rt"
	"soma/scan"
)

// array :=
//  '[' [expressions] ']'
//
func (p *Parser) parseArray() (a *ast.Array) {
	p.expect(scan.LBRACK)

	a = &ast.Array{Exprs: p.parseExpressions([]rt.Expr{})}

	p.expect(scan.RBRACK)
	return
}

// expressions :=
//  [expr ('.' expressions)*]*
//
func (p *Parser) parseExpressions(exprs []rt.Expr) []rt.Expr {
	if p.tok != scan.RBRACK {
		exprs = append(exprs, p.parseExpr())
	}
	
	switch p.tok {
	case scan.RBRACK:
		return exprs
	case scan.PERIOD:
		p.next()
		exprs = p.parseExpressions(exprs)
	default:
		p.error(p.pos, "expected expression, '.', or ']', found '%s'", p.lit)
		p.next()
	}
	return exprs
}