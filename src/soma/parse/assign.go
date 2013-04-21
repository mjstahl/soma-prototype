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

// assignment :=
//   targets ':=' expressions
//
func (p *Parser) parseAssignment(first string) *ast.Assign {
	assign := &ast.Assign{}

	targets := []string{first}
	assign.Targets = p.parseAssignTargets(targets)

	p.expect(scan.ASSIGN)

	exprs := []rt.Expr{p.parseExpr()}
	assign.Exprs = p.parseAssignExprs(exprs)

	return assign
}

// targets :=
//   IDENT (, IDENT)*
func (p *Parser) parseAssignTargets(targets []string) []string {
	if p.tok != scan.COMMA {
		return targets
	}
	p.expect(scan.COMMA)
	targets = append(targets, p.expect(scan.IDENT))

	return p.parseAssignTargets(targets)
}

// expressions :=
//   expression (, expression)*
func (p *Parser) parseAssignExprs(exprs []rt.Expr) []rt.Expr {
	if p.tok != scan.COMMA {
		return exprs
	}
	p.expect(scan.COMMA)
	exprs = append(exprs, p.parseExpr())

	return p.parseAssignExprs(exprs)
}
