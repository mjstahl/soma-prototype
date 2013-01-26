// Copyright 2013 Mark Stahl. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the BSD-LICENSE file.

package parse

import (
	"disco/ast"
	"disco/scan"
)

func (p *Parser) parseExpr(exp ast.Expression) ast.Expression {
	switch {
	default:
		// bad expression, expected a identifier, binary, keyword, or period
		return exp
	case p.tok == scan.PERIOD || p.tok == scan.RBRACK:
		return exp
	case p.tok == scan.NAME:
		e := &ast.Literal{Start: p.pos, Name: p.lit}
		return e
/**
		if exp.Receiver == nil {
			exp.Receiver = e
			return exp
		} 
		if exp.Receiver != nil && exp.Behavior != nil {
			exp.Args = append(exp.Args, e)
			return exp
		}
		if exp.Receiver != nil && exp.Behavior == nil {
			// error condition, NAME cannot not be part of a behavior
			return exp
		}
**/
	case p.tok == scan.IDENT:
/**
		if exp.Receiver != nil {
			exp.Behavior = p.lit
			return exp
		} else {	
			e := &ast.Expr{Start: p.pos, Receiver: exp}
			return e
		}
**/
	case p.tok == scan.BINARY:
		e := &ast.Expr{Start: p.pos, Receiver: exp, Behavior: p.lit}
		p.next()

		a := &ast.Expr{Start: p.pos}
		e.Args = append(e.Args, p.parseExpr(a))
		return e
	case p.tok == scan.KEYWORD:
		e := &ast.Expr{Start: p.pos, Receiver: exp}
		e.Behavior = e.Behavior + p.lit
		p.next()

		a := &ast.Expr{Start: p.pos}
		e.Args = append(e.Args, p.parseExpr(a))
		return e
	}

	return exp
}
