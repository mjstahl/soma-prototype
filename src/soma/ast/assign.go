package ast

import (
	"soma/rt"
)

type Assign struct {
	Targets []string
	Exprs   []rt.Expr
}

func (a *Assign) Eval(s *rt.Scope) rt.Value {
	lvar, lval := len(a.Targets), len(a.Exprs)
	var v rt.Value
	if lvar < lval {
		for index, target := range a.Targets {
			val := a.Exprs[index].Visit(s)
			s.Insert(target, val.OID())
		}

		for _, expr := range a.Exprs[lvar:] {
			v = expr.Visit(s)
		}
	} else {
		for index, expr := range a.Exprs {
			val := expr.Visit(s)
			s.Insert(a.Targets[index], val.OID())
			v = val
		}

		for _, target := range a.Targets[lval:] {
			s.Insert(target, rt.NIL.OID())
		}
	}

	return v
}

func (a *Assign) Visit(s *rt.Scope) rt.Value {
	return a.Eval(s)
}
