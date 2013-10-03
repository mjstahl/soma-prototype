package ast

import (
	"soma/rt"
)

type Assign struct {
	Target string
	Expr   rt.Expr
}

func (a *Assign) Eval(s *rt.Scope) rt.Value {
	value := a.Expr.Visit(s)
	s.Insert(a.Target, value.OID())

	return value
}

func (a *Assign) Visit(s *rt.Scope) rt.Value {
	return a.Eval(s)
}
