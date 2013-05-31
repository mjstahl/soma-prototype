package ast

import (
	"soma/rt"
)

type Return struct {
	Exprs []rt.Expr
}

func (a *Return) Eval(s *rt.Scope) rt.Value {
	return rt.NIL
}

func (a *Return) Visit(s *rt.Scope) rt.Value {
	return rt.NIL
}
