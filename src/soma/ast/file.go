package ast

import (
	"soma/rt"
)

type File struct {
	Exprs []rt.Expr
}

func (f *File) Eval(s *rt.Scope) rt.Value {
	var val rt.Value
	for _, expr := range f.Exprs {
		val = expr.Eval(s)
	}

	return val
}

func (f *File) Visit(s *rt.Scope) rt.Value {
	var val rt.Value
	for _, expr := range f.Exprs {
		val = expr.Visit(s)
	}

	return val
}
