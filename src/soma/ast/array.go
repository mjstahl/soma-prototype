package ast

import (
	"fmt"
	"soma/rt"
	"strings"
)

type Array struct {
	Exprs  []rt.Expr
	Values []rt.Value
}

func NewArray(a *Array, s *rt.Scope) rt.Value {
	for _, expr := range a.Exprs {
		a.Values = append(a.Values, expr.Eval(s))
	}

	obj := rt.CreateObject(a, s, 0)
	return obj
}

func (a *Array) Eval(s *rt.Scope) rt.Value {
	obj := NewArray(a, s)
	go rt.StartBehavior(obj)

	return obj
}

func (a *Array) Visit(s *rt.Scope) rt.Value {
	return a.Eval(s)
}

func (a *Array) String() string {
	if len(a.Values) == 0 {
		return "Array"
	}

	var reprs []string
	for index, repr := range a.Values {
		reprs = append(reprs, fmt.Sprintf("%d", index)+" => "+repr.String())
	}
	return fmt.Sprintf("%s", strings.Join(reprs, "\n    "))
}
