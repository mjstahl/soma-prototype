package ast

import (
	"fmt"
	"soma/rt"
	"strings"
)

type List struct {
	Exprs  []rt.Expr
	Values []rt.Value
}

func NewList(l *List, s *rt.Scope) rt.Value {
	for _, expr := range l.Exprs {
		l.Values = append(l.Values, expr.Eval(s))
	}

	obj := rt.CreateObject(l, s, 0)
	return obj
}

func (l *List) Eval(s *rt.Scope) rt.Value {
	obj := NewList(l, s)
	go rt.StartBehavior(obj)

	return obj
}

func (l *List) Visit(s *rt.Scope) rt.Value {
	return l.Eval(s)
}

func (l *List) String() string {
	if len(l.Values) == 0 {
		return "[]"
	}

	var reprs []string
	for _, repr := range l.Values {
		reprs = append(reprs, repr.String())
	}
	return fmt.Sprintf("[%s]", strings.Join(reprs, ". "))
}
