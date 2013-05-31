package ast

import (
	"soma/rt"
)

type Cascade struct {
	Messages []rt.Expr
}

func (c *Cascade) Eval(s *rt.Scope) rt.Value {
	var value rt.Value
	for _, val := range c.Messages {
		value = val.Visit(s)
	}
	return value
}

func (c *Cascade) Visit(s *rt.Scope) rt.Value {
	return c.Eval(s)
}
