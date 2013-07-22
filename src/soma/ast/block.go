package ast

import (
	"soma/rt"
)

type Block struct {
	Args       []string
	Statements []rt.Expr
}

func NewBlock(b *Block, s *rt.Scope) rt.Value {
	base := rt.NewScope(nil)
	for _, arg := range b.Args {
		base.Insert(arg, 0)
	}

	scope := base.AppendScope(s)
	obj := rt.CreateObject(b, scope, 0)
	return obj
}

func (b *Block) Eval(s *rt.Scope) rt.Value {
	var value rt.Value
	for _, stmt := range b.Statements {
		value = stmt.Eval(s)
	}

	if value == nil {
		return rt.NIL
	}
	return value
}

func (b *Block) Visit(s *rt.Scope) rt.Value {
	obj := NewBlock(b, s)
	go rt.StartBehavior(obj)

	return obj
}

func (b *Block) String() string {
	return "Block"
}
