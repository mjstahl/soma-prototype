package ast

import (
	"soma/rt"
)

type UnaryMessage struct {
	Receiver rt.Expr
	Behavior string
}

func (ue *UnaryMessage) Eval(s *rt.Scope) rt.Value {
	return rt.SendMessage(ue.Receiver, ue.Behavior, []rt.Expr{}, s)
}

func (ue *UnaryMessage) Visit(s *rt.Scope) rt.Value {
	return ue.Eval(s)
}

type BinaryMessage struct {
	Receiver rt.Expr
	Behavior string
	Arg      rt.Expr
}

func (be *BinaryMessage) Eval(s *rt.Scope) rt.Value {
	return rt.SendMessage(be.Receiver, be.Behavior, []rt.Expr{be.Arg}, s)
}

func (be *BinaryMessage) Visit(s *rt.Scope) rt.Value {
	return be.Eval(s)
}

type KeywordMessage struct {
	Receiver rt.Expr
	Behavior string
	Args     []rt.Expr
}

func (ke *KeywordMessage) Eval(s *rt.Scope) rt.Value {
	return rt.SendMessage(ke.Receiver, ke.Behavior, ke.Args, s)
}

func (ke *KeywordMessage) Visit(s *rt.Scope) rt.Value {
	return ke.Eval(s)
}
