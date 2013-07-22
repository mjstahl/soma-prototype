package ast

import (
	"soma/rt"
)

type Attribute struct {
	Value string
}

func (a *Attribute) Address() rt.Mailbox {
	return nil
}

func (a *Attribute) Eval(s *rt.Scope) rt.Value {
	return a
}

func (a *Attribute) LookupBehavior(name string) rt.Value {
	return nil
}

func (a *Attribute) OID() uint64 {
	return 0
}

func (a *Attribute) Return(msg *rt.AsyncMsg) {}

func (a *Attribute) Visit(s *rt.Scope) rt.Value {
	return a.Eval(s)
}

func (a *Attribute) String() string {
	return a.Value
}
