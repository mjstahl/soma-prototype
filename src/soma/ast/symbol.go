package ast

import (
	"soma/rt"
)

type Symbol struct {
	Text string
}

func (sym *Symbol) Address() rt.Mailbox {
	return nil
}

func (sym *Symbol) Eval(s *rt.Scope) rt.Value {
	return sym
}

func (sym *Symbol) LookupBehavior(name string) rt.Value {
	return nil
}

func (sym *Symbol) OID() uint64 {
	return 0
}

func (sym *Symbol) Return(msg *rt.AsyncMsg) {}

func (sym *Symbol) Visit(s *rt.Scope) rt.Value {
	return sym.Eval(s)
}

func (sym *Symbol) String() string {
	return "$" + sym.Text
}