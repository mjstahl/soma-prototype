package ast

import (
	"soma/rt"
)

type String struct {
	Text string
}

func (str *String) Address() rt.Mailbox {
	return nil
}

func (str *String) Eval(s *rt.Scope) rt.Value {
	return str
}

func (str *String) LookupBehavior(name string) rt.Value {
	return nil
}

func (str *String) OID() uint64 {
	return 0
}

func (str *String) Return(msg *rt.AsyncMsg) {}

func (str *String) Visit(s *rt.Scope) rt.Value {
	return str.Eval(s)
}

func (str *String) String() string {
	return str.Text
}
