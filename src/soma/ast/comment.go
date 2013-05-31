package ast

import (
	"soma/rt"
)

type Comment struct {
	Text string
}

func (c *Comment) Address() rt.Mailbox {
	return nil
}

func (c *Comment) Eval(s *rt.Scope) rt.Value {
	return c
}

func (c *Comment) LookupBehavior(name string) rt.Value {
	return nil
}

func (c *Comment) OID() uint64 {
	return 0
}

func (c *Comment) Return(msg *rt.AsyncMsg) {}

func (c *Comment) Visit(s *rt.Scope) rt.Value {
	return c.Eval(s)
}

func (c *Comment) String() string {
	return c.Text
}
