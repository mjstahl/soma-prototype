package ast

import (
	"soma/rt"
	"strconv"
)

type Integer struct {
	Value int64
}

func NewInteger(literal string) *Integer {
	mantissa, _ := strconv.ParseInt(literal, 10, 64)
	return &Integer{mantissa}
}

// Methods to satisfy the rt.Expr interface
func (i *Integer) Eval(s *rt.Scope) rt.Value {
	if i.Value > (-1<<55) && i.Value < (1<<55-1) {
		return i
	}
	return rt.NIL
}

func (i *Integer) Visit(s *rt.Scope) rt.Value {
	return i.Eval(s)
}

// Methods to satisfy the rt.Value interface
func (i *Integer) Address() rt.Mailbox {
	return rt.INT.Address()
}

func (i *Integer) LookupBehavior(name string) rt.Value {
	return rt.INT.LookupBehavior(name)
}

func (i *Integer) OID() uint64 {
	id := (i.Value << 8) | 0x7
	return uint64(id)
}

func (i *Integer) Return(am *rt.AsyncMsg) {
	promise := rt.RT.Heap.Lookup(am.PromisedTo)
	async := &rt.AsyncMsg{[]uint64{promise.OID(), 0, i.OID()}, "value:", 0}
	promise.Address() <- async
}

func (i *Integer) String() string {
	return strconv.FormatInt(i.Value, 10)
}
