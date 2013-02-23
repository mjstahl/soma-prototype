// Copyright (C) 2013 Mark Stahl

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.

// You should have received a copy of the GNU Affero General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

package rt

import (
	"fmt"
)

type Mailbox chan Message

type Value interface {
	OID() uint64
	Address() Mailbox
}

type Expr interface {
	Eval(*Scope) (Value, error)
}

type Object struct {
	ID   uint64
	Addr Mailbox

	Value     Expr

	Scope *Scope
	Behaviors map[string]Value
}

func (o *Object) OID() uint64 {
	return o.ID
}

func (o *Object) Address() Mailbox {
	return o.Addr
}

func NewObject(val Expr, scope *Scope) *Object {
	id := NewID(OBJECT)

	n := 128
	obj := &Object{ID: id, Value: val, Scope: scope, Addr: make(Mailbox, n)}

	RT.Heap.Insert(id, obj)

	return obj
}

func (o *Object) String() string {
	return fmt.Sprintf("%s (0x%x)", o.Value, o.ID)
}

type Promise struct {
	ID   uint64
	Addr Mailbox

	Value     uint64
	Behaviors map[string]Value
}

func NewPromise() *Promise {
	id := NewID(PROMISE)

	n := 128
	promise := &Promise{ID: id, Addr: make(Mailbox, n)}

	RT.Heap.Insert(id, promise)
	go StartObject(promise)

	return promise
}

func (p *Promise) String() string {
	return fmt.Sprintf("Promise (0x%x)", p.ID)
}

func (p *Promise) OID() uint64 {
	return p.ID
}

func (p *Promise) Address() Mailbox {
	return p.Addr
}

func StartObject(obj Value) {
	for {
		msg := <-obj.Address()
		msg.ReceiveMessage(obj)
	}
}
