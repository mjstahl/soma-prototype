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

type Expr interface {
	// The difference between Eval and Visit seems to only
	// be evident when dealing with Blocks.  Given that, my
	// my gut tells me there is a better way to do this.
	// (i.e. evaluate the same AST Nodes in two different
	// contexts)

	// Eval occurs when the objects representing the AST nodes
	// have already been created.
	//
	Eval(*Scope) (Value, error)

	// Visit occurs during the interpretation of the AST nodes
	// such as when the disco console receive an Expr to
	// evaluate.
	//
	Visit(*Scope) (Value, error)
}

type Value interface {
	OID() uint64
	Address() Mailbox
	LookupBehavior(string) Value
}

type Object struct {
	ID   uint64
	Addr Mailbox

	Expr Expr

	Scope     *Scope
	Behaviors map[string]Value
}

type Promise struct {
	ID   uint64
	Addr Mailbox

	Value     uint64
	Behaviors map[string]Value
}

type Mailbox chan Message

func StartObject(obj Value) {
	for {
		msg := <-obj.Address()
		msg.ForwardMessage(obj)
	}
}

func StartBehavior(obj Value) {
	for {
		msg := <-obj.Address()
		msg.ReceiveMessage(obj)
	}
}

func NewObject(val Expr, scope *Scope) *Object {
	id := NewID(OBJECT)

	n := 128
	obj := &Object{ID: id, Expr: val, Scope: scope, Addr: make(Mailbox, n)}

	RT.Heap.Insert(id, obj)

	return obj
}

func (o *Object) String() string {
	return fmt.Sprintf("%s (0x%x)", o.Expr, o.ID)
}

func (o *Object) OID() uint64 {
	return o.ID
}

func (o *Object) Address() Mailbox {
	return o.Addr
}

func (o *Object) LookupBehavior(name string) Value {
	return o.Behaviors[name]
}

func NewPromise() *Promise {
	id := NewID(PROMISE)

	n := 128
	promise := &Promise{ID: id, Addr: make(Mailbox, n), Behaviors: map[string]Value{}}

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

func (p *Promise) LookupBehavior(name string) Value {
	return p.Behaviors[name]
}
