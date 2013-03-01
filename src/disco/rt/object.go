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

type Object struct {
	ID   uint64
	Addr Mailbox

	Expr Expr

	Scope     *Scope
	Behaviors map[string]uint64
}

func CreateObject(val Expr, scope *Scope) *Object {
	id := NewID(OBJECT)

	n := 128
	obj := &Object{ID: id, Expr: val, Scope: scope, Addr: make(Mailbox, n), Behaviors: map[string]uint64{}}

	RT.Heap.Insert(id, obj)

	return obj
}

func (obj *Object) New() {
	for {
		msg := <-obj.Address()
		msg.ForwardMessage(obj)
	}
}

func (o *Object) Return(am *AsyncMsg) {
	promise := RT.Heap.Lookup(am.PromisedTo)
	async := &AsyncMsg{[]uint64{promise.OID(), o.OID()}, "value:", 0}
	promise.Address() <- async
}

func (o *Object) String() string {
	return fmt.Sprintf("%s (0x%x @ %s)", o.Expr, (o.ID & 0x7FFFFFFF), RT.IPAddr)
}

func (o *Object) OID() uint64 {
	return o.ID
}

func (o *Object) Address() Mailbox {
	return o.Addr
}

func (o *Object) LookupBehavior(name string) Value {
	oid := o.Behaviors[name]
	return RT.Heap.Lookup(oid)
}

func StartBehavior(obj Value) {
	for {
		msg := <-obj.Address()

		am := msg.(*AsyncMsg)
		ReceiveMessage(obj, am)
	}
}
