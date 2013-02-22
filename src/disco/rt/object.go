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
}

type Expr interface {
	Eval(*Scope) (Value, error)
}

type Object struct {
	ID        uint64
	Value     Expr
	Behaviors map[string]Value

	Addr     Mailbox
	Messages []Message
}

func NewObject(val Expr) *Object {
	id := GenID(OBJECT)

	n := 8
	obj := &Object{ID: id, Value: val, Messages: make([]Message, n)}

	RT.Heap.Insert(id, obj)

	// still need to start a goroutine and share the channel
	// between the object and goroutine
	return obj
}

func (o *Object) String() string {
	return fmt.Sprintf("%s (0x%x)", o.Value, o.ID)
}

type Promise struct {
	ID    uint64
	Value uint64

	Messages  []Message
	Behaviors map[string]Value
}
