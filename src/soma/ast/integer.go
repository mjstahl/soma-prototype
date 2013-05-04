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

package ast

import (
	"soma/rt"
	"strconv"
	"strings"
)

type Integer struct {
	Literal  string
	Negative bool
	Radix    int
	Value    int64
}

func NewInteger(literal string) *Integer {
	negate, number := false, literal
	if literal[0] == '-' {
		negate, number = true, literal[1:]
	}

	parts := strings.Split(number, "#")
	if len(parts) > 1 {
		radix, _ := strconv.Atoi(parts[0])
		mantissa, _ := strconv.ParseInt(parts[1], radix, 0)
		return &Integer{literal, negate, radix, mantissa}
	}

	mantissa, _ := strconv.ParseInt(literal, 10, 0)
	return &Integer{literal, negate, 10, mantissa}
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
	return rt.INTEGER.Address()
}

func (i *Integer) LookupBehavior(name string) rt.Value {
	return rt.INTEGER.LookupBehavior(name)
}

func (i *Integer) OID() uint64 {
	return (uint64)((i.Value << 8) | ((int64)(i.Radix << 4)) | 0x7)
}

func (i *Integer) Return(am *rt.AsyncMsg) {
	rt.INTEGER.Return(am)
	return
}

func (i *Integer) String() string {
	return i.Literal
}
