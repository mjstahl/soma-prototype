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
		return &Integer{negate, radix, mantissa}
	}

	mantissa, _ := strconv.ParseInt(literal, 10, 0)
	return &Integer{negate, 10, mantissa}
}

func (i *Integer) Eval(s *rt.Scope) rt.Value {
	return rt.NIL
}

func (i *Integer) Visit(s *rt.Scope) rt.Value {
	return i.Eval(s)
}
