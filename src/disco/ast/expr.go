// Copyright 2013 Mark Stahl. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the BSD-LICENSE file.

package ast

import (
	"disco/file"
)

type Expression interface {
	Visit()
	String() string
}

type Expr struct {
	Start    file.Pos
	Receiver Expression
	Behavior string
	Args     []Expression
}

func (e *Expr) Visit() {}

func (e *Expr) String() string {
	s := "EXPR\t" + e.Behavior + "\n"
	r := "  EXPR\t" + e.Receiver.String() + "\n"

	a := ""
	for _, e := range e.Args {
		a = a + "\n  " + e.String()
	}

	return s + r + a
}
