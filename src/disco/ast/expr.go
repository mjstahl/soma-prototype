// Copyright 2013 Mark Stahl. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the BSD-LICENSE file.

package ast

import (
	"disco/file"
	"strings"
)

type Expression interface {
	Visit()
}

type Expr struct {
	Start    file.Pos
	Receiver string
	Behavior string
	Args     []string
}

func (e *Expr) Visit() { }

func (e *Expr) String() string {
	s := "EXPR\t" + e.Receiver + " " + e.Behavior
	a := " (" + strings.Join(e.Args, ", ") + ")"

	return s + a
}
