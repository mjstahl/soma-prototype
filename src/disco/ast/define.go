// Copyright 2013 Mark Stahl. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the BSD-LICENSE file.

package ast

import (
	"disco/file"
	"strings"
)

type DefineType int

const (
	EXT DefineType = iota
)

type Define struct {
	Start    file.Pos
	Type     DefineType // External, Internal
	Receiver string     // True, False, Nil
	Behavior string     // not, ifTrue:ifFalse:
	Args     []string   // tBlock, fBlock
	//Body   []*Expr
}

func (d *Define) Visit() {}

func (d *Define) String() string {
	s := "DEFINE\t" + d.Receiver + " " + d.Behavior
	a := " (" + strings.Join(d.Args, ", ") + ")"

	return s + a
}
