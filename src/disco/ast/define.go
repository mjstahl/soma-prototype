// Copyright 2013 Mark Stahl. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the BSD-LICENSE file.

package ast

type Define struct {
	Receiver string   // True, False, Nil
	Behavior string   // not, ifTrue:ifFalse:
	Args     []string // tBlock, fBlock
	Body     *Block
}

func (d *Define) Visit() {}
