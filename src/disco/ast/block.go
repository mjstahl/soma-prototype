// Copyright 2013 Mark Stahl. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the BSD-LICENSE file.

package ast

type Block struct {
	Exprs []Expression
}

func (b *Block) Visit() {}

func (b *Block) String() string {
	s := "BLOCK\t{ ... }"

	var e string
	for _, exp := range b.Exprs {
		e = "\n  " + exp.String()
	}

	return s + e
}
