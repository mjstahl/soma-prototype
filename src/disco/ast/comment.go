// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the BSD-LICENSE file.

package ast

import (
	"disco/file"
)

type Comment struct {
	Quote file.Pos
	Text  string
}

func (c *Comment) Pos() file.Pos { return c.Quote }
func (c *Comment) End() file.Pos { return file.Pos(int(c.Quote) + len(c.Text)) }