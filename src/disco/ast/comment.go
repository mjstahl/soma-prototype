// Copyright 2013 Mark Stahl. All rights reserved.
// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the BSD-LICENSE file.

package ast

import (
	"disco/file"
)

type Comment struct {
	Start file.Pos
	Text  string
}

func (c *Comment) Visit() {}

func (c *Comment) String() string {
	return "COMMENT\t" + c.Text
}
