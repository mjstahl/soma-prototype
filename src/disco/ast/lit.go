// Copyright 2013 Mark Stahl. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the BSD-LICENSE file.

package ast

type Literal struct {
	Name string
}

func (l *Literal) Visit() {}

func (l *Literal) String() string {
	return "LIT\t" + l.Name
}
