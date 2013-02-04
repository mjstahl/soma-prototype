// Copyright 2013 Mark Stahl. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the BSD-LICENSE file.

package ast

type Noun struct {
	Name string
}

func (v *Noun) Visit() {}

type Name struct {
	Name string
}

func (o *Name) Visit() {}
