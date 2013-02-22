// Copyright 2013 Mark Stahl. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the BSD-LICENSE file.

package ast

import (
	"disco/rt"
	"fmt"
)

type Ident struct {
	Name string
}

func (i *Ident) Eval(s *rt.Scope) (interface{}, error) {
	return nil, nil
}

type Global struct {
	Name string
}

func (g *Global) Eval(s *rt.Scope) (interface{}, error) {
	obj := rt.RT.Globals.Lookup(g.Name)
	if obj == 0 {
		return nil, LookupError(g.Name)
	}
	return obj, nil
}

type lookupError struct {
	n string
}

func LookupError(name string) error {
	return &lookupError{n: name}
}

func (e *lookupError) Error() string {
	error := "%s was unavailable for eval. Try again after '%s' is defined."
	return fmt.Sprintf(error, e.n, e.n)
}
