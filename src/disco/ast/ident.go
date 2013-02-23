// Copyright 2013 Mark Stahl. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the BSD-LICENSE file.

package ast

import (
	"disco/rt"
	"fmt"
)

type Local struct {
	Value string
}

func (l *Local) Eval(s *rt.Scope) (rt.Value, error) {
	return nil, nil
}

func (l *Local) Visit(s *rt.Scope) (rt.Value, error) {
	return nil, nil
}

type Global struct {
	Value string
}

func (g *Global) String() string {
	return g.Value
}

func (g *Global) Eval(s *rt.Scope) (rt.Value, error) {
	oid, found := rt.RT.Globals.Lookup(g.Value);
	if !found {
		return nil, LookupError(g.Value)
	}

	obj := rt.RT.Heap.Lookup(oid)
	return obj, nil
}

func (g *Global) Visit(s *rt.Scope) (rt.Value, error) {
	return g.Eval(s)
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
