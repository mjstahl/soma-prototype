// Copyright 2013 Mark Stahl. All rights reserved.
// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the BSD-LICENSE file.

package parse

import (
	"disco/ast"
	"disco/file"
	"disco/scan"
)

type Parser struct {
	file    *file.File
	errors  scan.ErrorList
	scanner scan.Scanner

	comments []*ast.Comment

	pos file.Pos
	tok scan.Token
	lit string

	projScope *ast.Scope
	topScope  *ast.Scope

	trace  bool
	indent uint
}

func (p *Parser) Init(f *file.File, src []byte, trace bool) {
	p.file = f
	p.trace = trace

	eh := func(pos file.Position, msg string) { p.errors.Add(pos, msg) }
	p.scanner.Init(p.file, src, eh)

	p.next()

	p.openScope()
	p.projScope = p.topScope
}

func (p *Parser) Parse() {
	for p.tok != scan.EOF {
		switch {
		case p.tok == scan.COMMENT:

		case p.tok == scan.BINARY && p.lit == "+":
		}

		p.next()
	}
}

func (p *Parser) next() {
	p.pos, p.tok, p.lit = p.scanner.Scan()
}
