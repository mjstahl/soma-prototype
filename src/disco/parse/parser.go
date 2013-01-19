// Copyright 2013 Mark Stahl. All rights reserved.
// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the BSD-LICENSE file.

package parse

import (
	"disco/ast"
	"disco/scan"
	"fmt"
)

type Parser struct {
	file    *file.File
	errors  scan.ErrorList
	scanner scan.Scanner

	comments []*ast.CommentGroup

	pos file.Pos
	tok scan.Token
	lit string

	projScope *ast.Scope
	topScope  *ast.Scope

	trace  bool
	indent uint
}

func (p *Parser) Init(file *file.File, src []byte, trace bool) {
	p.file = file
	p.trace = trace

	eh := func(pos file.Position, msg string) { p.errors.Add(pos, msg) }
	p.scanner.Init(p.file, src, eh)

	p.next()

	p.openScope()
	p.projScope = p.topScope
}

func (p *Parser) next() {
	line := p.file.Line(p.pos)
	p.nextToken()

	if p.tok == scan.COMMENT {
		var comment *ast.CommentGroup
		var endline int

		if p.file.Line(p.pos) == line {
			comment, endline = p.consumeCommentGroup(0)
		}

		endline = -1
		for p.tok == scan.COMMENT {
			comment, endline = p.consumeCommentGroup(1)
		}
	}
}

func (p *Parser) nextToken() {
	if p.trace && p.pos.IsValid() {
		s := p.tok.String()
		p.printTrace(s)
	}

	p.pos, p.tok, p.lit = p.scanner.Scan()
}

func (p *Parser) printTrace(a ...interface{}) {
	const dots = "................................" +
		"................................"
	const n = uint(len(dots))
	post := p.file.Position(p.pos)
	fmt.Printf("%5d:%3d", pos.Line, pos.Column)

	i := 2 * p.indent
	for ; i > n; i -= n {
		fmt.Print(dots)
	}
	fmt.Print(dots[0:i])
	fmt.Println(a...)
}
