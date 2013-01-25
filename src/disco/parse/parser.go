// Copyright 2013 Mark Stahl. All rights reserved.
// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the BSD-LICENSE file.

package parse

import (
	"disco/ast"
	"disco/file"
	"disco/scan"
	"fmt"
)

type Parser struct {
	file    *file.File
	errors  scan.ErrorList
	scanner scan.Scanner

	comments []*ast.Comment
	defines  []*ast.Define

	pos file.Pos
	tok scan.Token
	lit string

	trace  bool
	indent uint
}

func (p *Parser) Init(f *file.File, src []byte, trace bool) {
	p.file = f
	p.trace = trace

	p.scanner.Init(p.file, src, nil)

	p.next()
}

func (p *Parser) Parse() {
	for p.tok != scan.EOF {
		switch {
		case p.tok == scan.COMMENT:
			comment := &ast.Comment{Start: p.pos, Text: p.lit}
			p.comments = append(p.comments, comment)
		case p.tok == scan.BINARY && p.lit == "+":
			define := p.parseExtDefine()
			p.defines = append(p.defines, define)
		}

		p.next()
	}
}

func (p *Parser) next() {
	p.pos, p.tok, p.lit = p.scanner.Scan()
}

func (p *Parser) expect(tok scan.Token) string {
	lit := p.lit
	if p.tok != tok {
		fmt.Printf("expected '%s', received '%s'\n", tok, p.tok)
		return ""
	}
	p.next()
	return lit
}
