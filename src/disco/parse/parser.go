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
	scanner scan.Scanner

	Exprs []ast.Expression

	pos file.Pos
	tok scan.Token
	lit string
}

func (p *Parser) Init(f *file.File, src []byte) {
	p.file = f
	p.scanner.Init(p.file, src, nil)

	p.next()
}

func (p *Parser) Parse() {
	var exprs []ast.Expression
	for p.tok != scan.EOF {
		switch {
		case p.tok == scan.COMMENT:
			comment := &ast.Comment{Start: p.pos, Text: p.lit}
			exprs = append(exprs, comment)
		case p.tok == scan.BINARY && p.lit == "+":
			define := p.parseExtDefine()
			exprs = append(exprs, define)
		}

		p.next()
	}
	p.Exprs = exprs
}

func (p *Parser) next() {
	p.pos, p.tok, p.lit = p.scanner.Scan()
}

func (p *Parser) expect(tok scan.Token) (lit string) {
	lit = p.lit
	if p.tok != tok {
		fmt.Printf("expected '%s', received '%s'\n", tok, p.tok)
		return ""
	}
	p.next()
	return
}
