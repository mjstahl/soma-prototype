// Copyright 2013 Mark Stahl. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the BSD-LICENSE file.

package parse

import (
	"disco/ast"
	"disco/file"
	"disco/rt"
	"disco/scan"
	"fmt"
)

type Parser struct {
	file    *file.File
	errors  scan.ErrorList
	scanner scan.Scanner

	Defns []*ast.Define
	Exprs []rt.Expr

	pos file.Pos
	tok scan.Token
	lit string
}

func (p *Parser) Init(f *file.File, src []byte) {
	p.file = f

	eh := func(pos file.Position, msg string) { p.errors.Add(pos, msg) }
	p.scanner.Init(p.file, src, eh)

	p.next()
}

func (p *Parser) parseFile() *ast.File {
	p.parse()

	return &ast.File{p.Defns, p.Exprs}
}

func (p *Parser) parse() {
	var defns []*ast.Define
	var exprs []rt.Expr

	for p.tok != scan.EOF {
		var defn *ast.Define
		var expr rt.Expr

		switch {
		case p.tok == scan.COMMENT:
			expr = &ast.Comment{Text: p.lit}
		case p.isExternalDefine():
			defn = p.parseDefine()
			if defn != nil {
				defns = append(defns, defn)
			}
		case p.isPrimary():
			expr = p.parseExpr()
			if expr != nil {
				exprs = append(exprs, expr)
			}
		}
		p.next()
	}

	p.Defns = defns
	p.Exprs = exprs
}

func (p *Parser) next() {
	p.pos, p.tok, p.lit = p.scanner.Scan()
}

func (p *Parser) expect(tok scan.Token) (lit string) {
	lit = p.lit
	pos := p.pos
	if p.tok != tok {
		p.error(pos, "expected %s, found %s (%s)", tok, p.tok, p.lit)

	}
	p.next()
	return
}

func (p *Parser) error(pos file.Pos, err string, args ...interface{}) {
	err = fmt.Sprintf(err, args...)
	p.errors.Add(p.file.Position(pos), err)
}
