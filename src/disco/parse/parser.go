// Copyright 2013 Mark Stahl. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the BSD-LICENSE file.

package parse

import (
	"disco/ast"
	"disco/file"
	"disco/scan"
	"fmt"
	"os"
)

type Parser struct {
	file    *file.File
	scanner scan.Scanner

	Defns []*ast.Define
	Exprs []ast.Expr

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
	var defns []*ast.Define
	var exprs []ast.Expr

	for p.tok != scan.EOF {
		var defn *ast.Define
		var expr ast.Expr

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
	if p.tok != tok {
		p.error("expected %s, found %s (%s)", tok, p.tok, p.lit)
	}
	p.next()
	return
}

func (p *Parser) error(err string, args ...interface{}) {
	err = fmt.Sprintf(err, args...)
	fmt.Println(err + "\n")

	os.Exit(1)
}
