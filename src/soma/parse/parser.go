// Copyright (C) 2013 Mark Stahl

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.

// You should have received a copy of the GNU Affero General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

package parse

import (
	"fmt"
	"soma/ast"
	"soma/file"
	"soma/rt"
	"soma/scan"
)

type Parser struct {
	file    *file.File
	errors  scan.ErrorList
	scanner scan.Scanner

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

	return &ast.File{p.Exprs}
}

func (p *Parser) parse() {
	var exprs []rt.Expr

	for p.tok != scan.EOF {
		var expr rt.Expr

		switch {
		case p.tok == scan.COMMENT:
			expr = &ast.Comment{Text: p.lit}
			p.next()
		case p.isExternalDefine():
			expr = p.parseDefine()
		case p.isPrimary():
			expr = p.parseExpr()
		default:
			p.error(p.pos, "expected Defn or Expr, found %s (%s)", p.tok, p.lit)
			p.next()
		}

		if expr != nil {
			exprs = append(exprs, expr)
		}
	}
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
