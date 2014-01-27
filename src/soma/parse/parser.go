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
		case p.isExternalDefine() || p.isInternalDefine():
			expr = p.parseDefine()
		case p.isPrimary():
			expr = p.parseExpr()
		case p.tok == scan.PERIOD:
			p.next()
		default:
			p.error(p.pos, "expected definition or expression, found '%s'", p.lit)
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
		p.error(pos, "expected %s, found '%s'", tok, p.lit)
	}
	p.next()
	return
}

func (p *Parser) error(pos file.Pos, err string, args ...interface{}) {
	err = fmt.Sprintf(err, args...)
	p.errors.Add(p.file.Position(pos), err)
}
