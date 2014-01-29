package parse

import (
	"soma/ast"
	"soma/rt"
	"soma/scan"
)

// define :=
//	'+' | '-' receiver message_pattern DEFINE block
// message_pattern :=
//	unary_define | binary_define | keyword_define
//
func (p *Parser) parseDefine() *ast.Define {
	p.expect(scan.BINARY)

	receiver, global := p.parseReceiver()

	var behavior string
	var args []string

	switch {
	case p.tok == scan.IDENT:
		behavior = p.parseUnaryDef()
	case p.tok == scan.BINARY:
		behavior, args = p.parseBinaryDef()
	case p.tok == scan.KEYWORD:
		behavior, args = p.parseKeywordDef()
	}

	if behavior == "" {
		p.error(p.pos, "expected unary, binary, or keyword behavior, found '%s'", p.lit)
	}

	p.expect(scan.DEFINE)
	
	body := &ast.Block{}
	body.Statements = p.parseStatements([]rt.Expr{})

	if receiver != "" {
		bargs := []string{receiver}
		body.Args = append(bargs, args...)
	} else {
		body.Args = args
	}

	return &ast.Define{global, behavior, body}
}

// receiver :=
//   GLOBAL | '(' IDENT GLOBAL ')'
func (p *Parser) parseReceiver() (string, string) {
	if p.tok == scan.GLOBAL {
		return "", p.expect(scan.GLOBAL)
	}

	p.expect(scan.LPAREN)

	arg := p.expect(scan.IDENT)
	obj := p.expect(scan.GLOBAL)

	p.expect(scan.RPAREN)

	return arg, obj
}

// unary_define :=
//	IDENT
//
func (p *Parser) parseUnaryDef() string {
	return p.expect(scan.IDENT)
}

// binary_define :=
//	BINARY IDENT
//
func (p *Parser) parseBinaryDef() (lit string, args []string) {
	lit = p.expect(scan.BINARY)
	args = append(args, p.expect(scan.IDENT))

	return
}

// keyword_define :=
//	(KEYWORD IDENT)+
//
func (p *Parser) parseKeywordDef() (lit string, args []string) {
	for p.tok == scan.KEYWORD {
		lit = lit + p.expect(scan.KEYWORD)

		arg := p.expect(scan.IDENT)
		if arg != "" {
			args = append(args, arg)
		}
	}
	return
}

func (p *Parser) isDefineStart() bool {
	return p.tok == scan.BINARY && p.lit == "+"
}
