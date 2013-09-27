package parse

import (
	"soma/ast"
	"soma/scan"
)

// define :=
//	'+' | '-' reciever message_pattern DEFINE block
// message_pattern :=
//	unary_define | binary_define | keyword_define
//
func (p *Parser) parseDefine() *ast.Define {
	external := true
	if p.expect(scan.BINARY) == "-" {
		external = false
	}

	argument, global := p.parseReceiver()

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
	body := p.parseBlock()

	bargs := []string{argument}
	body.Args = append(bargs, args...)

	return &ast.Define{external, global, behavior, args, body}
}

// NAME :=
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

func (p *Parser) isExternalDefine() bool {
	return p.tok == scan.BINARY && p.lit == "+"
}

func (p *Parser) isInternalDefine() bool {
	return p.tok == scan.BINARY && p.lit == "-"
}
