package parse

import (
	"soma/ast"
	"soma/rt"
	"soma/scan"
)

// define :=
//	paren_expr message_pattern DEFINE statements PERIOD
//
func (p *Parser) parseDefinition(define *ast.Define) *ast.Define {
	behavior, args := p.parseMessagePattern()

	p.expect(scan.DEFINE)

	define.Behavior = behavior
	define.Body.Args = append(define.Body.Args, args...)
	define.Body.Statements = p.parseStatements([]rt.Expr{})

	p.expect(scan.PERIOD)

	return define
}

// message_pattern :=
//  unary_define | binary_define | keyword_define
func (p *Parser) parseMessagePattern() (behavior string, args []string) {
	switch p.tok {
	case scan.IDENT:
		behavior = p.parseUnaryDef()
	case scan.BINARY:
		behavior, args = p.parseBinaryDef()
	case scan.KEYWORD:
		behavior, args = p.parseKeywordDef()
	}

	if behavior == "" {
		p.error(p.pos, "expected unary, binary, or keyword behavior, found '%s'", p.lit)
	}
	return
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

func createDefinition(global string, arg string) *ast.Define {
	body := &ast.Block{};
	if arg != "" {
		body = &ast.Block{Args: []string{arg}}
	} else {
		body = &ast.Block{Args: []string{}}
	}
	return &ast.Define{Receiver: global, Body: body}
}
