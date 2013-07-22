package parse

import (
	"soma/ast"
	"soma/rt"
	"soma/scan"
)

// expression :=
//	primary [messages cascaded_messages]
//    | assignment
//
func (p *Parser) parseExpr() rt.Expr {
	recv := p.parsePrimary()

	if p.isMessageStart() {
		msg := p.parseMessages(recv)
		if p.tok == scan.CASCADE {
			cascade := p.parseCascadeMessages(recv, []rt.Expr{msg})
			return &ast.Cascade{Messages: cascade}
		}
		return msg
	}
	return recv
}

// primary :=
//	ident | global | block | paren | return | integer
//
func (p *Parser) parsePrimary() (recv rt.Expr) {
	switch p.tok {
	case scan.IDENT:
		name := p.expect(scan.IDENT)
		if p.tok == scan.COMMA || p.tok == scan.ASSIGN {
			recv = p.parseAssignment(name)
		} else {
			recv = &ast.Local{Value: name}
		}
	case scan.GLOBAL:
		name := p.expect(scan.GLOBAL)
		recv = &ast.Global{Value: name}
	case scan.LBRACE:
		recv = p.parseBlock()
	case scan.LBRACK:
		recv = p.parseArray()
	case scan.LPAREN:
		recv = p.parseParenExpr()
	case scan.INT:
		integer := p.expect(scan.INT)
		recv = ast.NewInteger(integer)
	case scan.STRING:
		str := p.expect(scan.STRING)
		recv = &ast.String{Text: str}
	case scan.SYMBOL:
		sym := p.expect(scan.SYMBOL)
		recv = &ast.Symbol{Text: sym}
	default:
		p.error(p.pos, "expected identifier, block, array, map, number, or string found '%s'", p.lit)
		p.next()
	}
	return
}

func (p *Parser) isPrimary() bool {
	return p.tok == scan.IDENT ||
		p.tok == scan.GLOBAL ||
		p.tok == scan.LBRACE ||
		p.tok == scan.LBRACK ||
		p.tok == scan.LPAREN ||
		p.tok == scan.INT    ||
		p.tok == scan.STRING ||
		p.tok == scan.SYMBOL
}

// paren :=
// 	'(' expression ')'
//
func (p *Parser) parseParenExpr() (recv rt.Expr) {
	p.expect(scan.LPAREN)

	recv = p.parseExpr()

	p.expect(scan.RPAREN)
	return
}
