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

	if define, ok := recv.(*ast.Define); ok {
		return p.parseDefinition(define)
	} else {
		return p.parseMessagePortion(recv)
	}
}

func (p *Parser) parseMessagePortion(recv rt.Expr) rt.Expr {
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
//	comment | ident | global | block | paren_expr | integer
//
func (p *Parser) parsePrimary() (recv rt.Expr) {
	switch p.tok {
	case scan.COMMENT:
		comment := p.expect(scan.COMMENT)
		recv = &ast.Comment{Text: comment}
	case scan.IDENT:
		name := p.expect(scan.IDENT)
		if p.tok == scan.ASSIGN {
			recv = p.parseAssignment(name)
		} else {
			recv = &ast.Local{Value: name}
		}
	case scan.GLOBAL:
		name := p.expect(scan.GLOBAL)
		recv = &ast.Global{Value: name}
	case scan.LBRACE:
		recv = p.parseBlock()
	case scan.LPAREN:
		recv = p.parseParenExpr()
	case scan.INT:
		integer := p.expect(scan.INT)
		recv = ast.NewInteger(integer)
	default:
		p.error(p.pos, "expected identifier, block, array, map, number, or string found '%s'", p.lit)
		p.next()
	}
	return
}

func (p *Parser) isPrimary() bool {
	return p.tok == scan.COMMENT ||
		p.tok == scan.IDENT ||
		p.tok == scan.GLOBAL ||
		p.tok == scan.LBRACE ||
		p.tok == scan.LPAREN ||
		p.tok == scan.INT
}

// paren_expr :=
//	 LPAREN GLOBAL RPAREN
// | LPAREN IDENT GLOBAL RPAREN
// | LPAREN expression RPAREN
//
func (p *Parser) parseParenExpr() (expr rt.Expr) {
	p.expect(scan.LPAREN)

	recv := p.parsePrimary()
	switch recv.(type) {
	case *ast.Global:
		global := recv.(*ast.Global)
		if p.tok == scan.RPAREN {
			expr = createDefinition(global.Value, "")
		} else {
			expr = p.parseMessagePortion(recv)
		}
	case *ast.Local:
		local := recv.(*ast.Local)
		if p.tok == scan.GLOBAL {
			global := p.expect(scan.GLOBAL)
			expr = createDefinition(global, local.Value);
		}
	default:
		expr = p.parseMessagePortion(recv)
	}

	p.expect(scan.RPAREN)
	return
}
