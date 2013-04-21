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
	"soma/ast"
	"soma/rt"
	"soma/scan"
)

func (p *Parser) isMessageStart() bool {
	return p.tok == scan.IDENT || p.tok == scan.BINARY || p.tok == scan.KEYWORD
}

// messages :=
//	unary_message+ binary_message* [keyword_message]
//    |	binary_message+ [keyword_message]
//    | keyword_message
//
func (p *Parser) parseMessages(recv rt.Expr) rt.Expr {
	var msg rt.Expr

	switch {
	case p.tok == scan.IDENT:
		msg = p.parseUnaryMessage(recv)
	case p.tok == scan.BINARY:
		msg = p.parseBinaryMessage(recv)
	case p.tok == scan.KEYWORD:
		msg = p.parseKeywordMessage(recv)
	default:
		return recv
	}
	return p.parseMessages(msg)
}

// unary_message :=
//	IDENT
//
func (p *Parser) parseUnaryMessage(recv rt.Expr) (msg rt.Expr) {
	name := p.expect(scan.IDENT)
	msg = &ast.UnaryMessage{recv, name}

	return
}

// binary_message :=
//	BINARY binary_argument
func (p *Parser) parseBinaryMessage(recv rt.Expr) rt.Expr {
	name := p.expect(scan.BINARY)
	bm := &ast.BinaryMessage{recv, name, p.parseBinaryArgument()}

	return bm
}

// binary_argument :=
//	primary unary_message*
//
func (p *Parser) parseBinaryArgument() rt.Expr {
	primary := p.parsePrimary()
	return p.parseUnaryMessages(primary)
}

func (p *Parser) parseUnaryMessages(recv rt.Expr) rt.Expr {
	if p.tok != scan.IDENT {
		return recv
	}

	name := p.expect(scan.IDENT)
	msg := &ast.UnaryMessage{recv, name}

	return p.parseUnaryMessages(msg)
}

// keyword_message :=
//	(KEYWORD keyword_argument)+
//
func (p *Parser) parseKeywordMessage(recv rt.Expr) rt.Expr {
	km := &ast.KeywordMessage{Receiver: recv}
	for p.tok == scan.KEYWORD {
		km.Behavior = km.Behavior + p.expect(scan.KEYWORD)
		km.Args = append(km.Args, p.parseKeywordArgument())
	}
	return km
}

// keyword_argument :=
//	primary unary_message* binary_message*
//
func (p *Parser) parseKeywordArgument() rt.Expr {
	primary := p.parsePrimary()
	
	um := p.parseUnaryMessages(primary)
	return p.parseBinaryMessages(um)
}

func (p *Parser) parseBinaryMessages(recv rt.Expr) rt.Expr {
	if p.tok != scan.BINARY {
		return recv
	}
	behavior := p.expect(scan.BINARY)

	msg := &ast.BinaryMessage{recv, behavior, p.parseBinaryArgument()}
	return p.parseBinaryMessages(msg)
}

// cascaded_messages :=
//   (; messages)*
func (p *Parser) parseCascadeMessages(recv rt.Expr, msgs []rt.Expr) []rt.Expr {
	for p.tok == scan.CASCADE {
		p.expect(scan.CASCADE)

		if !p.isMessageStart() {
			p.error(p.pos, "expected unary, binary, or keyword message, found '%s'", p.lit)
			return msgs
		}
		msgs = append(msgs, p.parseMessages(recv))
	}
	return msgs
}
