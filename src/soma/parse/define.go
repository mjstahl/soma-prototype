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
	"soma/scan"
)

// define :=
//	"+" NAME message_pattern DEFINE block
// message_pattern :=
//	unary_define | binary_define | keyword_define
//
func (p *Parser) parseDefine() *ast.Define {
	p.expect(scan.BINARY)
	global := p.expect(scan.GLOBAL)

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

	bargs := []string{"self", "this"}
	body.Args = append(bargs, args...)

	return &ast.Define{global, behavior, args, body}
}

func (p *Parser) isExternalDefine() bool {
	return p.tok == scan.BINARY && p.lit == "+"
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
