// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the BSD-LICENSE file.

package parse

import (
	"disco/ast"
	"disco/scan"
)

func (p *Parser) consumeCommentGroup(n int) (comments *ast.CommentGroup, endline int) {
	var list []*ast.Comment
	endline = p.file.Line(p.pos)
	for p.tok == scan.COMMENT && p.file.Line(p.pos) <= endline+n {
		var comment *ast.Comment
		comment, endline = p.consumeComment()
		list = append(list, comment)
	}

	comments = &ast.CommentGroup{List: list}
	p.comments = append(p.comments, comments)

	return
}

func (p *Parser) consumeComment() (comment *ast.Comment, endline int) {
	endline = p.file.Line(p.pos)
	for i := 0; i < len(p.lit); i++ {
		if p.lit[i] == '\n' {
			endline++
		}
	}

	comment = &ast.Comment{Quote: p.pos, Text: p.lit}

	return
}
