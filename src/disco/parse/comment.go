// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the BSD-LICENSE file.

package parse

import (
	"disco/ast"
)

func (p *Parser) consumeComment() {
	comment := &ast.Comment{Quote: p.pos, Text: p.lit}
	p.comments = append(p.comments, comment)
}
