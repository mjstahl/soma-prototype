// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the BSD-LICENSE file.

package parse

func (p *Parser) openScope() {
	p.topScope = ast.CreateScope(p.topScope)
}
