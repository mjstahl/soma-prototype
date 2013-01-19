// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the BSD-LICENSE file.

package ast

type File struct {
	Doc	*CommentGroup
//	Exprs
//	Decls	// ultimately just an expression, so this may be removed
	Comments []*CommentGroup
}
