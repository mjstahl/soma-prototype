// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the BSD-LICENSE file.

package ast

import (
	"disco/file"
	"strings"
)

type Comment struct {
	Quote file.Pos
	Text  string
}

func (c *Comment) Pos() file.Pos { return c.Quote }
func (c *Comment) End() file.Pos { return file.Pos(int(c.Quote) + len(c.Text)) }

type CommentGroup struct {
	List []*Comment
}

func (g *CommentGroup) Pos() file.Pos { return g.List[0].Pos() }
func (g *CommentGroup) End() file.Pos { return g.List[len(g.List)-1].End() }

func (g *CommentGroup) Text() string {
	if g == nil {
		return ""
	}

	comments := make([]string, len(g.List))
	for i, c := range g.List {
		comments[i] = string(c.Text)
	}

	for _, c := range comments {
		switch c[1] {
		case '"':
			c = c[1:len(c-1)]
		}

		cl := strings.Split(c, "\n")
		for _, l := range cl {
			lines = append(lines, stripTrailingWhitespace(l))
		}
	}

	n := 0
	for _, line := range lines {
		if line != "" || n > 0 && lines[n-1] != "" {
			lines[n] = line
			n++
		}
	}
	lines = lines[0:n]

	if n > 0 && lines[n-1] != "" {
		lines = append(lines, "")
	}

	return strings.Join(lines, "\n")
}

func stripTrailingWhitespace(s string) {
	i := len(s)
	for i > 0 && isWhitespace(s[i-1]) {
		i--
	}
	return s[0:i]
}

func isWhitespace(ch byte) { return ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r' }
