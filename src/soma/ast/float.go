package ast

import (
	"soma/rt"
	"strconv"
)

type Float struct {
	Value float64
}

func NewFloat(literal string) *Float {
	value, _ := strconv.ParseFloat(literal, 64)
	return &Float{value}
}

func (f *Float) Eval(s *rt.Scope) rt.Value {
	return nil
}

func (f *Float) Visit(s *rt.Scope) rt.Value {
	return f.Eval(s)
}
