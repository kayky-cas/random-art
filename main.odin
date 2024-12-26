package main

import "core:fmt"
import "core:math"
import "core:math/rand"
import "core:mem"
import "core:os"
import "vendor:stb/image"

RGBAColor :: [4]u8
Color :: [3]f32

Vec2 :: [2]f32

Pair :: [2]^Node
Triple :: [3]^Node

NodeKind :: enum {
	Bool,
	Number,
	X,
	Y,
	RGB,
	Plus,
	Mul,
	Mod,
	If,
	Gt,
	Gte,
	Expr,
	Rand,
}

Grammar :: map[string]Rule

Rule :: []Expr

Expr :: struct {
	probability: f32,
	kind:        enum {
		Tag,
		Node,
	},
	value:       union {
		^Node,
		string,
	},
}

NodeValue :: union {
	bool,
	f32,
	Triple,
	Pair,
	string,
}

Node :: struct {
	kind:  NodeKind,
	value: NodeValue,
}

WIDTH :: 800
HEIGHT :: 800

image_buf := [WIDTH * HEIGHT]RGBAColor{}

eval_node :: proc(node: ^Node, p: Vec2, grammar: ^Grammar, depth: int) -> ^Node {
	switch node.kind {
	case .Bool:
		fallthrough
	case .Number:
		return node
	case .X:
		return new_node(NodeKind.Number, p.x)
	case .Y:
		return new_node(NodeKind.Number, p.y)
	case .Plus:
		lhs := eval_node(node.value.(Pair).x, p, grammar, depth)
		assert(lhs.kind == .Number)
		rhs := eval_node(node.value.(Pair).y, p, grammar, depth)
		assert(rhs.kind == .Number)

		return new_node(NodeKind.Number, lhs.value.(f32) + rhs.value.(f32))
	case .Mul:
		lhs := eval_node(node.value.(Pair).x, p, grammar, depth)
		assert(lhs.kind == .Number)
		rhs := eval_node(node.value.(Pair).y, p, grammar, depth)
		assert(rhs.kind == .Number)

		return new_node(NodeKind.Number, lhs.value.(f32) * rhs.value.(f32))
	case .Mod:
		lhs := eval_node(node.value.(Pair).x, p, grammar, depth)
		assert(lhs.kind == .Number)
		rhs := eval_node(node.value.(Pair).y, p, grammar, depth)
		assert(rhs.kind == .Number)

		return new_node(NodeKind.Number, math.mod_f32(lhs.value.(f32), rhs.value.(f32)))
	case .Gt:
		lhs := eval_node(node.value.(Pair).x, p, grammar, depth)
		assert(lhs.kind == .Number)
		rhs := eval_node(node.value.(Pair).y, p, grammar, depth)
		assert(rhs.kind == .Number)

		return new_node(NodeKind.Bool, lhs.value.(f32) > rhs.value.(f32))
	case .Gte:
		lhs := eval_node(node.value.(Pair).x, p, grammar, depth)
		assert(lhs.kind == .Number)
		rhs := eval_node(node.value.(Pair).y, p, grammar, depth)
		assert(rhs.kind == .Number)

		return new_node(NodeKind.Bool, lhs.value.(f32) >= rhs.value.(f32))
	case .If:
		condition := eval_node(node.value.(Triple).x, p, grammar, depth)
		assert(condition.kind == .Bool)

		if condition.value.(bool) {
			lhs := eval_node(node.value.(Triple).y, p, grammar, depth)
			assert_final_rgb(lhs)
			return lhs
		}

		rhs := eval_node(node.value.(Triple).z, p, grammar, depth)
		assert_final_rgb(rhs)

		return rhs
	case .RGB:
		rgb := &node.value.(Triple)

		r := eval_node(rgb.r, p, grammar, depth)
		g := eval_node(rgb.g, p, grammar, depth)
		b := eval_node(rgb.b, p, grammar, depth)

		n := new_node(NodeKind.RGB, Triple{r, g, b})

		assert_final_rgb(n)

		return n
	case .Rand:
		pair := &node.value.(Pair)

		min := eval_node(pair.x, p, grammar, depth)
		assert(min.kind == .Number)
		max := eval_node(pair.y, p, grammar, depth)
		assert(max.kind == .Number)

		rnd := min.value.(f32) + (rand.float32() * (min.value.(f32) + max.value.(f32)))
		return new_node(NodeKind.Number, rnd)
	case .Expr:
		expr := node.value.(string)
		return eval_expr(expr, p, grammar, depth - 1)
	}

	return nil
}

eval_expr :: proc(expr: string, p: Vec2, grammar: ^Grammar, depth: int) -> ^Node {
	rule := grammar[expr]

	if depth <= 0 {
		expr := rule[0]

		if expr.kind == .Node && expr.value.(^Node).kind == .RGB {
			return expr.value.(^Node)
		}
	}

	r := rand.float32()

	for e, _ in rule {
		if r < e.probability {
			if e.kind == .Tag {
				return eval_expr(e.value.(string), p, grammar, depth - 1)
			}
			return eval_node(e.value.(^Node), p, grammar, depth)
		}
		r -= e.probability
	}

	return nil
}

assert_final_rgb :: proc(node: ^Node) {
	assert(node.kind == .RGB)

	rgb := &node.value.(Triple)

	assert(rgb.r.kind == .Number)
	assert(rgb.g.kind == .Number)
	assert(rgb.b.kind == .Number)
}

eval :: proc(p: Vec2, grammar: ^Grammar, initial_rule: string) -> Color {
	n := eval_expr(initial_rule, p, grammar, 10)
	assert_final_rgb(n)

	l := &n.value.(Triple)

	return {l[0].value.(f32), l[1].value.(f32), l[2].value.(f32)}
}

apply_to_rgba_node :: proc(
	data: []RGBAColor,
	w: int,
	h: int,
	grammar: ^Grammar,
	initial_rule: string,
) {
	for j in 0 ..< h {
		nj := f32(j) / f32(h) * 2. - 1
		for i in 0 ..< w {
			ni := f32(i) / f32(w) * 2. - 1

			n := eval({ni, nj}, grammar, initial_rule)

			data[j * w + i] = {
				u8((n.r + 1) * 255 / 2),
				u8((n.g + 1) * 255 / 2),
				u8((n.b + 1) * 255 / 2),
				0xFF,
			}
		}
	}
}

apply_to_rgba :: proc(f: proc(p: Vec2) -> Color, data: []RGBAColor, w: int, h: int) {
	for j in 0 ..< h {
		nj := f32(j) / f32(h) * 2. - 1
		for i in 0 ..< w {
			ni := f32(i) / f32(w) * 2. - 1

			n := f({ni, nj})

			data[j * w + i] = {
				u8((n.r + 1) * 255 / 2),
				u8((n.g + 1) * 255 / 2),
				u8((n.b + 1) * 255 / 2),
				0xFF,
			}
		}
	}
}

new_node :: proc(kind: NodeKind, value: NodeValue = nil, a: ^mem.Arena = nil) -> ^Node {
	node: ^Node

	if a != nil {
		ptr, _ := mem.arena_alloc(a, size_of(Node))
		node = transmute(^Node)ptr
	} else {
		node = new(Node)
	}

	node.kind = kind
	node.value = value
	return node
}

main :: proc() {
	root := new_node(
		.If,
		Triple {
			new_node(
				.Gte,
				Pair{new_node(.Mul, Pair{new_node(.X), new_node(.Y)}), new_node(.Number, 0)},
			),
			new_node(.RGB, Triple{new_node(.X), new_node(.Y), new_node(.Number, 1)}),
			new_node(
				.RGB,
				Triple {
					new_node(.Mod, Pair{new_node(.X), new_node(.Y)}),
					new_node(.Mod, Pair{new_node(.X), new_node(.Y)}),
					new_node(.Mod, Pair{new_node(.X), new_node(.Y)}),
				},
			),
		},
	)

	// root := new_node(.RGB, Triple{new_node(.X), new_node(.X), new_node(.X)})

	//root := new_node(
	//	.RGB,
	//	Triple {
	//		new_node(.X),
	//		new_node(.Y),
	//		new_node(.Mul, Pair{new_node(.Number, -1), new_node(.X)}),
	//	},
	//)

	grammar := make(Grammar)
	defer delete(grammar)

	grammar["E"] = []Expr {
		{
			1,
			.Node,
			new_node(
				.RGB,
				Triple{new_node(.Expr, "C"), new_node(.Expr, "C"), new_node(.Expr, "C")},
			),
		},
	}

	grammar["A"] = []Expr {
		{1. / 3., .Node, new_node(.Rand, Pair{new_node(.Number, -1), new_node(.Number, 1)})},
		{1. / 3., .Node, new_node(.X)},
		{1. / 3., .Node, new_node(.Y)},
	}

	grammar["C"] = []Expr {
		{1. / 4., .Tag, "A"},
		{3. / 8., .Node, new_node(.Plus, Pair{new_node(.Expr, "C"), new_node(.Expr, "C")})},
		{3. / 8., .Node, new_node(.Plus, Pair{new_node(.Expr, "C"), new_node(.Expr, "C")})},
	}


	apply_to_rgba_node(image_buf[:], WIDTH, HEIGHT, &grammar, "E")

	if image.write_png("output.png", WIDTH, HEIGHT, 4, &image_buf, WIDTH * size_of(RGBAColor)) ==
	   0 {
		fmt.println("Error writing image")
		os.exit(1)
	}
}
