package main

import "core:c"
import "core:fmt"
import "core:math"
import "core:math/rand"
import "core:mem"
import "core:os"
import "core:strconv"
import "core:strings"
import "core:time"
import r "vendor:raylib"
import "vendor:raylib/rlgl"
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
	Time,
	Div,
	Sin,
}

Grammar :: map[string]Rule

Rule :: []Expr

Expr :: struct {
	probability: f32,
	value:       ^Node,
}

NodeValue :: union {
	bool,
	f32,
	Triple,
	Pair,
	string,
	Rule,
	^Node,
}

Node :: struct {
	kind:  NodeKind,
	value: NodeValue,
}

WIDTH :: 800
HEIGHT :: 800

image_buf := [WIDTH * HEIGHT]RGBAColor{}

eval_arena := mem.Arena{}

eval_node :: proc(node: ^Node, p: Vec2, a: ^mem.Arena = nil) -> ^Node {
	switch node.kind {
	case .Time:
		return new_node(NodeKind.Number, 0)
	case .Bool:
		fallthrough
	case .Number:
		return node
	case .X:
		return new_node(NodeKind.Number, p.x, a)
	case .Y:
		return new_node(NodeKind.Number, p.y, a)
	case .Plus:
		lhs := eval_node(node.value.(Pair).x, p, a)
		assert(lhs.kind == .Number)
		rhs := eval_node(node.value.(Pair).y, p, a)
		assert(rhs.kind == .Number)

		return new_node(NodeKind.Number, lhs.value.(f32) + rhs.value.(f32), a)
	case .Div:
		lhs := eval_node(node.value.(Pair).x, p, a)
		assert(lhs.kind == .Number)
		rhs := eval_node(node.value.(Pair).y, p, a)
		assert(rhs.kind == .Number)

		return new_node(NodeKind.Number, lhs.value.(f32) / rhs.value.(f32), a)
	case .Sin:
		lhs := eval_node(node.value.(^Node), p, a)
		assert(lhs.kind == .Number)

		return new_node(NodeKind.Number, math.sinh(lhs.value.(f32)), a)
	case .Mul:
		lhs := eval_node(node.value.(Pair).x, p, a)
		assert(lhs.kind == .Number)
		rhs := eval_node(node.value.(Pair).y, p, a)
		assert(rhs.kind == .Number)

		return new_node(NodeKind.Number, lhs.value.(f32) * rhs.value.(f32), a)
	case .Mod:
		lhs := eval_node(node.value.(Pair).x, p, a)
		assert(lhs.kind == .Number)
		rhs := eval_node(node.value.(Pair).y, p, a)
		assert(rhs.kind == .Number)

		return new_node(NodeKind.Number, math.mod_f32(lhs.value.(f32), rhs.value.(f32)), a)
	case .Gt:
		lhs := eval_node(node.value.(Pair).x, p, a)
		assert(lhs.kind == .Number)
		rhs := eval_node(node.value.(Pair).y, p, a)
		assert(rhs.kind == .Number)

		return new_node(NodeKind.Bool, lhs.value.(f32) > rhs.value.(f32), a)
	case .Gte:
		lhs := eval_node(node.value.(Pair).x, p, a)
		assert(lhs.kind == .Number)
		rhs := eval_node(node.value.(Pair).y, p, a)
		assert(rhs.kind == .Number)

		return new_node(NodeKind.Bool, lhs.value.(f32) >= rhs.value.(f32), a)
	case .If:
		condition := eval_node(node.value.(Triple).x, p, a)
		assert(condition.kind == .Bool)

		if condition.value.(bool) {
			lhs := eval_node(node.value.(Triple).y, p, a)
			assert_final_rgb(lhs)
			return lhs
		}

		rhs := eval_node(node.value.(Triple).z, p, a)
		assert_final_rgb(rhs)

		return rhs
	case .RGB:
		rgb := &node.value.(Triple)

		r := eval_node(rgb.r, p, a)
		g := eval_node(rgb.g, p, a)
		b := eval_node(rgb.b, p, a)

		n := new_node(NodeKind.RGB, Triple{r, g, b}, a)

		assert_final_rgb(n)

		return n
	case .Rand:
		pair := &node.value.(Pair)

		min := eval_node(pair.x, p, a)
		assert(min.kind == .Number)
		max := eval_node(pair.y, p, a)
		assert(max.kind == .Number)

		rng := rand.float32()
		rnd := min.value.(f32) + (rng * (max.value.(f32) - min.value.(f32)))
		return new_node(NodeKind.Number, rnd, a)
	case .Expr:
		break
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

eval :: proc(root: ^Node, p: Vec2) -> Color {
	mem.arena_free_all(&eval_arena)

	n := eval_node(root, p, &eval_arena)
	assert_final_rgb(n)

	l := &n.value.(Triple)

	return {l[0].value.(f32), l[1].value.(f32), l[2].value.(f32)}
}


generate_node_o :: proc(node: ^Node, grammar: ^Grammar, depth: int) -> ^Node {
	switch node.kind {
	case .Time:
		fallthrough
	case .Bool:
		fallthrough
	case .Number:
		fallthrough
	case .X:
		fallthrough
	case .Y:
		return node
	case .Plus:
		x := generate_node(node.value.(Pair).x, grammar, depth - 1)
		if x == nil {return nil}
		y := generate_node(node.value.(Pair).y, grammar, depth - 1)
		if y == nil {return nil}

		if x.kind == .Number && y.kind == .Number {
			return new_node(.Number, x.value.(f32) + y.value.(f32))
		}

		return new_node(node.kind, Pair{x, y})
	case .Mul:
		x := generate_node(node.value.(Pair).x, grammar, depth - 1)
		if x == nil {return nil}
		y := generate_node(node.value.(Pair).y, grammar, depth - 1)
		if y == nil {return nil}

		if x.kind == .Number && y.kind == .Number {
			return new_node(.Number, x.value.(f32) * y.value.(f32))
		}

		return new_node(node.kind, Pair{x, y})
	case .Div:
		x := generate_node(node.value.(Pair).x, grammar, depth - 1)
		if x == nil {return nil}
		y := generate_node(node.value.(Pair).y, grammar, depth - 1)
		if y == nil {return nil}

		if x.kind == .Number && y.kind == .Number {
			return new_node(.Number, x.value.(f32) / y.value.(f32))
		}

		return new_node(node.kind, Pair{x, y})
	case .Mod:
		x := generate_node(node.value.(Pair).x, grammar, depth - 1)
		if x == nil {return nil}
		y := generate_node(node.value.(Pair).y, grammar, depth - 1)
		if y == nil {return nil}

		if x.kind == .Number && y.kind == .Number {
			return new_node(.Number, math.mod_f32(x.value.(f32), y.value.(f32)))
		}

		return new_node(node.kind, Pair{x, y})
	case .Gt:
		x := generate_node(node.value.(Pair).x, grammar, depth - 1)
		if x == nil {return nil}
		y := generate_node(node.value.(Pair).y, grammar, depth - 1)
		if y == nil {return nil}

		if x.kind == .Number && y.kind == .Number {
			return new_node(.Bool, x.value.(f32) > y.value.(f32))
		}

		return new_node(node.kind, Pair{x, y})
	case .Gte:
		x := generate_node(node.value.(Pair).x, grammar, depth - 1)
		if x == nil {return nil}
		y := generate_node(node.value.(Pair).y, grammar, depth - 1)
		if y == nil {return nil}

		if x.kind == .Number && y.kind == .Number {
			return new_node(.Bool, x.value.(f32) >= y.value.(f32))
		}

		return new_node(node.kind, Pair{x, y})
	case .Sin:
		x := generate_node(node.value.(^Node), grammar, depth - 1)
		if x == nil {return nil}

		if x.kind == .Number {
			return new_node(.Number, math.sinh(x.value.(f32)))
		}

		return new_node(node.kind, x)
	case .Rand:
		return eval_node(node, 0)
	case .RGB:
		x := generate_node(node.value.(Triple).x, grammar, depth - 1)
		if x == nil {return nil}
		y := generate_node(node.value.(Triple).y, grammar, depth - 1)
		if y == nil {return nil}
		z := generate_node(node.value.(Triple).z, grammar, depth - 1)
		if z == nil {return nil}
		return new_node(node.kind, Triple{x, y, z})
	case .If:
		x := generate_node(node.value.(Triple).x, grammar, depth - 1)
		if x == nil {return nil}

		if x.kind == .Bool {
			if x.value.(bool) {
				y := generate_node(node.value.(Triple).y, grammar, depth - 1)
				if y == nil {return nil}
				return y
			}

			z := generate_node(node.value.(Triple).z, grammar, depth - 1)
			if z == nil {return nil}
			return z
		}

		y := generate_node(node.value.(Triple).y, grammar, depth - 1)
		if y == nil {return nil}
		z := generate_node(node.value.(Triple).z, grammar, depth - 1)
		if z == nil {return nil}
		return new_node(node.kind, Triple{x, y, z})
	case .Expr:
		return generate_rule(node.value.(string), grammar, depth - 1)
	}
	return nil
}

generate_node :: proc(node: ^Node, grammar: ^Grammar, depth: int) -> ^Node {
	switch node.kind {
	case .Time:
		fallthrough
	case .Bool:
		fallthrough
	case .Number:
		fallthrough
	case .X:
		fallthrough
	case .Y:
		return node
	case .Plus:
		fallthrough
	case .Mul:
		fallthrough
	case .Mod:
		fallthrough
	case .Gt:
		fallthrough
	case .Div:
		fallthrough
	case .Gte:
		x := generate_node(node.value.(Pair).x, grammar, depth - 1)
		if x == nil {return nil}
		y := generate_node(node.value.(Pair).y, grammar, depth - 1)
		if y == nil {return nil}
		return new_node(node.kind, Pair{x, y})
	case .Rand:
		return eval_node(node, 0)
	case .Sin:
		x := generate_node(node.value.(^Node), grammar, depth - 1)
		if x == nil {return nil}
		return new_node(node.kind, x)
	case .RGB:
		fallthrough
	case .If:
		x := generate_node(node.value.(Triple).x, grammar, depth - 1)
		if x == nil {return nil}
		y := generate_node(node.value.(Triple).y, grammar, depth - 1)
		if y == nil {return nil}
		z := generate_node(node.value.(Triple).z, grammar, depth - 1)
		if z == nil {return nil}
		return new_node(node.kind, Triple{x, y, z})
	case .Expr:
		return generate_rule(node.value.(string), grammar, depth - 1)
	}
	return nil
}

is_terminal :: proc(node: ^Node) -> bool {
	switch node.kind {
	case .Time:
		fallthrough
	case .Bool:
		fallthrough
	case .Number:
		fallthrough
	case .X:
		fallthrough
	case .Y:
		return true
	case .Plus:
		fallthrough
	case .Mul:
		fallthrough
	case .Mod:
		fallthrough
	case .Gt:
		fallthrough
	case .Gte:
		fallthrough
	case .Div:
		fallthrough
	case .Rand:
		x := is_terminal(node.value.(Pair).x)
		y := is_terminal(node.value.(Pair).y)
		return x && y
	case .Sin:
		x := is_terminal(node.value.(^Node))
		return x
	case .RGB:
		fallthrough
	case .If:
		x := is_terminal(node.value.(Triple).x)
		y := is_terminal(node.value.(Triple).y)
		z := is_terminal(node.value.(Triple).z)
		return x && y && z
	case .Expr:
		return false
	}
	return false
}

gen_shader :: proc(node: ^Node, sb: ^strings.Builder) {
	switch node.kind {
	case .Time:
		fmt.sbprintf(sb, "t")
	case .Bool:
		fmt.sbprintf(sb, "%t", node.value.(bool))
	case .Number:
		fmt.sbprintf(sb, "%f", node.value.(f32))
	case .X:
		fmt.sbprintf(sb, "x")
	case .Y:
		fmt.sbprintf(sb, "y")
	case .RGB:
		r := node.value.(Triple).r
		g := node.value.(Triple).g
		b := node.value.(Triple).b
		fmt.sbprintf(sb, "vec3(")
		gen_shader(r, sb)
		fmt.sbprintf(sb, ",")
		gen_shader(g, sb)
		fmt.sbprintf(sb, ",")
		gen_shader(b, sb)
		fmt.sbprintf(sb, ")")
	case .Plus:
		x := node.value.(Pair).x
		y := node.value.(Pair).y
		fmt.sbprintf(sb, "(")
		gen_shader(x, sb)
		fmt.sbprintf(sb, "+")
		gen_shader(y, sb)
		fmt.sbprintf(sb, ")")
	case .Mul:
		x := node.value.(Pair).x
		y := node.value.(Pair).y

		fmt.sbprintf(sb, "(")
		gen_shader(x, sb)
		fmt.sbprintf(sb, "*")
		gen_shader(y, sb)
		fmt.sbprintf(sb, ")")
	case .Div:
		x := node.value.(Pair).x
		y := node.value.(Pair).y

		fmt.sbprintf(sb, "(")
		gen_shader(x, sb)
		fmt.sbprintf(sb, "/")
		gen_shader(y, sb)
		fmt.sbprintf(sb, ")")
	case .Mod:
		x := node.value.(Pair).x
		y := node.value.(Pair).y
		fmt.sbprintf(sb, "(mod_f32(")
		gen_shader(x, sb)
		fmt.sbprintf(sb, ",")
		gen_shader(y, sb)
		fmt.sbprintf(sb, "))")
	case .If:
		r := node.value.(Triple).r
		g := node.value.(Triple).g
		b := node.value.(Triple).b
		fmt.sbprintf(sb, "(if (")
		gen_shader(r, sb)
		fmt.sbprintf(sb, ") (")
		gen_shader(g, sb)
		fmt.sbprintf(sb, ") else (")
		gen_shader(b, sb)
		fmt.sbprintf(sb, "))")
	case .Gt:
		x := node.value.(Pair).x
		y := node.value.(Pair).y
		fmt.sbprintf(sb, "(")
		gen_shader(x, sb)
		fmt.sbprintf(sb, ">")
		gen_shader(y, sb)
		fmt.sbprintf(sb, ")")
	case .Gte:
		x := node.value.(Pair).x
		y := node.value.(Pair).y
		fmt.sbprintf(sb, "(")
		gen_shader(x, sb)
		fmt.sbprintf(sb, ">=")
		gen_shader(y, sb)
		fmt.sbprintf(sb, ")")
	case .Sin:
		x := node.value.(^Node)
		fmt.sbprintf(sb, "sin(")
		gen_shader(x, sb)
		fmt.sbprintf(sb, ")")
	case .Expr:
		break
	case .Rand:
		break
	}
}

dump_tree :: proc(node: ^Node, sb: ^strings.Builder) {
	switch node.kind {
	case .Time:
		fmt.sbprintf(sb, "time()")
	case .Bool:
		fmt.sbprintf(sb, "%t", node.value.(bool))
	case .Number:
		fmt.sbprintf(sb, "%v", node.value.(f32))
	case .X:
		fmt.sbprintf(sb, "x")
	case .Y:
		fmt.sbprintf(sb, "y")
	case .RGB:
		r := node.value.(Triple).r
		g := node.value.(Triple).g
		b := node.value.(Triple).b
		fmt.sbprintf(sb, "rgb(")
		dump_tree(r, sb)
		fmt.sbprintf(sb, ",")
		dump_tree(g, sb)
		fmt.sbprintf(sb, ",")
		dump_tree(b, sb)
		fmt.sbprintf(sb, ")")
	case .Plus:
		x := node.value.(Pair).x
		y := node.value.(Pair).y
		fmt.sbprintf(sb, "(")
		dump_tree(x, sb)
		fmt.sbprintf(sb, "+")
		dump_tree(y, sb)
		fmt.sbprintf(sb, ")")
	case .Mul:
		x := node.value.(Pair).x
		y := node.value.(Pair).y
		fmt.sbprintf(sb, "(")
		dump_tree(x, sb)
		fmt.sbprintf(sb, "*")
		dump_tree(y, sb)
		fmt.sbprintf(sb, ")")
	case .Div:
		x := node.value.(Pair).x
		y := node.value.(Pair).y
		fmt.sbprintf(sb, "(")
		dump_tree(x, sb)
		fmt.sbprintf(sb, "/")
		dump_tree(y, sb)
		fmt.sbprintf(sb, ")")
	case .Mod:
		x := node.value.(Pair).x
		y := node.value.(Pair).y
		fmt.sbprintf(sb, "mod(")
		dump_tree(x, sb)
		fmt.sbprintf(sb, ",")
		dump_tree(y, sb)
		fmt.sbprintf(sb, ")")
	case .If:
		condition := node.value.(Triple).x
		then := node.value.(Triple).y
		elze := node.value.(Triple).z
		fmt.sbprintf(sb, "if (")
		dump_tree(condition, sb)
		fmt.sbprintf(sb, ") then (")
		dump_tree(then, sb)
		fmt.sbprintf(sb, ") else (")
		dump_tree(elze, sb)
		fmt.sbprintf(sb, ")")
	case .Gt:
		x := node.value.(Pair).x
		y := node.value.(Pair).y
		fmt.sbprintf(sb, "(")
		dump_tree(x, sb)
		fmt.sbprintf(sb, ">")
		dump_tree(y, sb)
		fmt.sbprintf(sb, ")")
	case .Gte:
		x := node.value.(Pair).x
		y := node.value.(Pair).y
		fmt.sbprintf(sb, "(")
		dump_tree(x, sb)
		fmt.sbprintf(sb, ">=")
		dump_tree(y, sb)
		fmt.sbprintf(sb, ")")
	case .Sin:
		x := node.value.(^Node)
		fmt.sbprintf(sb, "sin(")
		dump_tree(x, sb)
		fmt.sbprintf(sb, ")")
	case .Expr:
		break
	case .Rand:
		x := node.value.(Pair).x
		y := node.value.(Pair).y
		fmt.sbprintf(sb, "rand(")
		dump_tree(x, sb)
		fmt.sbprintf(sb, ",")
		dump_tree(y, sb)
		fmt.sbprintf(sb, ")")
	}
}

generate_rule :: proc(tag: string, grammar: ^Grammar, depth: int) -> ^Node {
	rule, ok := grammar[tag]

	if !ok {
		return nil
	}

	if depth <= 0 {
		return nil
	}

	attempts := 0

	rng := rand.float32()

	for attempts < 10 {
		for expr in rule {
			if rng > expr.probability {
				rng -= expr.probability
				continue
			}

			node := generate_node_o(expr.value, grammar, depth - 1)

			if node != nil {
				return node
			}
		}
		attempts += 1
	}

	return nil
}

apply_to_rgba_node :: proc(data: []RGBAColor, root: ^Node, w: int, h: int) {
	mem.arena_init(&eval_arena, make([]byte, 1024 * 1024))

	for j in 0 ..< h {
		nj := f32(j) / f32(h) * 2. - 1
		for i in 0 ..< w {
			ni := f32(i) / f32(w) * 2. - 1

			n := eval(root, {ni, nj})

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
		ptr, err := mem.arena_alloc(a, size_of(Node))
		if err != nil {
			node = new(Node)
		} else {
			node = transmute(^Node)ptr
		}
	} else {
		node = new(Node)
	}

	node.kind = kind
	node.value = value
	return node
}

generate_image :: proc(tree: ^Node, file_name: string) {
	apply_to_rgba_node(image_buf[:], tree, WIDTH, HEIGHT)

	if image.write_png("output.png", WIDTH, HEIGHT, 4, &image_buf, WIDTH * size_of(RGBAColor)) ==
	   0 {
		fmt.println("Error writing image")
		os.exit(1)
	}
}

compile_shader :: proc(tree: ^Node, sb: ^strings.Builder) {
	strings.builder_reset(sb)

	base := `#version 330
in vec2 fragTexCoord;
out vec4 finalColor;

uniform float time;

vec4 map_color(vec3 rgb) {{
    return vec4((rgb + 1)/2.0, 1.0);
}}

float mod_f32(float x, float y) {{
    return x - y * floor(x / y);
}

void main() {{
    float x = fragTexCoord.x * 2.0 - 1.0;
    float y = fragTexCoord.y * 2.0 - 1.0;
    float t = sin(time);
    finalColor = map_color(`


	fmt.sbprintf(sb, base)

	gen_shader(tree, sb)

	fmt.sbprintf(sb, `);
}}`)
}

generate_real_time :: proc(tree: ^Node) -> int {
	show_tree := false

	sb := strings.builder_make()
	dump_tree(tree, &sb)

	tree_str := strings.clone_to_cstring(strings.to_string(sb))

	strings.builder_reset(&sb)
	compile_shader(tree, &sb)

	fragment := strings.to_cstring(&sb)
	fmt.println("Shader:")
	fmt.println(fragment)

	r.SetConfigFlags({.WINDOW_RESIZABLE})
	r.InitWindow(WIDTH, HEIGHT, "Random Art")
	defer r.CloseWindow()

	r.SetTargetFPS(60)
	shader := r.LoadShaderFromMemory(nil, fragment)

	time_loc := r.GetShaderLocation(shader, "time")

	texture := r.Texture{rlgl.GetTextureIdDefault(), 1, 1, 1, .UNCOMPRESSED_R8G8B8A8}

	pause := false
	time := f32(0)

	for !r.WindowShouldClose() {
		dt := r.GetFrameTime()

		r.SetShaderValue(shader, time_loc, &time, .FLOAT)

		r.BeginDrawing();{
			r.ClearBackground(r.WHITE)
			r.BeginShaderMode(shader);{
				r.DrawTexturePro(
					texture,
					{0, 0, 1, 1},
					{0, 0, f32(r.GetScreenWidth()), f32(r.GetScreenHeight())},
					0,
					0,
					r.RED,
				)
			};r.EndShaderMode()

			if show_tree {
				r.DrawText(tree_str, 10, 10, 20, r.BLACK)
			}
		};r.EndDrawing()

		if r.IsKeyPressed(.SPACE) {
			pause = !pause
		}

		if r.IsKeyPressed(.S) {
			generate_image(tree, "output.png")
		}

		if r.IsKeyPressed(.T) {
			show_tree = true
		}

		if r.IsKeyReleased(.T) {
			show_tree = false
		}

		if !pause {
			time += dt
		}
	}

	return 0
}

main :: proc() {
	grammar := make(Grammar)
	defer delete(grammar)

	grammar["E"] = []Expr {
		{
			1,
			new_node(
				.RGB,
				Triple{new_node(.Expr, "C"), new_node(.Expr, "C"), new_node(.Expr, "C")},
			),
		},
	}

	grammar["A"] = []Expr {
		{1. / 10., new_node(.Time)},
		{3. / 10., new_node(.Rand, Pair{new_node(.Number, -1), new_node(.Number, 1)})},
		{3. / 10., new_node(.X)},
		{3. / 10., new_node(.Y)},
	}


	grammar["C"] = []Expr {
		{1. / 5., new_node(.Expr, "A")},
		{1. / 5., new_node(.Plus, Pair{new_node(.Expr, "C"), new_node(.Expr, "C")})},
		{1. / 5., new_node(.Mul, Pair{new_node(.Expr, "C"), new_node(.Expr, "C")})},
		{1. / 5., new_node(.Sin, new_node(.Expr, "C"))},
		{1. / 5., new_node(.Div, Pair{new_node(.Expr, "C"), new_node(.Expr, "C")})},
	}

	root := generate_rule("E", &grammar, 20)

	if root == nil {
		fmt.eprintln("ERROR: not possible to generate the tree")
		os.exit(1)
	}

	tree_builder := strings.builder_make()
	dump_tree(root, &tree_builder)

	fmt.println(strings.to_string(tree_builder))

	generate_real_time(root)
}
