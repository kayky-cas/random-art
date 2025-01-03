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

wrap_inf :: proc(x: f32) -> f32 {
	#partial switch math.classify_f32(x) {
	case .NaN:
		return 0
	case .Inf:
		return 1
	case .Neg_Inf:
		return -1
	}

	return x
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
			return new_node(.Number, wrap_inf(x.value.(f32) / y.value.(f32)))
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
			return new_node(.Number, wrap_inf(math.sinh(x.value.(f32))))
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

dump_grammar :: proc(g: ^Grammar, sb: ^strings.Builder) {
	for tag, rule in g {
		fmt.sbprintf(sb, "%s -> ", tag)
		for expr in rule {
			fmt.sbprintf(sb, "%f ", expr.probability)
			dump_tree(expr.value, sb)
			fmt.sbprintf(sb, " | ")
		}
		fmt.sbprintf(sb, "\n")
	}
}

dump_tree :: proc(node: ^Node, sb: ^strings.Builder) {
	if node == nil {
		return
	}
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
		fmt.sbprintf(sb, "%s", node.value.(string))
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

			node := generate_node(expr.value, grammar, depth - 1)

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
	show_fps := false

	sb := strings.builder_make()
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

	sb_fps := strings.builder_make()

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

			if show_fps {
				fmt.sbprintf(&sb_fps, "%d", r.GetFPS())
				r.DrawText(strings.to_cstring(&sb_fps), 10, 10, 20, r.BLACK)
				strings.builder_reset(&sb_fps)
			}
		};r.EndDrawing()

		if r.IsKeyPressed(.SPACE) {
			pause = !pause
		}

		if r.IsKeyPressed(.S) {
			generate_image(tree, "output.png")
		}

		if r.IsKeyPressed(.T) {
			show_fps = true
		}

		if r.IsKeyReleased(.T) {
			show_fps = false
		}

		if !pause {
			time += dt
		}
	}

	return 0
}

TokenKind :: enum {
	Plus, // +
	Star, // *
	Slash, // /
	Pipe, // |
	X, // x
	Y, // y
	Ident, // [a-zA-Z][a-zA-Z0-9]*
	Number, // [0-9]+(\.[0-9]+)?
	Invalid,
	EOF,
	LParen, // (
	RParen, // )
	Comma, // ,
}

Token :: struct {
	kind:  TokenKind,
	value: string,
	pos:   int,
}

Lexer :: struct {
	source: string,
	pos:    int,
}

peek_char :: proc(l: ^Lexer) -> byte {
	if l.pos >= len(l.source) {
		return 0
	}
	return l.source[l.pos]
}

next_char :: proc(l: ^Lexer) -> byte {
	if l.pos >= len(l.source) {
		return 0
	}

	c := l.source[l.pos]
	l.pos += 1

	return c
}

skip_whitespace :: proc(l: ^Lexer) {
	for c := peek_char(l); strings.is_space(rune(c)); c = peek_char(l) {
		next_char(l)
	}
}

next_token :: proc(l: ^Lexer) -> Token {
	skip_whitespace(l)

	token := Token {
		kind = .Invalid,
		pos  = l.pos,
	}

	ch := next_char(l)

	switch ch {
	case '+':
		token.kind = .Plus
		token.value = "+"
	case '*':
		token.kind = .Star
		token.value = "*"
	case '/':
		token.kind = .Slash
		token.value = "/"
	case '|':
		token.kind = .Pipe
		token.value = "|"
	case 'x':
		token.kind = .X
		token.value = "x"
	case 'y':
		token.kind = .Y
		token.value = "y"
	case '(':
		token.kind = .LParen
		token.value = "("
	case ')':
		token.kind = .RParen
		token.value = ")"
	case ',':
		token.kind = .Comma
		token.value = ","
	case 0:
		token.kind = .EOF
	case '0' ..= '9':
		token.kind = .Number

		has_dot := ch == '.'

		for ch := peek_char(l);
		    ch != 0 && (ch >= '0' && ch <= '9' || (!has_dot && ch == '.'));
		    ch = peek_char(l) {
			if ch == '.' {
				has_dot = true
			}
			next_char(l)
		}

	case 'a' ..= 'z', 'A' ..= 'Z':
		token.kind = .Ident

		for ch := peek_char(l);
		    ch != 0 &&
		    (ch >= 'a' && ch <= 'z' || ch >= 'A' && ch <= 'Z' || ch >= '0' && ch <= '9');
		    ch = peek_char(l) {
			next_char(l)
		}

		token.value = l.source[token.pos:l.pos]
	}

	return token
}

parse_rgb :: proc(l: ^Lexer) -> ^Node {
	rgb_ident := next_token(l)

	if rgb_ident.kind != .Ident || rgb_ident.value != "rgb" || next_token(l).kind != .LParen {
		return nil
	}

	r := parse_node(l)

	if r == nil || next_token(l).kind != .Comma {
		return nil
	}

	g := parse_node(l)

	if g == nil || next_token(l).kind != .Comma {
		return nil
	}

	b := parse_node(l)

	if b == nil || next_token(l).kind != .RParen {
		return nil
	}

	return new_node(NodeKind.RGB, Triple{r, g, b})
}

parse_ident :: proc(l: ^Lexer) -> ^Node {
	token := next_token(l)
	if token.kind != .Ident {
		return nil
	}
	return new_node(NodeKind.Expr, token.value)
}

parse_add :: proc(l: ^Lexer) -> ^Node {
	add_ident := next_token(l)

	if add_ident.kind != .Ident || add_ident.value != "add" || next_token(l).kind != .LParen {
		return nil
	}

	lhs := parse_node(l)

	if lhs == nil || next_token(l).kind != .Comma {
		return nil
	}

	rhs := parse_node(l)

	if rhs == nil || next_token(l).kind != .RParen {
		return nil
	}

	return new_node(NodeKind.Plus, Pair{lhs, rhs})
}


parse_mul :: proc(l: ^Lexer) -> ^Node {
	mul_ident := next_token(l)

	if mul_ident.kind != .Ident || mul_ident.value != "mul" || next_token(l).kind != .LParen {
		return nil
	}

	lhs := parse_node(l)

	if lhs == nil || next_token(l).kind != .Comma {
		return nil
	}

	rhs := parse_node(l)

	if rhs == nil || next_token(l).kind != .RParen {
		return nil
	}

	return new_node(NodeKind.Mul, Pair{lhs, rhs})
}

parse_div :: proc(l: ^Lexer) -> ^Node {
	div_ident := next_token(l)

	if div_ident.kind != .Ident || div_ident.value != "div" || next_token(l).kind != .LParen {
		return nil
	}

	lhs := parse_node(l)

	if lhs == nil || next_token(l).kind != .Comma {
		return nil
	}

	rhs := parse_node(l)

	if rhs == nil || next_token(l).kind != .RParen {
		return nil
	}

	return new_node(NodeKind.Div, Pair{lhs, rhs})
}

parse_sin :: proc(l: ^Lexer) -> ^Node {
	sin_ident := next_token(l)

	if sin_ident.kind != .Ident || sin_ident.value != "sin" || next_token(l).kind != .LParen {
		return nil
	}

	v := parse_node(l)

	if v == nil || next_token(l).kind != .RParen {
		return nil
	}

	return new_node(NodeKind.Sin, v)
}

parse_rand :: proc(l: ^Lexer) -> ^Node {
	rand_ident := parse_ident(l)

	if rand_ident == nil ||
	   rand_ident.value.(string) != "rand" ||
	   next_token(l).kind != .LParen ||
	   next_token(l).kind != .RParen {
		return nil
	}

	return new_node(NodeKind.Rand, Pair{new_node(.Number, -1), new_node(.Number, 1)})
}

parse_time :: proc(l: ^Lexer) -> ^Node {
	time_ident := parse_ident(l)

	if time_ident == nil ||
	   time_ident.value.(string) != "time" ||
	   next_token(l).kind != .LParen ||
	   next_token(l).kind != .RParen {
		return nil
	}

	return new_node(NodeKind.Time)
}

parse_x :: proc(l: ^Lexer) -> ^Node {
	x_token := next_token(l)

	if x_token.kind != .X {
		return nil
	}

	return new_node(NodeKind.X)
}

parse_y :: proc(l: ^Lexer) -> ^Node {
	y_token := next_token(l)

	if y_token.kind != .Y {
		return nil
	}

	return new_node(NodeKind.Y)
}

parse_node :: proc(l: ^Lexer) -> ^Node {
	pos := l.pos

	if node := parse_rgb(l); node != nil {
		return node
	}

	l.pos = pos

	if node := parse_add(l); node != nil {
		return node
	}

	l.pos = pos

	if node := parse_mul(l); node != nil {
		return node
	}

	l.pos = pos

	if node := parse_rand(l); node != nil {
		return node
	}

	l.pos = pos

	if node := parse_time(l); node != nil {
		return node
	}

	l.pos = pos

	if node := parse_div(l); node != nil {
		return node
	}

	l.pos = pos

	if node := parse_sin(l); node != nil {
		return node
	}

	l.pos = pos

	if node := parse_x(l); node != nil {
		return node
	}

	l.pos = pos

	if node := parse_y(l); node != nil {
		return node
	}

	l.pos = pos

	if node := parse_ident(l); node != nil {
		return node
	}

	l.pos = pos

	return nil
}

parse_expr :: proc(l: ^Lexer) -> (Expr, bool) {
	pipes_count := 0

	token := next_token(l)

	for ; token.kind == .Pipe; token = next_token(l) {
		pipes_count += 1
	}

	l.pos = token.pos

	if pipes_count == 0 {
		return {}, false
	}

	node := parse_node(l)

	if node == nil {
		return {}, false
	}

	return {f32(pipes_count), node}, true
}

parse_rule :: proc(l: ^Lexer) -> (string, []Expr) {
	tag_token := next_token(l)

	if tag_token.kind != .Ident {
		return "", nil
	}

	tag := tag_token.value

	exprs: [dynamic]Expr

	all_pipes: f32 = 0

	for expr, ok := parse_expr(l); ok; expr, ok = parse_expr(l) {
		all_pipes += expr.probability
		append(&exprs, expr)
	}

	for &expr in exprs {
		expr.probability /= all_pipes
	}

	return tag, exprs[:]
}

parse_grammar :: proc(l: ^Lexer) -> (string, Grammar) {
	grammar := make(Grammar)
	first_rule := ""

	for tag, rule := parse_rule(l); tag != ""; tag, rule = parse_rule(l) {
		if first_rule == "" {
			first_rule = tag
		}

		grammar[tag] = rule
	}

	return first_rule, grammar
}

main :: proc() {
	data, ok := os.read_entire_file("./grammar.ra")

	if !ok {
		fmt.eprintln("Error reading file")
		os.exit(1)
	}

	lexer := Lexer{string(data), 0}

	rule, grammar := parse_grammar(&lexer)
	defer delete(grammar)

	g_sb := strings.builder_make()
	dump_grammar(&grammar, &g_sb)
	fmt.println(strings.to_string(g_sb))

	root := generate_rule(rule, &grammar, 70)

	if root == nil {
		fmt.eprintln("ERROR: not possible to generate the tree")
		os.exit(1)
	}

	generate_real_time(root)
}
