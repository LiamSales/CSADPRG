
package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
	"regexp"
	"sort"
	"strconv"
	"strings"
	"unicode"
)

// Calculator language study: lexer, recursive-descent parser, AST,
// evaluator, mutable environment, history, tests, and concept experiments.

type Token struct {
	Kind string
	Text string
}

type Lexer struct {
	input string
	pos   int
}

func (l *Lexer) Tokenize() ([]Token, error) {
	tokens := []Token{}
	for l.pos < len(l.input) {
		r := rune(l.input[l.pos])
		if unicode.IsSpace(r) {
			l.pos++
			continue
		}
		if (r >= '0' && r <= '9') || r == '.' {
			start := l.pos
			dot := false
			for l.pos < len(l.input) {
				c := l.input[l.pos]
				if c >= '0' && c <= '9' {
					l.pos++
				} else if c == '.' && !dot {
					dot = true
					l.pos++
				} else {
					break
				}
			}
			text := l.input[start:l.pos]
			if text == "." {
				return nil, fmt.Errorf("invalid number '.'")
			}
			if _, err := strconv.ParseFloat(text, 64); err != nil {
				return nil, fmt.Errorf("invalid number %q", text)
			}
			tokens = append(tokens, Token{"NUMBER", text})
			continue
		}
		if unicode.IsLetter(r) || r == '_' {
			start := l.pos
			for l.pos < len(l.input) {
				c := rune(l.input[l.pos])
				if unicode.IsLetter(c) || unicode.IsDigit(c) || c == '_' {
					l.pos++
				} else {
					break
				}
			}
			tokens = append(tokens, Token{"IDENT", l.input[start:l.pos]})
			continue
		}
		kind := map[byte]string{
			'+': "PLUS", '-': "MINUS", '*': "STAR", '/': "SLASH",
			'%': "PERCENT", '^': "CARET", '(': "LPAREN",
			')': "RPAREN", ',': "COMMA",
		}[l.input[l.pos]]
		if kind == "" {
			return nil, fmt.Errorf("unexpected character %q", l.input[l.pos])
		}
		tokens = append(tokens, Token{kind, l.input[l.pos : l.pos+1]})
		l.pos++
	}
	return append(tokens, Token{"EOF", ""}), nil
}

type Expr interface{ isExpr() }

type Number struct{ Value float64 }

func (Number) isExpr() {}

type Variable struct{ Name string }

func (Variable) isExpr() {}

type Unary struct {
	Op    string
	Right Expr
}

func (Unary) isExpr() {}

type Binary struct {
	Left  Expr
	Op    string
	Right Expr
}

func (Binary) isExpr() {}

type Call struct {
	Name string
	Args []Expr
}

func (Call) isExpr() {}

type Parser struct {
	tokens []Token
	pos    int
}

func (p *Parser) current() Token { return p.tokens[p.pos] }

func (p *Parser) advance() Token {
	t := p.current()
	p.pos++
	return t
}

func (p *Parser) match(kind string) bool {
	if p.current().Kind != kind {
		return false
	}
	p.advance()
	return true
}

func (p *Parser) expect(kind string) error {
	if !p.match(kind) {
		return fmt.Errorf("expected %s, found %q", kind, p.current().Text)
	}
	return nil
}

func (p *Parser) Parse() (Expr, error) {
	e, err := p.addition()
	if err != nil {
		return nil, err
	}
	if p.current().Kind != "EOF" {
		return nil, fmt.Errorf("unexpected token %q", p.current().Text)
	}
	return e, nil
}

func (p *Parser) addition() (Expr, error) {
	e, err := p.multiplication()
	if err != nil {
		return nil, err
	}
	for p.current().Kind == "PLUS" || p.current().Kind == "MINUS" {
		op := p.advance().Text
		r, er := p.multiplication()
		if er != nil {
			return nil, er
		}
		e = Binary{e, op, r}
	}
	return e, nil
}

func (p *Parser) multiplication() (Expr, error) {
	e, err := p.unary()
	if err != nil {
		return nil, err
	}
	for {
		k := p.current().Kind
		if k != "STAR" && k != "SLASH" && k != "PERCENT" {
			break
		}
		op := p.advance().Text
		r, er := p.unary()
		if er != nil {
			return nil, er
		}
		e = Binary{e, op, r}
	}
	return e, nil
}

func (p *Parser) unary() (Expr, error) {
	if p.current().Kind == "PLUS" || p.current().Kind == "MINUS" {
		op := p.advance().Text
		r, err := p.unary()
		if err != nil {
			return nil, err
		}
		return Unary{op, r}, nil
	}
	return p.power()
}

func (p *Parser) power() (Expr, error) {
	left, err := p.primary()
	if err != nil {
		return nil, err
	}
	if p.match("CARET") {
		right, er := p.unary()
		if er != nil {
			return nil, er
		}
		return Binary{left, "^", right}, nil
	}
	return left, nil
}

func (p *Parser) primary() (Expr, error) {
	t := p.current()
	switch t.Kind {
	case "NUMBER":
		p.advance()
		v, _ := strconv.ParseFloat(t.Text, 64)
		return Number{v}, nil
	case "IDENT":
		p.advance()
		if p.match("LPAREN") {
			args := []Expr{}
			if p.current().Kind != "RPAREN" {
				for {
					a, err := p.addition()
					if err != nil {
						return nil, err
					}
					args = append(args, a)
					if !p.match("COMMA") {
						break
					}
				}
			}
			if err := p.expect("RPAREN"); err != nil {
				return nil, err
			}
			return Call{t.Text, args}, nil
		}
		return Variable{t.Text}, nil
	case "LPAREN":
		p.advance()
		e, err := p.addition()
		if err != nil {
			return nil, err
		}
		if err := p.expect("RPAREN"); err != nil {
			return nil, err
		}
		return e, nil
	default:
		return nil, fmt.Errorf("expected a number, variable, or parenthesized expression")
	}
}

type Environment map[string]float64
type Function func(float64) float64

type Evaluator struct {
	Vars      Environment
	Functions map[string]Function
}

func NewEvaluator(vars Environment) *Evaluator {
	return &Evaluator{Vars: vars, Functions: map[string]Function{
		"sqrt": math.Sqrt, "abs": math.Abs, "sin": math.Sin,
		"cos": math.Cos, "tan": math.Tan, "log": math.Log10, "ln": math.Log,
	}}
}

func (e *Evaluator) Eval(expr Expr) (float64, error) {
	var result float64
	switch n := expr.(type) {
	case Number:
		result = n.Value
	case Variable:
		v, ok := e.Vars[n.Name]
		if !ok {
			return 0, fmt.Errorf("unknown variable %q", n.Name)
		}
		result = v
	case Unary:
		v, err := e.Eval(n.Right)
		if err != nil {
			return 0, err
		}
		if n.Op == "-" {
			result = -v
		} else {
			result = v
		}
	case Binary:
		l, err := e.Eval(n.Left)
		if err != nil {
			return 0, err
		}
		r, err := e.Eval(n.Right)
		if err != nil {
			return 0, err
		}
		switch n.Op {
		case "+":
			result = l + r
		case "-":
			result = l - r
		case "*":
			result = l * r
		case "/":
			if r == 0 {
				return 0, fmt.Errorf("cannot divide by zero")
			}
			result = l / r
		case "%":
			if r == 0 {
				return 0, fmt.Errorf("cannot modulo by zero")
			}
			result = math.Mod(l, r)
		case "^":
			result = math.Pow(l, r)
		}
	case Call:
		fn, ok := e.Functions[n.Name]
		if !ok {
			return 0, fmt.Errorf("unknown function %q", n.Name)
		}
		if len(n.Args) != 1 {
			return 0, fmt.Errorf("function %q expects one argument", n.Name)
		}
		v, err := e.Eval(n.Args[0])
		if err != nil {
			return 0, err
		}
		result = fn(v)
	default:
		return 0, fmt.Errorf("unsupported expression type")
	}
	if math.IsNaN(result) || math.IsInf(result, 0) {
		return 0, fmt.Errorf("result is not a finite real number")
	}
	return result, nil
}

type HistoryEntry struct{ Input, Result string }

type Calculator struct {
	vars    Environment
	history []HistoryEntry
}

func NewCalculator() *Calculator {
	return &Calculator{vars: Environment{"pi": math.Pi, "e": math.E}}
}

func format(v float64) string {
	if v == 0 {
		return "0"
	}
	return strconv.FormatFloat(v, 'f', -1, 64)
}

var assignmentRE = regexp.MustCompile(`^([a-zA-Z_][a-zA-Z0-9_]*)\s*=\s*(.+)$`)

func (c *Calculator) calculate(source string) (float64, error) {
	tokens, err := (&Lexer{input: source}).Tokenize()
	if err != nil {
		return 0, err
	}
	expr, err := (&Parser{tokens: tokens}).Parse()
	if err != nil {
		return 0, err
	}
	return NewEvaluator(c.vars).Eval(expr)
}

func (c *Calculator) evaluateLine(line string) (string, error) {
	if m := assignmentRE.FindStringSubmatch(line); m != nil {
		name, source := m[1], m[2]
		if name == "pi" || name == "e" {
			return "", fmt.Errorf("cannot overwrite constant %q", name)
		}
		v, err := c.calculate(source)
		if err != nil {
			return "", err
		}
		c.vars[name] = v
		out := format(v)
		c.history = append(c.history, HistoryEntry{line, out})
		return name + " = " + out, nil
	}
	v, err := c.calculate(line)
	if err != nil {
		return "", err
	}
	out := format(v)
	c.history = append(c.history, HistoryEntry{line, out})
	return out, nil
}

func (c *Calculator) showHelp() {
	fmt.Println(`Operations: + - * / % ^
Functions: sqrt, abs, sin, cos, tan, log, ln
Constants: pi, e
Assignment: x = 10
Commands: help, history, clear, variables, clearvars, test, experiments, exit
Trigonometric functions use radians. log is base 10; ln is natural logarithm.`)
}

func (c *Calculator) showHistory() {
	if len(c.history) == 0 {
		fmt.Println("No calculations yet.")
		return
	}
	for i, h := range c.history {
		fmt.Printf("%d. %s = %s\n", i+1, h.Input, h.Result)
	}
}

func (c *Calculator) showVariables() {
	keys := make([]string, 0, len(c.vars))
	for k := range c.vars {
		keys = append(keys, k)
	}
	sort.Strings(keys)
	for _, k := range keys {
		fmt.Printf("%s = %s\n", k, format(c.vars[k]))
	}
}

func factorialRecursive(n int) int {
	if n <= 1 {
		return 1
	}
	return n * factorialRecursive(n-1)
}

func factorialIterative(n int) int {
	result := 1
	for i := 2; i <= n; i++ {
		result *= i
	}
	return result
}

func (c *Calculator) experiments() {
	fmt.Println("=== Go Concept Experiments ===")
	numbers := []int{1, 2, 3, 4, 5}
	squares := make([]int, len(numbers))
	for i, v := range numbers {
		squares[i] = v * v
	}
	fmt.Println("slice:", numbers, "squares:", squares)
	fmt.Println("recursive factorial(5):", factorialRecursive(5))
	fmt.Println("iterative factorial(5):", factorialIterative(5))
	multiplier := 3
	multiplyByThree := func(x int) int { return x * multiplier } // closure
	fmt.Println("closure multiplyByThree(7):", multiplyByThree(7))
	fmt.Println("TODO: compare pointer/value behavior, interfaces, goroutines, channels, allocations, and benchmarks.")
}

func (c *Calculator) tests() {
	tests := []struct {
		input string
		want  float64
	}{
		{"2 + 3", 5}, {"10 - 4", 6}, {"5 * 6", 30}, {"20 / 4", 5}, {"10 % 3", 1},
		{"2 ^ 8", 256}, {"2 + 3 * 4", 14}, {"(2 + 3) * 4", 20},
		{"2 ^ 3 ^ 2", 512}, {"-2^2", -4}, {"-5 * 2", -10}, {"sqrt(25)", 5},
		{"abs(-10)", 10}, {"sin(pi / 2)", 1},
	}
	passed := 0
	for _, t := range tests {
		v, err := c.calculate(t.input)
		if err == nil && math.Abs(v-t.want) < 1e-9 {
			passed++
			fmt.Println("PASS:", t.input)
		} else {
			fmt.Printf("FAIL: %s (want %v, got %v, err %v)\n", t.input, t.want, v, err)
		}
	}
	fmt.Printf("%d/%d tests passed.\n", passed, len(tests))
}

func (c *Calculator) Run() {
	fmt.Println("Go Calculator. Type 'help' for commands, 'exit' to quit.")
	scanner := bufio.NewScanner(os.Stdin)
	for {
		fmt.Print("> ")
		if !scanner.Scan() {
			return
		}
		line := strings.TrimSpace(scanner.Text())
		if line == "" {
			continue
		}
		switch strings.ToLower(line) {
		case "exit", "quit":
			fmt.Println("Goodbye.")
			return
		case "help":
			c.showHelp()
		case "history":
			c.showHistory()
		case "clear":
			c.history = nil
			fmt.Println("History cleared.")
		case "variables":
			c.showVariables()
		case "clearvars":
			c.vars = Environment{"pi": math.Pi, "e": math.E}
			fmt.Println("Variables cleared.")
		case "test":
			c.tests()
		case "experiments":
			c.experiments()
		default:
			out, err := c.evaluateLine(line)
			if err != nil {
				fmt.Println("Error:", err)
			} else {
				fmt.Println(out)
			}
		}
	}
}

func main() {
	NewCalculator().Run()
}
