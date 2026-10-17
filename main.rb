
# frozen_string_literal: true

# Ruby Calculator Language Study
# Concepts: dynamic typing, duck typing, lambdas, closures, hashes,
# classes, exceptions, recursion, collections, and AST interpretation.

Token = Struct.new(:kind, :text)
HistoryEntry = Struct.new(:input, :result)

class CalculatorError < StandardError; end

class Lexer
  def initialize(source)
    @source = source
    @position = 0
  end

  def tokenize
    tokens = []
    while @position < @source.length
      char = @source[@position]
      if char.match?(/\s/)
        @position += 1
      elsif char.match?(/[0-9.]/)
        tokens << read_number
      elsif char.match?(/[A-Za-z_]/)
        tokens << read_identifier
      else
        kind = {
          "+" => :plus, "-" => :minus, "*" => :star, "/" => :slash,
          "%" => :percent, "^" => :caret, "(" => :lparen,
          ")" => :rparen, "," => :comma
        }[char]
        raise CalculatorError, "unexpected character #{char.inspect}" unless kind

        tokens << Token.new(kind, char)
        @position += 1
      end
    end
    tokens << Token.new(:eof, "")
    tokens
  end

  private

  def read_number
    start = @position
    decimal_seen = false
    while @position < @source.length
      char = @source[@position]
      if char.match?(/[0-9]/)
        @position += 1
      elsif char == "." && !decimal_seen
        decimal_seen = true
        @position += 1
      else
        break
      end
    end
    text = @source[start...@position]
    raise CalculatorError, "invalid number '.'" if text == "."
    raise CalculatorError, "invalid number #{text.inspect}" unless Float(text, exception: false)

    Token.new(:number, text)
  end

  def read_identifier
    start = @position
    @position += 1
    while @position < @source.length && @source[@position].match?(/[A-Za-z0-9_]/)
      @position += 1
    end
    Token.new(:identifier, @source[start...@position])
  end
end

# AST nodes use Struct for compact value-like data objects.
NumberExpr = Struct.new(:value)
VariableExpr = Struct.new(:name)
UnaryExpr = Struct.new(:operator, :operand)
BinaryExpr = Struct.new(:left, :operator, :right)
CallExpr = Struct.new(:name, :arguments)

class Parser
  def initialize(tokens)
    @tokens = tokens
    @position = 0
  end

  def parse
    expression = parse_addition
    raise CalculatorError, "unexpected token #{current.text.inspect}" unless current.kind == :eof

    expression
  end

  private

  def current = @tokens[@position]

  def advance
    token = current
    @position += 1
    token
  end

  def match(kind)
    return false unless current.kind == kind

    advance
    true
  end

  def expect(kind)
    return if match(kind)

    raise CalculatorError, "expected #{kind}, found #{current.text.inspect}"
  end

  def parse_addition
    expression = parse_multiplication
    while %i[plus minus].include?(current.kind)
      operator = advance.text
      expression = BinaryExpr.new(expression, operator, parse_multiplication)
    end
    expression
  end

  def parse_multiplication
    expression = parse_unary
    while %i[star slash percent].include?(current.kind)
      operator = advance.text
      expression = BinaryExpr.new(expression, operator, parse_unary)
    end
    expression
  end

  def parse_unary
    return UnaryExpr.new(advance.text, parse_unary) if %i[plus minus].include?(current.kind)

    parse_power
  end

  def parse_power
    left = parse_primary
    return left unless match(:caret)

    BinaryExpr.new(left, "^", parse_unary)
  end

  def parse_primary
    token = current
    case token.kind
    when :number
      advance
      NumberExpr.new(Float(token.text))
    when :identifier
      advance
      if match(:lparen)
        args = []
        unless current.kind == :rparen
          loop do
            args << parse_addition
            break unless match(:comma)
          end
        end
        expect(:rparen)
        CallExpr.new(token.text, args)
      else
        VariableExpr.new(token.text)
      end
    when :lparen
      advance
      expression = parse_addition
      expect(:rparen)
      expression
    else
      raise CalculatorError, "expected a number, variable, or parenthesized expression"
    end
  end
end

class Evaluator
  def initialize(variables)
    @variables = variables
    # Lambdas are first-class objects and can be stored in a Hash.
    @functions = {
      "sqrt" => ->(x) { Math.sqrt(x) },
      "abs" => ->(x) { x.abs },
      "sin" => ->(x) { Math.sin(x) },
      "cos" => ->(x) { Math.cos(x) },
      "tan" => ->(x) { Math.tan(x) },
      "log" => ->(x) { Math.log10(x) },
      "ln" => ->(x) { Math.log(x) }
    }
  end

  def evaluate(expression)
    result = case expression
             when NumberExpr
               expression.value
             when VariableExpr
               @variables.fetch(expression.name) do
                 raise CalculatorError, "unknown variable #{expression.name.inspect}"
               end
             when UnaryExpr
               value = evaluate(expression.operand)
               expression.operator == "-" ? -value : value
             when BinaryExpr
               left = evaluate(expression.left)
               right = evaluate(expression.right)
               case expression.operator
               when "+" then left + right
               when "-" then left - right
               when "*" then left * right
               when "/"
                 raise CalculatorError, "cannot divide by zero" if right.zero?
                 left / right
               when "%"
                 raise CalculatorError, "cannot modulo by zero" if right.zero?
                 left % right
               when "^" then left**right
               end
             when CallExpr
               function = @functions[expression.name]
               raise CalculatorError, "unknown function #{expression.name.inspect}" unless function
               raise CalculatorError, "function #{expression.name.inspect} expects one argument" unless expression.arguments.length == 1

               function.call(evaluate(expression.arguments.first))
             else
               raise CalculatorError, "unsupported expression type"
             end

    raise CalculatorError, "result is not a finite real number" unless result.finite?

    result
  rescue Math::DomainError, FloatDomainError => e
    raise CalculatorError, e.message
  end
end

class Calculator
  ASSIGNMENT = /\A([a-zA-Z_][a-zA-Z0-9_]*)\s*=\s*(.+)\z/

  def initialize
    @variables = { "pi" => Math::PI, "e" => Math::E }
    @history = []
  end

  def run
    puts "Ruby Calculator. Type 'help' for commands, 'exit' to quit."
    loop do
      print "> "
      input = $stdin.gets
      break unless input

      line = input.strip
      next if line.empty?

      begin
        case line.downcase
        when "exit", "quit"
          puts "Goodbye."
          break
        when "help" then show_help
        when "history" then show_history
        when "clear"
          @history.clear
          puts "History cleared."
        when "variables" then show_variables
        when "clearvars"
          @variables = { "pi" => Math::PI, "e" => Math::E }
          puts "Variables cleared."
        when "test" then run_tests
        when "experiments" then run_experiments
        else puts evaluate_line(line)
        end
      rescue CalculatorError, ArgumentError, ZeroDivisionError => e
        puts "Error: #{e.message}"
      end
    end
  end

  private

  def calculate(source)
    tokens = Lexer.new(source).tokenize
    expression = Parser.new(tokens).parse
    Evaluator.new(@variables).evaluate(expression)
  end

  def evaluate_line(line)
    if (match = ASSIGNMENT.match(line))
      name, source = match.captures
      raise CalculatorError, "cannot overwrite constant #{name.inspect}" if %w[pi e].include?(name)

      value = calculate(source)
      @variables[name] = value
      result = format_number(value)
      @history << HistoryEntry.new(line, result)
      return "#{name} = #{result}"
    end

    value = calculate(line)
    result = format_number(value)
    @history << HistoryEntry.new(line, result)
    result
  end

  def format_number(value)
    return "0" if value.zero?

    value.to_s
  end

  def show_help
    puts <<~HELP
      Operations: + - * / % ^
      Functions:  sqrt, abs, sin, cos, tan, log, ln
      Constants:  pi, e
      Assignment: x = 10
      Commands:   help, history, clear, variables, clearvars, test, experiments, exit

      Trigonometric functions use radians. log is base 10; ln is natural logarithm.
    HELP
  end

  def show_history
    return puts("No calculations yet.") if @history.empty?

    @history.each_with_index do |entry, index|
      puts "#{index + 1}. #{entry.input} = #{entry.result}"
    end
  end

  def show_variables
    return puts("No variables.") if @variables.empty?

    @variables.sort.each { |name, value| puts "#{name} = #{format_number(value)}" }
  end

  def factorial_recursive(n)
    n <= 1 ? 1 : n * factorial_recursive(n - 1)
  end

  def factorial_iterative(n)
    (2..n).reduce(1, :*)
  end

  def run_experiments
    puts "=== Ruby Concept Experiments ==="
    numbers = [1, 2, 3, 4, 5]
    puts "map: #{numbers.inspect} -> #{numbers.map { |n| n * n }.inspect}"
    puts "recursive factorial(5): #{factorial_recursive(5)}"
    puts "iterative factorial(5): #{factorial_iterative(5)}"

    multiplier = 3
    multiply_by_three = ->(n) { n * multiplier } # closure
    puts "closure multiply_by_three(7): #{multiply_by_three.call(7)}"

    mutable = [1, 2, 3]
    alias_to_same_array = mutable
    mutable << 4
    puts "array alias observes mutation: #{alias_to_same_array.inspect}"
    puts "TODO: compare object identity, exceptions vs result values, threads, and benchmarks."
  end

  def run_tests
    tests = {
      "2 + 3" => 5.0,
      "10 - 4" => 6.0,
      "5 * 6" => 30.0,
      "20 / 4" => 5.0,
      "10 % 3" => 1.0,
      "2 ^ 8" => 256.0,
      "2 + 3 * 4" => 14.0,
      "(2 + 3) * 4" => 20.0,
      "2 ^ 3 ^ 2" => 512.0,
      "-2^2" => -4.0,
      "-5 * 2" => -10.0,
      "sqrt(25)" => 5.0,
      "abs(-10)" => 10.0,
      "sin(pi / 2)" => 1.0
    }

    passed = 0
    tests.each do |input, expected|
      begin
        actual = calculate(input)
        if (actual - expected).abs < 1e-9
          passed += 1
          puts "PASS: #{input}"
        else
          puts "FAIL: #{input} (expected #{expected}, got #{actual})"
        end
      rescue CalculatorError => e
        puts "FAIL: #{input} (#{e.message})"
      end
    end
    puts "#{passed}/#{tests.length} tests passed."
  end
end

Calculator.new.run
