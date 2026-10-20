import kotlin.math.*
import java.math.BigDecimal

private enum class TokenType {
    NUMBER, IDENTIFIER,
    PLUS, MINUS, STAR, SLASH, PERCENT, CARET,
    LPAREN, RPAREN, COMMA, EOF
}

private data class Token(
    val type: TokenType,
    val text: String
) //datatype + data

private class CalculatorException(message: String) :
    RuntimeException(message)

private class Lexer(private val source: String) { //converts raw source code into tokens
    
    private var position = 0 //just a pointer, since we get a string it does it character by character

    fun tokenize(): List<Token> { //list of tokens is the input transformed into output
        val tokens = mutableListOf<Token>() //empty container

        while (position < source.length) { //iterate
            val c = source[position] //read char at the current position

            when {
                c.isWhitespace() -> position++ //ignore

                c.isDigit() || c == '.' ->
                    tokens.add(readNumber()) //one token, but would it be the same number??

                c.isLetter() || c == '_' ->
                    tokens.add(readIdentifier())

                else -> {
                    val type = when (c) {
                        '+' -> TokenType.PLUS
                        '-' -> TokenType.MINUS
                        '*' -> TokenType.STAR
                        '/' -> TokenType.SLASH
                        '%' -> TokenType.PERCENT
                        '^' -> TokenType.CARET
                        '(' -> TokenType.LPAREN
                        ')' -> TokenType.RPAREN
                        ',' -> TokenType.COMMA
                        else -> throw CalculatorException(
                            "Unexpected character '$c'"
                        )
                    }

                    tokens.add(Token(type, c.toString())) 
                    position++
                }
            }
        }

        tokens.add(Token(TokenType.EOF, ""))
        return tokens
    }

    private fun readNumber(): Token { // no parameter
        val start = position //position is still in the scope of the class
        var decimalSeen = false

        while (position < source.length) { //still in the scope
            val c = source[position] //just like the lexer

            when {
                c.isDigit() -> position++

                c == '.' && !decimalSeen -> {
                    decimalSeen = true
                    position++
                } //only one decimal pass, but how do we know where to keep decimal

                else -> break //breaks when not number nor first decimal
            }
        }

        val text = source.substring(start, position)

        if (text == ".") {
            throw CalculatorException("Invalid number '.'")
        }

        return Token(TokenType.NUMBER, text)
    }

    private fun readIdentifier(): Token {
        val start = position

        while (
            position < source.length &&
            (
                source[position].isLetterOrDigit() ||
                source[position] == '_'
            )
        ) {
            position++
        }

        return Token(
            TokenType.IDENTIFIER,
            source.substring(start, position)
        )
    }
}

// ============================================================
// 2. ABSTRACT SYNTAX TREE
// ============================================================

private sealed interface Expression

private data class NumberExpression(
    val value: Double
) : Expression

private data class VariableExpression(
    val name: String
) : Expression

private data class UnaryExpression(
    val operator: TokenType,
    val operand: Expression
) : Expression

private data class BinaryExpression(
    val left: Expression,
    val operator: TokenType,
    val right: Expression
) : Expression

private data class FunctionExpression(
    val name: String,
    val arguments: List<Expression>
) : Expression

private class Parser(private val tokens: List<Token>) {
    private var position = 0

    fun parse(): Expression {
        val expression = parseAddition()

        if (current().type != TokenType.EOF) {
            throw CalculatorException(
                "Unexpected token '${current().text}'"
            )
        }

        return expression
    }

    private fun parseAddition(): Expression {
        var expression = parseMultiplication()

        while (
            current().type == TokenType.PLUS ||
            current().type == TokenType.MINUS
        ) {
            val operator = advance().type
            val right = parseMultiplication()

            expression = BinaryExpression(
                expression,
                operator,
                right
            )
        }

        return expression
    }

    private fun parseMultiplication(): Expression {
        var expression = parseUnary()

        while (
            current().type == TokenType.STAR ||
            current().type == TokenType.SLASH ||
            current().type == TokenType.PERCENT
        ) {
            val operator = advance().type
            val right = parseUnary()

            expression = BinaryExpression(
                expression,
                operator,
                right
            )
        }

        return expression
    }

    private fun parseUnary(): Expression {
        if (
            current().type == TokenType.PLUS ||
            current().type == TokenType.MINUS
        ) {
            val operator = advance().type

            return UnaryExpression(
                operator,
                parseUnary()
            )
        }

        return parsePower()
    }

    private fun parsePower(): Expression {
        val left = parsePrimary()

        if (match(TokenType.CARET)) {
            return BinaryExpression(
                left,
                TokenType.CARET,
                parseUnary()
            )
        }

        return left
    }

    private fun parsePrimary(): Expression {
        val token = current()

        when (token.type) {
            TokenType.NUMBER -> {
                advance()

                val value = token.text.toDoubleOrNull()
                    ?: throw CalculatorException(
                        "Invalid number '${token.text}'"
                    )

                return NumberExpression(value)
            }

            TokenType.IDENTIFIER -> {
                advance()

                if (match(TokenType.LPAREN)) {
                    val arguments = mutableListOf<Expression>()

                    if (current().type != TokenType.RPAREN) {
                        do {
                            arguments.add(parseAddition())
                        } while (match(TokenType.COMMA))
                    }

                    expect(TokenType.RPAREN)

                    return FunctionExpression(
                        token.text,
                        arguments
                    )
                }

                return VariableExpression(token.text)
            }

            TokenType.LPAREN -> {
                advance()
                val expression = parseAddition()
                expect(TokenType.RPAREN)
                return expression
            }

            else -> throw CalculatorException(
                "Expected a number, variable, or parenthesized expression"
            )
        }
    }

    private fun current(): Token = tokens[position]

    private fun advance(): Token = tokens[position++]

    private fun match(type: TokenType): Boolean {
        if (current().type != type) return false

        advance()
        return true
    }

    private fun expect(type: TokenType) {
        if (!match(type)) {
            throw CalculatorException(
                "Expected $type, found '${current().text}'"
            )
        }
    }
}

// ============================================================
// 4. EXPRESSION EVALUATOR
// ============================================================

private class Evaluator(
    private val variables: Map<String, Double>
) {
    // Higher-order functions and function references.
    private val functions: Map<String, (Double) -> Double> =
        mapOf(
            "sqrt" to ::sqrt,
            "abs" to ::abs,
            "sin" to ::sin,
            "cos" to ::cos,
            "tan" to ::tan,
            "log" to ::log10,
            "ln" to ::ln
        )

    fun evaluate(expression: Expression): Double {
        val result = when (expression) {
            is NumberExpression -> expression.value

            is VariableExpression -> {
                variables[expression.name]
                    ?: throw CalculatorException(
                        "Unknown variable '${expression.name}'"
                    )
            }

            is UnaryExpression -> {
                val value = evaluate(expression.operand)

                when (expression.operator) {
                    TokenType.PLUS -> value
                    TokenType.MINUS -> -value
                    else -> error("Invalid unary operator")
                }
            }

            is BinaryExpression -> {
                val left = evaluate(expression.left)
                val right = evaluate(expression.right)

                when (expression.operator) {
                    TokenType.PLUS -> left + right
                    TokenType.MINUS -> left - right
                    TokenType.STAR -> left * right

                    TokenType.SLASH -> {
                        if (right == 0.0) {
                            throw CalculatorException(
                                "Cannot divide by zero"
                            )
                        }
                        left / right
                    }

                    TokenType.PERCENT -> {
                        if (right == 0.0) {
                            throw CalculatorException(
                                "Cannot modulo by zero"
                            )
                        }
                        left % right
                    }

                    TokenType.CARET -> left.pow(right)

                    else -> error("Invalid binary operator")
                }
            }

            is FunctionExpression -> {
                val function = functions[expression.name]
                    ?: throw CalculatorException(
                        "Unknown function '${expression.name}'"
                    )

                if (expression.arguments.size != 1) {
                    throw CalculatorException(
                        "Function '${expression.name}' expects one argument"
                    )
                }

                function(evaluate(expression.arguments.single()))
            }
        }

        if (!result.isFinite()) {
            throw CalculatorException(
                "Result is not a finite real number"
            )
        }

        return result
    }
}

// ============================================================
// 5. DATA MODELS
// ============================================================

private data class HistoryEntry(
    val input: String,
    val result: String
)

// ============================================================
// 6. CALCULATOR AND CLI
// ============================================================

private class Calculator {
    private val variables = mutableMapOf(
        "pi" to PI,
        "e" to E
    )

    private val history = mutableListOf<HistoryEntry>()

    fun run() {
        println("Kotlin Calculator")
        println("Type 'help' for commands, or 'exit' to quit.")

        while (true) {
            print("> ")

            val input = readlnOrNull() ?: break
            val line = input.trim()

            if (line.isEmpty()) continue

            try {
                when (line.lowercase()) {
                    "exit", "quit" -> {
                        println("Goodbye.")
                        return
                    }

                    "help" -> showHelp()
                    "history" -> showHistory()
                    "clear" -> {
                        history.clear()
                        println("History cleared.")
                    }

                    "variables" -> showVariables()

                    "clearvars" -> {
                        variables.clear()
                        variables["pi"] = PI
                        variables["e"] = E
                        println("Variables cleared.")
                    }

                    "test" -> runTests()
                    "experiments" -> runExperiments()

                    else -> println(evaluateLine(line))
                }
            } catch (e: CalculatorException) {
                println("Error: ${e.message}")
            } catch (e: ArithmeticException) {
                println("Error: ${e.message ?: "Arithmetic error"}")
            } catch (e: IllegalArgumentException) {
                println("Error: ${e.message ?: "Invalid argument"}")
            }
        }
    }

    private fun evaluateLine(line: String): String {
        val assignment = Regex(
            "^([a-zA-Z_][a-zA-Z0-9_]*)\\s*=\\s*(.+)$"
        ).matchEntire(line)

        if (assignment != null) {
            val name = assignment.groupValues[1]
            val expressionText = assignment.groupValues[2]

            if (name == "pi" || name == "e") {
                throw CalculatorException(
                    "Cannot overwrite constant '$name'"
                )
            }

            val value = calculate(expressionText)
            variables[name] = value

            val result = format(value)
            history.add(HistoryEntry(line, result))

            return "$name = $result"
        }

        val value = calculate(line)
        val result = format(value)

        history.add(HistoryEntry(line, result))
        return result
    }

    private fun calculate(source: String): Double {
        val tokens = Lexer(source).tokenize()
        val expression = Parser(tokens).parse()

        return Evaluator(variables).evaluate(expression)
    }

    private fun showHistory() {
        if (history.isEmpty()) {
            println("No calculations yet.")
            return
        }

        history.forEachIndexed { index, entry ->
            println(
                "${index + 1}. ${entry.input} = ${entry.result}"
            )
        }
    }

    private fun showVariables() {
        if (variables.isEmpty()) {
            println("No variables.")
            return
        }

        variables.toSortedMap().forEach { (name, value) ->
            println("$name = ${format(value)}")
        }
    }

    private fun showHelp() {
        println(
            """
            Operations: +  -  *  /  %  ^
            Functions:  sqrt, abs, sin, cos, tan, log, ln
            Constants:  pi, e
            Assignment: x = 10

            Commands:
              help        Show this help
              history     Show calculation history
              clear       Clear calculation history
              variables   Show stored variables
              clearvars   Clear variables except constants
              test        Run automated tests
              experiments Run Kotlin concept experiments
              exit        Quit

            Notes:
              Trigonometric functions use radians.
              log(x) is base 10.
              ln(x) is the natural logarithm.
              ^ means exponentiation.
            """.trimIndent()
        )
    }

    // ========================================================
    // 7. KOTLIN CONCEPT EXPERIMENTS
    // ========================================================

    private fun runExperiments() {
        println("=== Kotlin Concept Experiments ===")

        // Type inference: Kotlin infers List<Int>.
        val numbers = listOf(1, 2, 3, 4, 5)

        // Higher-order function: map receives a lambda.
        val squares = numbers.map { it * it }

        println("map: $numbers -> $squares")

        // Immutable list vs mutable list.
        val immutableList = listOf(1, 2, 3)
        val mutableList = mutableListOf(1, 2, 3)

        mutableList.add(4)

        println("listOf: $immutableList")
        println("mutableListOf after add: $mutableList")

        // Recursion vs iteration.
        println("recursive factorial(5): ${factorialRecursive(5)}")
        println("iterative factorial(5): ${factorialIterative(5)}")

        // Closure: the lambda captures multiplier.
        val multiplier = 3
        val multiplyByThree: (Int) -> Int = {
            it * multiplier
        }

        println(
            "closure: multiplyByThree(7) = ${multiplyByThree(7)}"
        )

        println()
        println("Further experiments to implement:")
        println("- Compare sealed interfaces with abstract classes.")
        println("- Compare exceptions with explicit result types.")
        println("- Compare recursive and iterative AST evaluation.")
        println("- Benchmark parsing separately from evaluation.")
        println("- Profile JVM allocation and garbage collection.")
        println("- Compare mutable and immutable variable environments.")
    }

    private fun factorialRecursive(n: Int): Long {
        require(n >= 0)

        return if (n <= 1) {
            1L
        } else {
            n * factorialRecursive(n - 1)
        }
    }

    private fun factorialIterative(n: Int): Long {
        require(n >= 0)

        var result = 1L

        for (i in 2..n) {
            result *= i.toLong()
        }

        return result
    }

    // ========================================================
    // 8. AUTOMATED TESTS
    // ========================================================

    private fun runTests() {
        data class TestCase(
            val input: String,
            val expected: Double
        )

        val cases = listOf(
            TestCase("2 + 3", 5.0),
            TestCase("10 - 4", 6.0),
            TestCase("5 * 6", 30.0),
            TestCase("20 / 4", 5.0),
            TestCase("10 % 3", 1.0),
            TestCase("2 ^ 8", 256.0),
            TestCase("2 + 3 * 4", 14.0),
            TestCase("(2 + 3) * 4", 20.0),
            TestCase("2 ^ 3 ^ 2", 512.0),
            TestCase("-2^2", -4.0),
            TestCase("-5 * 2", -10.0),
            TestCase("sqrt(25)", 5.0),
            TestCase("abs(-10)", 10.0),
            TestCase("sin(pi / 2)", 1.0)
        )

        var passed = 0

        for (test in cases) {
            try {
                val tokens = Lexer(test.input).tokenize()
                val ast = Parser(tokens).parse()

                val actual = Evaluator(
                    mapOf("pi" to PI, "e" to E)
                ).evaluate(ast)

                if (abs(actual - test.expected) < 1e-9) {
                    passed++
                    println("PASS: ${test.input}")
                } else {
                    println(
                        "FAIL: ${test.input}, expected " +
                            "${test.expected}, got $actual"
                    )
                }
            } catch (e: Exception) {
                println(
                    "FAIL: ${test.input}, ${e.message}"
                )
            }
        }

        println("$passed/${cases.size} tests passed.")
    }

    // ========================================================
    // 9. NUMBER FORMATTING
    // ========================================================

    private fun format(value: Double): String {
        if (value == 0.0) return "0"

        return BigDecimal.valueOf(value)
            .stripTrailingZeros()
            .toPlainString()
    }
}

// ============================================================
// 10. PROGRAM ENTRY POINT
// ============================================================

fun main() {
    Calculator().run()
}
