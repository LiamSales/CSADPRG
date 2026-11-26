import kotlin.math.*
import java.math.BigDecimal

//parsing power and unary and binary i dont get

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
                    tokens.add(readNumber()) //one token, thats the entire number, basically calls this, moves control (well it never leaves) of our position value, duplicates the cursor (like a for loop)
//returns the whole thing as a single token (type included) 

                c.isLetter() || c == '_' ->
                    tokens.add(readIdentifier())

                else -> { //we dont do equals then get the value of the variable
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

                    tokens.add(Token(type, c.toString())) // this is only one character then to string
                    position++
                }
            }
        }

        tokens.add(Token(TokenType.EOF, "")) //manually add the EOF and empty string as the last part of our list
        return tokens
    }

    private fun readNumber(): Token { // no parameter
        val start = position //position is still in the scope of the class, local start
        var decimalSeen = false //only one decimal

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

        val text = source.substring(start, position) //local start until where we ended,

        if (text == ".") {
            throw CalculatorException("Invalid number '.'")
        } //except if its only the decimal

        return Token(TokenType.NUMBER, text)
    }

    private fun readIdentifier(): Token {
        val start = position

        while (
            position < source.length &&
            (
                source[position].isLetterOrDigit() || //not defined function???
                source[position] == '_'
            )
        ) {
            position++
        }

        return Token(
            TokenType.IDENTIFIER,
            source.substring(start, position) //as long as it doesnt start with the number, it could be a letter, string, underscore, just nothing that wouldnt be a function name
        )
    }
}


private sealed interface Expression //shared type, different dataclasses have a common type

private data class NumberExpression(
    val value: Double
) : Expression

private data class VariableExpression(
    val name: String
) : Expression

private data class UnaryExpression(
    val operator: TokenType,
    val operand: Expression
) : Expression // sign + number

private data class BinaryExpression(
    val left: Expression,
    val operator: TokenType,
    val right: Expression
) : Expression // number, arithmetic, number

private data class FunctionExpression(
    val name: String,
    val arguments: List<Expression>
) : Expression // sqrt, et al

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

// all just definitions but no initiations

private class Parser(private val tokens: List<Token>) { //but main passes the parameter (which is defined locally in main, not globally
    
    private var position = 0 //new beginning were done with the lexer

    fun parse(): Expression { //notice how parsers  have no parameters and argyments, they utilize the class parameter instead. One object instantiation, one equation.
        
        val expression = parseAddition() //so everything passes through all steps, because everything gets parse()

        //all the nested expressions (object holding object, are stored here

        if (current().type != TokenType.EOF) {
            throw CalculatorException(
                "Unexpected token '${current().text}'" //If parsing finishes but there are still unexpected tokens, the parser throws an exception
            )
        }

        return expression //then we return the fully realized parsed expression at the end (this i dont know how it looks like)
    }

    private fun parseAddition(): Expression { //recursive-descent parsing, breaks down into identifying smaller tasks, stalgcm type
        
        var expression = parseMultiplication() //so it gets the first number, while token (class var) is directed to the next 

        //token now at next (thanks to the advance in the following functions)
        //whats important is that expression is saved here specifically in addition
        while ( 
            current().type == TokenType.PLUS ||
            current().type == TokenType.MINUS
        ) { //while checks the next
            val operator = advance().type //operator saves the plus or minus, goes to the next
            val right = parseMultiplication() //then we do the same things (EXCEPT ONLY AFTER THE OPERATOR, EXPRESSION HAS BEEN SAVED ALREADY

            //so what if right has an addition inside it? wouldnt it skip?
            //no because at the end current will recheck again if plus or minus

            //so expression is settled, then operator, then right, but wouldnt right just be overriden everytime??
            //right does not override because expression saves the current version bellow
            
            expression = BinaryExpression(
                expression,
                operator,
                right
            ) //fully save a binex and return to parse(), however what if its more than one operator??? this doesnt call itself for addition rechecking???
        }// because its in a while loop so current, then itll advance, then the end of "right" just advances again then we repeat the loop

        //because the expression is saved in a recursive typa thing, we get left associativity
        return expression //final product will look like (((2 + 3) + 4) + 5)
        //left version is declared once out of the loop, then within the loop it gets refreshed as one expression including right
    }
    //we can bypass the binary expression part dont forget, then just simply end with a unary

    private fun parseMultiplication(): Expression { //we simply follow the JVM compiler precedence,now it CAN work without this but takes more work and we add a leading 0 
        
        var expression = parseUnary()

        while (
            current().type == TokenType.STAR ||
            current().type == TokenType.SLASH ||
            current().type == TokenType.PERCENT
        ) { //on the primary returns and were at these opeators, we dont go back to addition, completely forget about addition, so while */% keeps going
            val operator = advance().type
            val right = parseUnary()  //were here because we go set a checkpoint

            expression = BinaryExpression(
                expression, 
                operator,
                right //expressions are NOT strings, nether are operators
            )
        }

        return expression //so the expression we give addition will be a ((x*y) *z) ... one single expression, then this will be nested
    }// remember expressions are objects HOLDING double (or string). they have an attribute called value which we get!

    private fun parseUnary(): Expression {
        
        //so we actually do this first, we dont call the parse power yet
        
        if ( //not while, just an if
            current().type == TokenType.PLUS ||
            current().type == TokenType.MINUS
        ) {
            val operator = advance().type //new var, operator is equal to the sign, but why do we advance rather than current? so KOTLIN gets type before evaulating exp
//POSTFIX USE VARIABLE CURRENT VALUE IN EXPRESSION AND THEN ++ BECAUSE ++ IS AFTER, so ok we save the operator
            
            return UnaryExpression( //our first actual return, it will only go back to itself and recurse if +/- and get an embedded unary
                operator, // remember +-+- is 4 different tokens and we only advance later during parse primary
                parseUnary()// return then recurse, basically just go back but current is in a new postion because its not declared in this functions scope
            )
        } //does it just skip this? yes. because if it has, anyway itll call itself again with the same thing

        return parsePower() //but then nothing more after this for unary, its only job was to get if the first ever char is a +/-, then it just proceeds to power
        //so all roads lead it to parsing power.
    }

    private fun parsePower(): Expression {
        
        val left = parsePrimary() //but left here is just another name for expression, we know its not a caret
        //so we dont check carets yet, when we finish this we do the caret

        //so primary parsing is mandatory before caret

        //fix everything before caret

        if (match(TokenType.CARET)) { // we already have types of expressions defined up there as data classes, and additions base case gives us a binex
            return BinaryExpression( //left operator right
                left,
                TokenType.CARET,
                parseUnary() // can cycle back here
            ) //yes exponent (operand, operator, operand)
        }

        return left //this will only be reached if no caret (basically an else), final return
    }

    //but how do we know only parentheses and numbers get passed to this???
    
    private fun parsePrimary(): Expression { //where the real parsing happens?? recognizes the basic building blocks of expressions.
        
        val token = current()

        when (token.type) {
            TokenType.NUMBER -> { //remember everything is already tokenized, so its all sorted and handled by this point. we just need to arrange stuff
                
                advance() //Because the parser has finished recognizing the current token as a number. It needs to move past that token so the next parser operation can handle whatever comes afterward.
                //ok this advance is not for us anymore, advance moves the bigger scope (class) token list, so that we can parse whats the next list

                //this whole class basically just converts to usable types??
                val value = token.text.toDoubleOrNull() //important part, converts strings into doubles
                    ?: throw CalculatorException(
                        "Invalid number '${token.text}'"
                    )

                return NumberExpression(value) //this gets passed back to the other parsers
            } //so when its a number itll go back to previous functions until the next token is consumed (* gets caught earlier than +)


            //ok i get passing number expressions now
            
            TokenType.IDENTIFIER -> { 
                
                advance() //token is still saved 

                if (match(TokenType.LPAREN)) { //because string() is a function but string + is a variable, so we see whats beside the string
                    // parenthesis multiplication doesnt work since all operators must be explicit
                    
                    val arguments = mutableListOf<Expression>()

                    if (current().type != TokenType.RPAREN) {
                        do { //
                            arguments.add(parseAddition()) //inside the parenthesis we have parameters and commas (like a real function)
                        } while (match(TokenType.COMMA)) //The parser needs a flexible collection to store the expressions passed into the function.
                    } //It stores AST nodes, not raw tokens or numbers
                    //it stores EXPRESSIONS

                    expect(TokenType.RPAREN) //checks whether current tokem is this, and if not then it errors

                    return FunctionExpression(
                        token.text, //we still saved the old token before advancing
                        arguments //then a map
                    ) //this is if theres a parenthesis then it all ends here
                }

                return VariableExpression(token.text) //but if we have a string, and no parenthesis next to it then its a variable, so we just return string
            }

            //so we basically did both variable and funciton call, as well as numbers

            TokenType.LPAREN -> { //figure out how it isolates and prioritizes parenthesis
                advance() //but if the very first thing it sees is this parenthesis
                val expression = parseAddition() //we parse and wait for an end
                //everything is obv self contained and isolated here
                expect(TokenType.RPAREN)
                
                return expression //so when it returns an expression the pointer to this obj is everything in the parenthesis
            } //imagine the tree going back up

            else -> throw CalculatorException(
                "Expected a number, variable, or parenthesized expression"
            )
        }
    }
} 


private class Evaluator(
    private val variables: Map<String, Double> //we assume all the variables are already sorted to key value pairs, and i assume we sequentially get each variable by passing through the tokenized array
) {
    //NEW CONCCEPT... FUNCTION TYPE!!
    private val functions: Map<String, (Double) -> Double> = //string and function (function AS AN ARGUMENT!) gets double, RETURNS DOUBLE???
        mapOf(
            "sqrt" to ::sqrt, //now its a map of string, and then the double->double
            "abs" to ::abs, //:: is the callable reference operator.
            "sin" to ::sin, //we reference sin() without activating it, so it just exists as a pointer
            "cos" to ::cos,
            "tan" to ::tan,
            "log" to ::log10,
            "ln" to ::ln
        ) // therefore, all the strings are mapped to the pre-defined ::func() operators (called as variables), now thats a new thing!
        //so for example we do squareroot to ::sqrt, then squareroot(25.0) becomes 5.0? then if we map it to a void function it returns null?
        //minor correction, yes then no it doesnt return null it gives us Unit, which i should do independent research on
        
        //recap, everything has been tokenized, we have exactly one lexer class, with a tokens return value in its main function
        // so ok everything passes through that one function EZ

        //the tokens map gets passed to parser with specifically parse() giving a return value of an AST tree, 
    // now we forget the tokens map, then assuming everything defaults back to addition ... expression operation expression... thats the law
        
    fun evaluate(expression: Expression): Double { //main solving thing????, remember the first parameter is the deepest expression so
        
        val result = when (expression) { //when clause is a FUNCTION AS A VARIABLE 
            is NumberExpression -> expression.value //because expression is an object, we very well could have just used the double and skipped the object but then its harder
            //easier to code with INTERFACE!!
            //RESULT BECOMES EXPRESSION.VALUE

            is VariableExpression -> { //evaluate is our only necessary function so why werent variables declared in here? i assume we will call evaluate more than once, thats a turning point
                variables[expression.name] //assume variables is already properly declared and string double, not double? so all have names and values
                    ?: throw CalculatorException(
                        "Unknown variable '${expression.name}'"
                    )
            } //when expression is variable (and were sure of that) 
            //IF THE VARIABLE IS NON NULL THEN WE GET RESULT = NAME; WHEN ITS NULL, IT BECOMES EQUAL TO THE THROW CALCULATOR EXPRESSION (SO WE CAN CALL RESULT AS A EXCEPTION???)
            //IS THE DATA TYPE OF RESULT UP IN THE AIR? KOTLIN IS A COMPILED AND NOT INTERPRETTED LANGUAGE

            is UnaryExpression -> {
                val value = evaluate(expression.operand) //on the rare case that - n then higher precedence */% HOWEVER no succeeding - or + after the number token
                //recursion unlike the one on top, new strat figure out the play, so we call the functoin again if its a unary, and we evaluate only the number (it calls number expression)
                //THEN afterwards we handle the operator?? basically value gets the number as usual EZPZ
                when (expression.operator) {
                    TokenType.PLUS -> value// value is inherently a double? the operator is a token, that was its datatype, do we automatically negate the value?, this basically skips (Optional???)
                    TokenType.MINUS -> -value //this is the only necessary
                    else -> error("Invalid unary operator")
                }
            } //so this changes the value because the next one also does, basically the next case is super self explanatory 

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

                    TokenType.CARET -> left.pow(right) //pre-defined function

                    else -> error("Invalid binary operator")
                }
            }

            is FunctionExpression -> { //remember we mapped string to double(double) functions, so the choices are already defined in the map atp
                val function = functions[expression.name] //val function is equal to the value (double->double) of functions["sqrt"], name being the key,
                // function is equal to the call but not the result??? (idk?) yes based on the later line
                    ?: throw CalculatorException(
                        "Unknown function '${expression.name}'"
                    ) //if null, (does not appear in our key value map list, then nvm exception time

                if (expression.arguments.size != 1) { //why do we do only 1 if our parser allows a list ; because of the limitations of our evaluator (LOCALLY), however.... we can add a case with and define it here
                    throw CalculatorException(
                        "Function '${expression.name}' expects one argument"
                    )//This is an example of separating syntax recognition from semantic validation.
                }

                function(evaluate(expression.arguments.single())) //.single() retrieves the only element in our argument list, why not use the regular retrieve[0]
                //generally safer, if a more sophisticated function is available USE IT.
                //recursive case so we basically go sin(evaluate (tan (x))
            }
        } //when clause ends here so result = function return

        if (!result.isFinite()) { //how does this work behind the scenes???
            throw CalculatorException( //Math.abs(number) <= Double.MAX_VALUE // max_value is identical in every device
                //for double its 1.7976931348623157E308; IEEE standard (universal for all PL)
                //Certain math operations naturally return infinity instead of crashing, 0/0 is infinity, values over the max val are
                //however max val incremented is max val
                "Result is not a finite real number"
            )
        }

        return result // finally real calculator, DONE
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
