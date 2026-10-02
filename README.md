# CSADPRG
Advanced Programming Techniques
 
Calculator Language Study

Build the same CLI calculator in Go, R, Ruby, and Kotlin.

The goal is to use one project to practice programming while directly experimenting with language features and computer science concepts inside the code.

Calculator Features

Implement:

- "+", "-", "*", "/", "%", "^"
- Integers, floats, and negative numbers
- Parentheses and operator precedence
- Variables and constants
- Scientific functions
- Calculation history
- Interactive CLI
- Error handling
- Automated tests
- Tokenizer
- Parser
- Abstract Syntax Tree (AST)
- Expression evaluation

Learning Requirements

Do not simply translate the same implementation between languages.

For each language, use the project to experiment with how the language works.

The code should contain experiments, TODOs, comments, tests, and alternative implementations for concepts such as:

- Static vs dynamic typing
- Type inference
- Type conversion
- Value vs reference semantics
- Mutability and immutability
- Functions and closures
- Higher-order functions
- OOP
- Interfaces / protocols
- Generics
- Error handling
- Collections and data structures
- Memory allocation
- Garbage collection
- Recursion
- Concurrency
- Parallelism
- Performance
- Compilation and runtime behavior

Parser

Build the calculator progressively:

Input
  ↓
Tokenizer
  ↓
Tokens
  ↓
Parser
  ↓
AST
  ↓
Evaluator
  ↓
Result

Use the parser to experiment with concepts such as:

- Recursive descent
- Recursion
- Operator precedence
- Tree data structures
- Pattern matching / type dispatch
- Object-oriented vs functional designs

Comparative Experiments

Where meaningful, implement the same feature in multiple ways.

For example:

TODO: Implement expression evaluation using
      1. OOP
      2. Functional style
      3. A language-specific idiomatic approach

Then use tests or small benchmark programs to observe the differences.

Other experiments can include:

TODO: Compare mutable vs immutable state

TODO: Compare exception-based vs value-based error handling

TODO: Compare recursive vs iterative evaluation

TODO: Compare different collection types

TODO: Test reference/value behavior

TODO: Measure memory usage where possible

TODO: Benchmark large numbers of expressions

TODO: Experiment with concurrency where supported

Important Rule

The code is the study.

Do not create a separate theoretical report. Learn the concepts by implementing them, breaking them, testing them, and comparing the behavior of Go, R, Ruby, and Kotlin directly in the projects.

Final Goal

End up with four calculators that perform the same job, while the code itself demonstrates how different programming languages approach the same underlying problems.