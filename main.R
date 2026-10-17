
# R Calculator Language Study
# Concepts: lists, environments, closures, lexical scoping, vectorization,
# recursive functions, condition handling, and AST interpretation.

# ------------------------------------------------------------
# 1. Errors and lexer
# ------------------------------------------------------------

stop_calc <- function(message) {
  stop(structure(
    list(message = message, call = NULL),
    class = c("calculator_error", "error", "condition")
  ))
}

token <- function(kind, text) {
  list(kind = kind, text = text)
}

tokenize <- function(source) {
  chars <- strsplit(source, "", fixed = TRUE)[[1]]
  n <- length(chars)
  pos <- 1L
  tokens <- list()

  while (pos <= n) {
    ch <- chars[[pos]]

    if (grepl("^[[:space:]]$", ch)) {
      pos <- pos + 1L
      next
    }

    if (grepl("^[0-9.]$", ch)) {
      start <- pos
      dot_seen <- FALSE

      while (pos <= n && grepl("^[0-9.]$", chars[[pos]])) {
        if (chars[[pos]] == ".") {
          if (dot_seen) stop_calc("invalid number: multiple decimal points")
          dot_seen <- TRUE
        }
        pos <- pos + 1L
      }

      text <- paste(chars[start:(pos - 1L)], collapse = "")
      if (text == ".") stop_calc("invalid number '.'")
      if (is.na(suppressWarnings(as.numeric(text)))) {
        stop_calc(paste0("invalid number '", text, "'"))
      }
      tokens[[length(tokens) + 1L]] <- token("NUMBER", text)
      next
    }

    if (grepl("^[A-Za-z_]$", ch)) {
      start <- pos
      while (pos <= n && grepl("^[A-Za-z0-9_]$", chars[[pos]])) {
        pos <- pos + 1L
      }
      text <- paste(chars[start:(pos - 1L)], collapse = "")
      tokens[[length(tokens) + 1L]] <- token("IDENT", text)
      next
    }

    kinds <- c(
      "+" = "PLUS", "-" = "MINUS", "*" = "STAR", "/" = "SLASH",
      "%" = "PERCENT", "^" = "CARET", "(" = "LPAREN",
      ")" = "RPAREN", "," = "COMMA"
    )
    kind <- unname(kinds[ch])

    if (length(kind) == 0L || is.na(kind)) {
      stop_calc(paste0("unexpected character '", ch, "'"))
    }

    tokens[[length(tokens) + 1L]] <- token(kind, ch)
    pos <- pos + 1L
  }

  tokens[[length(tokens) + 1L]] <- token("EOF", "")
  tokens
}

# ------------------------------------------------------------
# 2. AST constructors
# R lists represent nodes in the abstract syntax tree.
# ------------------------------------------------------------

number_expr <- function(value) list(type = "number", value = value)
variable_expr <- function(name) list(type = "variable", name = name)
unary_expr <- function(op, operand) list(type = "unary", op = op, operand = operand)
binary_expr <- function(left, op, right) list(type = "binary", left = left, op = op, right = right)
call_expr <- function(name, args) list(type = "call", name = name, args = args)

# ------------------------------------------------------------
# 3. Recursive-descent parser
# ------------------------------------------------------------

new_parser <- function(tokens) {
  state <- new.env(parent = emptyenv())
  state$tokens <- tokens
  state$pos <- 1L

  current <- function() state$tokens[[state$pos]]
  advance <- function() {
    token <- current()
    state$pos <- state$pos + 1L
    token
  }
  match_token <- function(kind) {
    if (current()$kind != kind) return(FALSE)
    advance()
    TRUE
  }
  expect <- function(kind) {
    if (!match_token(kind)) {
      stop_calc(paste0("expected ", kind, ", found '", current()$text, "'"))
    }
  }

  parse_addition <- NULL
  parse_multiplication <- NULL
  parse_unary <- NULL
  parse_power <- NULL
  parse_primary <- NULL

  parse_addition <- function() {
    expr <- parse_multiplication()
    while (current()$kind %in% c("PLUS", "MINUS")) {
      op <- advance()$text
      expr <- binary_expr(expr, op, parse_multiplication())
    }
    expr
  }

  parse_multiplication <- function() {
    expr <- parse_unary()
    while (current()$kind %in% c("STAR", "SLASH", "PERCENT")) {
      op <- advance()$text
      expr <- binary_expr(expr, op, parse_unary())
    }
    expr
  }

  parse_unary <- function() {
    if (current()$kind %in% c("PLUS", "MINUS")) {
      op <- advance()$text
      return(unary_expr(op, parse_unary()))
    }
    parse_power()
  }

  # Right-associative exponentiation; unary signs bind less tightly.
  parse_power <- function() {
    left <- parse_primary()
    if (match_token("CARET")) {
      return(binary_expr(left, "^", parse_unary()))
    }
    left
  }

  parse_primary <- function() {
    token <- current()
    if (token$kind == "NUMBER") {
      advance()
      return(number_expr(as.numeric(token$text)))
    }
    if (token$kind == "IDENT") {
      advance()
      if (match_token("LPAREN")) {
        args <- list()
        if (current()$kind != "RPAREN") {
          repeat {
            args[[length(args) + 1L]] <- parse_addition()
            if (!match_token("COMMA")) break
          }
        }
        expect("RPAREN")
        return(call_expr(token$text, args))
      }
      return(variable_expr(token$text))
    }
    if (token$kind == "LPAREN") {
      advance()
      expr <- parse_addition()
      expect("RPAREN")
      return(expr)
    }
    stop_calc("expected a number, variable, or parenthesized expression")
  }

  parse <- function() {
    expr <- parse_addition()
    if (current()$kind != "EOF") {
      stop_calc(paste0("unexpected token '", current()$text, "'"))
    }
    expr
  }

  list(parse = parse)
}

# ------------------------------------------------------------
# 4. Evaluator
# Environments provide mutable variable storage with parent lookup disabled.
# ------------------------------------------------------------

evaluate_ast <- function(expr, vars) {
  functions <- list(
    sqrt = sqrt, abs = abs, sin = sin, cos = cos, tan = tan,
    log = log10, ln = log
  )

  result <- switch(expr$type,
    number = expr$value,
    variable = {
      if (!exists(expr$name, envir = vars, inherits = FALSE)) {
        stop_calc(paste0("unknown variable '", expr$name, "'"))
      }
      get(expr$name, envir = vars, inherits = FALSE)
    },
    unary = {
      value <- evaluate_ast(expr$operand, vars)
      if (expr$op == "-") -value else value
    },
    binary = {
      left <- evaluate_ast(expr$left, vars)
      right <- evaluate_ast(expr$right, vars)
      if (expr$op == "/" && right == 0) stop_calc("cannot divide by zero")
      if (expr$op == "%" && right == 0) stop_calc("cannot modulo by zero")
      switch(expr$op,
        "+" = left + right,
        "-" = left - right,
        "*" = left * right,
        "/" = left / right,
        "%" = left %% right,
        "^" = left ^ right
      )
    },
    call = {
      fn <- functions[[expr$name]]
      if (is.null(fn)) stop_calc(paste0("unknown function '", expr$name, "'"))
      if (length(expr$args) != 1L) {
        stop_calc(paste0("function '", expr$name, "' expects one argument"))
      }
      fn(evaluate_ast(expr$args[[1]], vars))
    },
    stop_calc("unsupported expression type")
  )

  if (length(result) != 1L || !is.finite(result)) {
    stop_calc("result is not a finite real number")
  }
  result
}

# ------------------------------------------------------------
# 5. Calculator state and CLI
# ------------------------------------------------------------

format_number <- function(value) {
  if (value == 0) return("0")
  format(value, scientific = FALSE, trim = TRUE, digits = 15)
}

new_calculator <- function() {
  state <- new.env(parent = emptyenv())
  state$vars <- new.env(parent = emptyenv())
  assign("pi", pi, envir = state$vars)
  assign("e", exp(1), envir = state$vars)
  state$history <- list()

  calculate <- function(source) {
    tokens <- tokenize(source)
    expr <- new_parser(tokens)$parse()
    evaluate_ast(expr, state$vars)
  }

  evaluate_line <- function(line) {
    assignment <- regexec("^([A-Za-z_][A-Za-z0-9_]*)[[:space:]]*=[[:space:]]*(.+)$", line)
    match <- regmatches(line, assignment)[[1]]
    if (length(match) > 0L) {
      name <- match[[2]]
      source <- match[[3]]
      if (name %in% c("pi", "e")) stop_calc(paste0("cannot overwrite constant '", name, "'"))
      value <- calculate(source)
      assign(name, value, envir = state$vars)
      result <- format_number(value)
      state$history[[length(state$history) + 1L]] <- list(input = line, result = result)
      return(paste0(name, " = ", result))
    }

    value <- calculate(line)
    result <- format_number(value)
    state$history[[length(state$history) + 1L]] <- list(input = line, result = result)
    result
  }

  show_history <- function() {
    if (length(state$history) == 0L) {
      cat("No calculations yet.\n")
      return(invisible(NULL))
    }
    for (i in seq_along(state$history)) {
      entry <- state$history[[i]]
      cat(i, ". ", entry$input, " = ", entry$result, "\n", sep = "")
    }
  }

  show_variables <- function() {
    names <- sort(ls(envir = state$vars, all.names = TRUE))
    if (length(names) == 0L) {
      cat("No variables.\n")
      return(invisible(NULL))
    }
    for (name in names) {
      cat(name, " = ", format_number(get(name, envir = state$vars)), "\n", sep = "")
    }
  }

  run_experiments <- function() {
    cat("=== R Concept Experiments ===\n")
    numbers <- c(1, 2, 3, 4, 5)
    cat("vectorized squares:", numbers ^ 2, "\n")
    cat("lapply squares:", unlist(lapply(as.list(numbers), function(x) x ^ 2)), "\n")

    multiplier <- 3
    multiply_by_three <- function(x) x * multiplier
    cat("closure multiply_by_three(7):", multiply_by_three(7), "\n")

    # R uses copy-on-modify semantics in many ordinary cases.
    original <- c(1, 2, 3)
    changed <- original
    changed[1] <- 99
    cat("original after changing copy:", original, "\n")
    cat("changed copy:", changed, "\n")
    cat("recursive factorial(5):", factorial_recursive(5), "\n")
    cat("TODO: compare environments, copy-on-modify, vectorization, and parallel packages.\n")
  }

  factorial_recursive <- function(n) {
    if (n <= 1) return(1)
    n * factorial_recursive(n - 1)
  }

  run_tests <- function() {
    cases <- list(
      list("2 + 3", 5), list("10 - 4", 6), list("5 * 6", 30),
      list("20 / 4", 5), list("10 % 3", 1), list("2 ^ 8", 256),
      list("2 + 3 * 4", 14), list("(2 + 3) * 4", 20),
      list("2 ^ 3 ^ 2", 512), list("-2^2", -4),
      list("-5 * 2", -10), list("sqrt(25)", 5),
      list("abs(-10)", 10), list("sin(pi / 2)", 1)
    )
    passed <- 0L
    for (test in cases) {
      input <- test[[1]]
      expected <- test[[2]]
      tryCatch({
        actual <- calculate(input)
        if (abs(actual - expected) < 1e-9) {
          passed <<- passed + 1L
          cat("PASS:", input, "\n")
        } else {
          cat("FAIL:", input, "expected", expected, "got", actual, "\n")
        }
      }, error = function(e) cat("FAIL:", input, conditionMessage(e), "\n"))
    }
    cat(passed, "/", length(cases), " tests passed.\n", sep = "")
  }

  help <- function() {
    cat("Operations: + - * / % ^\n")
    cat("Functions: sqrt, abs, sin, cos, tan, log, ln\n")
    cat("Constants: pi, e; assignment: x = 10\n")
    cat("Commands: help, history, clear, variables, clearvars, test, experiments, exit\n")
    cat("Trigonometric functions use radians; log is base 10; ln is natural logarithm.\n")
  }

  run <- function() {
    cat("R Calculator. Type 'help' for commands, 'exit' to quit.\n")
    repeat {
      line <- readline("> ")
      if (line == "") next
      command <- tolower(trimws(line))
      tryCatch({
        if (command %in% c("exit", "quit")) {
          cat("Goodbye.\n")
          break
        } else if (command == "help") {
          help()
        } else if (command == "history") {
          show_history()
        } else if (command == "clear") {
          state$history <- list()
          cat("History cleared.\n")
        } else if (command == "variables") {
          show_variables()
        } else if (command == "clearvars") {
          rm(list = setdiff(ls(envir = state$vars), c("pi", "e")), envir = state$vars)
          cat("Variables cleared.\n")
        } else if (command == "test") {
          run_tests()
        } else if (command == "experiments") {
          run_experiments()
        } else {
          cat(evaluate_line(trimws(line)), "\n")
        }
      }, error = function(e) {
        cat("Error:", conditionMessage(e), "\n")
      })
    }
  }

  list(run = run)
}

new_calculator()$run()
