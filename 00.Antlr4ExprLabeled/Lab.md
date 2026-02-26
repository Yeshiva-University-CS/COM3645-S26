# Assignment --- Extend ExprLabeled with Unary Minus and Power (`^`)

You are given the following ANTLR4 expression language (assignments +
expression printing). Your task is to extend the grammar and interpreter
to support:

1.  **Unary minus** (negation): `-x`, `-(1+2)`, `--5`
2.  **Exponentiation** with `^`: `2^3`, `a^b`, `2^3^2`

------------------------------------------------------------------------

## Language Behavior Requirements

### 1) Operator precedence (highest → lowest)

1.  Parentheses: `( ... )`
2.  Exponentiation: `^`
3.  Unary minus: `-expr`
4.  Multiplication / Division: `*`, `/`
5.  Addition / Subtraction: `+`, `-`

This precedence implies, for example:

-   `-2^2` must parse as `-(2^2)` and evaluate to `-4`
-   `(-2)^2` must evaluate to `4`

### 2) Associativity

-   `^` is **right-associative**
    -   `2^3^2` means `2^(3^2)` = `2^9` = `512`
-   `*`, `/`, `+`, `-` (binary) are **left-associative**
    -   `10-3-2` means `(10-3)-2` = `5`


### 3) Numeric type

Use **integers only**.

-   Division should follow your existing integer behavior.
-   Exponentiation should return an integer.
-   You may reject negative exponents or define behavior; test files use
    non-negative exponents.

------------------------------------------------------------------------

# Implementation Steps

## Step 1 --- Extend the Grammar and Interpreter

Modify your `expr` rule to include:

-   Unary minus
-   Power operator `^` with right-associativity
-   Correct precedence relative to existing operators

You may implement precedence using either:

-   Multi-level grammar rules, OR
-   ANTLR4 direct-left-recursion with `<assoc=right>` for `^`

Your solution must parse and evaluate the provided test files correctly.

### Interpreter / Visitor Changes (Required)

In your visitor (or listener-based evaluator), implement:

1.  Unary minus
    -   `-x` evaluates to the negation of `x`
2.  Power
    -   `a ^ b` evaluates to `a` raised to the `b` power
    -   Must be right-associative based on your parse tree

Your existing handling of:

-   integers
-   identifiers
-   parentheses
-   `+ - * /`
-   assignments and printing

must continue to work.

------------------------------------------------------------------------

## Step 2 --- Refactor to Use Explicit Operator Tokens

After your solution works, refactor your grammar to define explicit
operator tokens:

POW : '\^' ; MUL : '\*' ; DIV : '/' ; ADD : '+' ; SUB : '-' ;

Then update your parser rule to use these token names instead of string
literals. For example:

-   Use `expr op=(MUL|DIV) expr` instead of `('*'|'/')`
-   Use `expr op=(ADD|SUB) expr` instead of `('+'|'-')`
-   Use `SUB expr` for unary minus
-   Use `expr POW expr` for exponentiation

Finally, update your visitor to use:

    ctx.op.getType()

and compare against:

    ExprLabeledParser.MUL
    ExprLabeledParser.DIV
    ExprLabeledParser.ADD
    ExprLabeledParser.SUB
    ExprLabeledParser.POW

This makes your interpreter cleaner and avoids string comparisons.

------------------------------------------------------------------------



# Test files Expected Output (Correct)

## unary.txt
```
-5 
5 
-7 
-15
```
## power.txt
```
8 
4 
512 
27
```
## unary_power.txt
```
-4 
4 
-4 
8 
-12
```