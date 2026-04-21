# COM-3645 Compilers and Tools, Spring 2026

## Compiler Project: Aiden

**Total: 100 points**

| Component                                    | Points |
| -------------------------------------------- | ------ |
| Semantic Analysis                            | 20     |
| Executor                                     | 25     |
| Converter                                    | 25     |
| Optimizations, Language Features, Constructs | 30     |

---

## A. Semantic Analysis (20 points)

Implement the second pass of the compiler in which you perform the semantic analysis of the program and populate the symbol table. Implement type checking and other semantic checks for the Aiden language, as you did for the Pascal language.

### Provided Files

* The Aiden ANTLR grammar (`Aiden.g4`)
* A semantic analyzer stub (`Semantics.java`)
* A semantic error handler (`SemanticErrorHandler.java`)

Use the `TypeChecker.java` utility class for type checking.

You should write unit tests to validate correct and incorrect programs. Your implementation must pass:

* `SemanticSuccessTest.java`
* `SemanticErrorTest.java`

Refer to the *Aiden Language Summary* for full language specifications.

---

### Semantic Rules to Enforce

#### Valid Declarations and Scoping

* Each identifier (let/variable/function/parameter) must be declared once per scope
* Identifiers must be declared before use
* Shadowing is allowed in nested blocks
* Function parameters must be uniquely named

#### Type Checking

* Assignment: Types must be compatible (e.g., no `int = "hello"`)
* Arithmetic: Only numeric types for `+`, `-`, `*`, `/`
* Logical: Only boolean operands for `and`, `or`, `!`
* Comparison: Only compatible types (`int < int`, `string < string`)
* Equality: Only compatible types (`int == int`, `string != string`)
* String concatenation: Only `string + string` is allowed
* If condition must be boolean
* Return consistency: All return statements in a function must match the inferred return type
* Any type may hold the value `none`; assigning `none` to any variable is always valid

#### Function Semantics

* Cannot redeclare a function
* Must pass the correct number of arguments in function calls
* Only functions can be called
* Return statements outside a function should cause an error

#### Loop Semantics

* The `until` condition must be boolean
* The count expression in a `repeat` statement must be an integer
* Loop variable names must be unique within the same loop variable list
* Loop variables may be reassigned inside the loop body; their initializer type determines their type

---

### Errors to Report

* `REDECLARED_IDENTIFIER`
* `UNDECLARED_IDENTIFIER`
* `TYPE_MISMATCH`
* `INVALID_ASSIGNMENT`
* `MISPLACED_RETURN`
* `ARGUMENT_COUNT_MISMATCH`
* `INVALID_FUNCTION`
* `IMMUTABLE_ASSIGNMENT`

---

## B. Executor (25 points)

Implement the Executor classes in the back end to run your Aiden code.

---

## C. Converter (25 points)

Implement the Converter classes in the back end to convert your code into Java or any other language.

* The generated code must execute correctly
* Formatting and spacing will **not** be graded

---

## D. Optimizations, Language Features, and Constructs (30 points)

(Combined: 30 points)

Implement any combination of the following. At least one item from each category is required. Points are awarded per item as listed below.

| Category | Items & Points |
|---|---|
| Optimizations | 3 × 5 pts each, min 1 required |
| Language Features | 4 × 5 pts each, min 1 required |
| Language Constructs | 3 × 5 pts each, min 1 required |

---

### Optimizations

Implement any of the following compiler optimizations. Each is worth **5 points**.

1. **Constant Folding** — evaluate constant expressions at compile time
2. **Loop Invariant Code Motion** — hoist loop-invariant computations out of loops
3. **Function Inlining** — replace calls with function bodies

---

### Language Features

Implement any of the following language features. Each is worth **5 points**.

**1. Value Pattern Matching**

Instead of chained `if/else if`, allow a `match` expression that branches on equality. All branches must return the same type. If no default (`_`) case is provided and no branch matches, the expression returns `none`.

```aiden
let grade = match score {
    90 -> "A",
    80 -> "B",
    _ -> "C"
};
```

**2. String Interpolation**

Backtick strings with `${expr}` embedded expressions:

```aiden
let msg = `Hello, ${name}! Your score is ${score}.`;
```

**3. Null-Coalescing Operator (`??`)**

The `??` operator returns the left operand if it is not `none`, otherwise it evaluates and returns the right operand. Like `&&` in C#, it short-circuits — the right side is not evaluated if the left side is non-null.

```aiden
let name = input ?? "Guest";
```

**4. Multiple Return Values**

Functions can return a comma-separated tuple, and callers can destructure with `let`:

```aiden
let fn minmax a b = if (a < b) then a, b else b, a;
let low, high = minmax(x, y);
```

---

### Language Constructs

Implement any of the following language constructs. Each is worth **5 points**.

**1. Enumerations**

Declare a fixed set of named constants with `enum`:

```aiden
enum Direction { North, South, East, West }
let d = Direction.North;
```

**2. Records (Named Structs)**

Declare a simple record type with typed, named fields:

```aiden
record Point { x: int, y: int }
let p = Point { x = 3, y = 4 };
let dist = p.x + p.y;
```

**3. First-Class Functions / Function Variables**

Allow functions to be stored in `let` bindings, passed as arguments, and returned:

```aiden
let fn double x = x * 2;
let fn apply f x = f(x);
print apply(double, 5);  # prints 10
```

```aiden
let fn multiplier n = let fn by x = x * n;
let triple = multiplier(3);
print triple(7);  # prints 21
```

---

### Extra Credit

Choose **at most one** of the following for **10 points** extra credit.

**Optimizations**

1. **Dead Code Elimination** — remove unreachable or unused code
2. **Common Subexpression Elimination** — avoid recomputing identical expressions
3. **Copy Propagation** — eliminate unnecessary variable-to-variable copies

**Language Construct**

4. **Lists** — an ordered, typed collection where all elements must be the same type.

*Static creation* — declare a list with a bracket literal:

```aiden
let nums = [1, 2, 3, 4, 5];
```

*Dynamic creation* — use `collect` inside a loop to build a list from computed values. `_i` is the current iteration index (0-based):

```aiden
let squares = collect repeat 5 { _i * _i };
```

*Iteration* — call `.loop` on a list variable. Inside the body, `_v` is the current element and `_i` is the current index:

```aiden
nums.loop {
    print `${_i}: ${_v}`;
}
```

---

## Submissions

* Check in your working code to your YU GitHub repository in your **Compilers branch**
* The project (`.pom` file) should be rooted in the `/Aiden` directory


