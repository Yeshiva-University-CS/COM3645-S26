## **Aiden Language Summary**

### **Data Types**
- `integer`: Whole numbers (e.g., 5)
- `real`: Decimal numbers (e.g., 3.14)
- `boolean`: `true` or `false`
- `string`: Quoted strings (e.g., `"hello"`)
- `none`: Null-like value

### **Declarations**
- **Bindings**: `let x = 5;`
- **Functions**: `let fn add a b = a + b;` or with a block body:

  ```aiden
  let fn greet name = {
    let message = "Hello, " + name;
    return message;
  }
  ```

### **Expressions and Operators**

| Type                 | Operators                     | Example                    |
|----------------------|-------------------------------|----------------------------|
| Arithmetic           | `+`, `-`, `*`, `/`, unary `-` | `x + y`, `-a`, `a / b`     |
| String Concatenation | `+`                           | `"Hi, " + name`            |
| Logical              | `and`, `or`, `!`              | `a and b`, `!flag`         |
| Comparison           | `<`, `>`, `<=`, `>=`          | `x < y`, `score >= 90`     |
| Equality             | `==`, `!=`                    | `x == y`, `flag != true`   |
| Assignment           | `=`                           | `x = y + 1;`               |
| Function call        | `f(x, y)`                     | `add(2, 3)`                |

### **Statements and Control Flow**
- **Conditionals**: `if (cond) { ... } else { ... }`
- **Loops**: Loop-scoped variables are declared in a parenthesized list before the loop body: `(x = 0, y = 1)`. These variables are mutable and may be reassigned with `=` inside the loop body:

  ```aiden
  loop (sum = 0, i = 1) {
    sum = sum + i;
    i = i + 1;
  } until (i > 10);
  ```

  - `loop (vars) { ... } until (cond);`
  - `repeat n times (vars) { ... }`
- **Print**: `print value;`
- **Return**: `return value;` (inside functions)
- **Blocks**: `{ ... }` introduce a new scope
