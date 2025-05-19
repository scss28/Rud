<h1 align="center">Rud</h1>
<h3 align="center">A language</h3>

## Getting started
### Prerequisites
- [Zig](https://ziglang.org) (0.14.0)

### Building and running
```
# Build
zig build

# Build and run
zig build run

# See other options
zig build --help
```

## Language Reference
### Types
| Name    | Description                                                   |
|---------|---------------------------------------------------------------|
| `int`   | A 32-bit integer                                              |
| `float` | A 32-bit floating point number                                |
| `cstr`  | A constant string                                             |
| `nil`   | No value, returned by expressions such as variable assignment |

### Operators
| Name           | Syntax   | Description                    |
|----------------|----------|--------------------------------|
| Addition       | `a + b`  | Add two numbers                |
| Subtraction    | `a - b`  | Subtract two numbers           |
| Multiplication | `a * b`  | Multiply two numbers           |
| Division       | `a / b`  | Divide one number by the other |
| Power          | `a ** b` | Raise one number to some power |

### Builtin Functions
| Name                  | Description                                   |
|-----------------------|-----------------------------------------------|
| `@print(fmt, args..)` | Print a string with optional format arguments |
| `@int(x)`             | Convert a `float` to an `int`                 |
| `@float(x)`           | Convert an `int` to a `float`                 |

## Examples
### Hello, world!
```
a := "Hello"
b := "world"
@print("{}, {}!\n", a, b)
```
run: `zig build run -- run examples/hello_world.rud`
