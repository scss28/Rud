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
| Name  | Description                                                              |
|-------|--------------------------------------------------------------------------|
| `num` | Represents a number                                                      |
| `nil` | Represents no value, returned by expressions such as variable assignment |

### Operators
| Name           | Syntax   | Description                    |
|----------------|----------|--------------------------------|
| Addition       | `a + b`  | Add two numbers                |
| Subtraction    | `a - b`  | Subtract two numbers           |
| Multiplication | `a * b`  | Multiply two numbers           |
| Division       | `a / b`  | Divide one number by the other |
| Power          | `a ** b` | Raise one number to some power |

### Builtin Functions
| Name            | Description                               |
|-----------------|-------------------------------------------|
| `lerp(a, b, t)` | Linear interpolation between a and b by t |

### Variables
```
a := 2  # a is now 2
a       # = 2
```

## Examples
### Repl
```
$ rud
> 2 + 2
= 4

> a := 2
> a ** 3
= 8

> a * (3 - 1) / 12
= 0.3333333333333333

> lerp(1, 3, 0.5)
= 2
```
