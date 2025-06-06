<h1 align="center">ioscript</h1>
The 'big thing' about this language is that you can specify the types a file will take as arguments:
```
ioc examples/sum.ios(i32, i32)
```

In the file accessing arguments looks like this:
```
# examples/sum.ios
a := @arg(0)
b := @arg(1)
ret a + b
```

Files can be run from other files and if successful they can be evaluated fully
at compile time:
```
# examples/main.ios
ret @run("sum.ios", 1, 1) # evaluated to 2
```
In this case the types of the passed arguments are both `static_int` (which is an int with arbitrary bit width) because that is
the type an int literal is evaluated to.

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
