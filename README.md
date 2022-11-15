# OCamlCalc

It's a OCaml project I wanted to work on for fun. The objective is to make a
calculator with a TUI interface, or just quicly evaluate the command line
arguments to instantly give the result. There are probably 1000 projects like
this but it's fun so I don't care ^^


## Usage

For now, it's only capable of evaluating command line arguments, no interface,
and only in the `reverse polish` style, since it's a lot more ergonomic, (you
don't need to write parentheses) and absolutely not because I'm lazy and this
style is a lot easier to parse then the standard style.

In all seriousness, I'm currently working on making a parser for the standard
style. I just wanted something that works before doing the hard parts.

## Objectives

- [X] Quick evaluation reverse polish style
- [X] Quick evaluation standard style
- [X] A descriptive help message / docs / cleaner error handeling
- [ ] rudementary TUI interface
- [ ] fancy functions (like `sin`, `ln`...)
- [ ] custom variable / function definition
