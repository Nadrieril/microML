# MicroML

### Features

- Type inference
- Compilation to an Abtract Stack Machine
- Algebraic DataTypes

### Build

Install [stack](http://docs.haskellstack.org/en/stable/README); run

    stack build

### Usage

    ./microml eval [--show] <file>

    ./microml compile <file>

    ./microml run <compiled file>

See the `tests/` folder for examples.

### Known bugs

- Equality is restricted to `int`s/`bool`s
- Semantics are neither totally strict nor totally lazy due to interpreting a strict instruction set in Haskell and a lack of time
