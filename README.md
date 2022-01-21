# clingofmt [![Build Status](https://github.com/potassco/clingofmt/workflows/CI%20Test/badge.svg)](https://github.com/potassco/clingofmt)

A tool that formats your clingo code.

## Compile

```sh
cargo build --release
```

## Run

```sh
.target/release/clingofmt example.lp > formated_example.lp
```

This will turn:

```prolog
% Check lower and upper bounds via "Sinz counter" on selected varying atoms
index(A,I):-vary(A),I = #count{ B : vary(B),B <= A },not bounds(0,0).
counter(I,1)  :-index(A,I),bounds(L,U),L <= I,selected(A).
```

into

```prolog
% Check lower and upper bounds via "Sinz counter" on selected varying atoms
index(A,I) :- 
    vary(A),
    I = #count { B : 
            vary(B),
            B <= A
    },
    not bounds(0,0).

counter(I,1) :- 
    index(A,I),
    bounds(L,U),
    L <= I,
    selected(A).
```
