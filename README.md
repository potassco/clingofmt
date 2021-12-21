# clingofmt

A tool that automatically formats your clingo code.

**ATTENTION**: This is work in progress `clingofmt` will eat your code!

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
    I=#count { B : 
            vary(B),
            B<=A
    },
    not bounds(0,0).

counter(I,1) :- 
    index(A,I),
    bounds(L,U),
    L<=I,
    selected(A).
```
