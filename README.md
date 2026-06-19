# ![CNApy screenshot](clingofmt.png) [![Build Status](https://github.com/potassco/clingofmt/workflows/CI%20Test/badge.svg)](https://github.com/potassco/clingofmt)

A tool that formats your clingo code.

## Compile

```sh
cargo build --release
```

## Run

```sh
./target/release/clingofmt example.lp > formatted_example.lp
```

To rewrite a file in place, pass `-i` or `--inplace` with an explicit file:

```sh
./target/release/clingofmt --inplace example.lp
```

If no file is provided, `clingofmt` reads from stdin and writes the formatted
program to stdout.

```sh
cat example.lp | ./target/release/clingofmt > formatted_example.lp
```

This will turn

```prolog
% Check lower and upper bounds via "Sinz counter" on selected varying atoms
index(A,I):-vary(A),I = #count{ B : vary(B),B <= A },not bounds(0,0).
counter(I,1)  :-index(A,I),bounds(L,U),L <= I,selected(A).
```

into

```prolog
% Check lower and upper bounds via "Sinz counter" on selected varying atoms
index(A, I) :-
    vary(A),
    I = #count {
        B :
            vary(B),
            B <= A
    },
    not bounds(0, 0).

counter(I, 1) :-
    index(A, I),
    bounds(L, U),
    L <= I,
    selected(A).

```

## Disable formatting

Use standalone `%% fmt` comments to keep hand-formatted or generated regions
unchanged:

```prolog
%% fmt: off
hand-formatted clingo code stays untouched
%% fmt: on
```

Marker comments must start with `%%` and occupy their own logical line. The
formatter also accepts flexible whitespace between marker tokens, such as
`%% fmt:   off`, `%%   fmt: on`, or `%% fmt :   off`. The colon after `fmt` is
required. Single-percent comments like `% fmt: off` are ordinary comments, and
marker-looking text inside string literals or block comments is ignored.

## Configure

`clingofmt` reads formatting options from a `.clingofmt` file in the current
working directory.

```toml
break_after_head = true
break_after_body_atom = false
break_after_colon = true
soft_flush_limit = 60
align_continuation_after_colon = true
indent_width = 4
```

which typically pipe the current buffer to the formatter over stdin.
