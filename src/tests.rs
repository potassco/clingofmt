#![warn(missing_docs)]
#![deny(clippy::missing_docs_in_private_items)]
//! Unit tests for formatter output and configuration behavior.

use crate::{format_source, FormatOptions};

/// Formats `source_code` with default options and compares it to `res`.
fn fmt_and_cmp(source_code: &str, res: &str) {
    fmt_with_options(source_code, res, &FormatOptions::default());
}

/// Formats `source_code` with explicit options and compares it to `res`.
fn fmt_with_options(source_code: &str, res: &str, options: &FormatOptions) {
    let mut buf = Vec::new();
    format_source(source_code.as_bytes(), &mut buf, false, options).unwrap();
    let parse_res = std::str::from_utf8(&buf).unwrap();
    assert_eq!(parse_res, res)
}

#[test]
/// Verifies that the built-in defaults match the recommended configuration.
fn test_recommended_default_options() {
    assert_eq!(
        FormatOptions::default(),
        FormatOptions {
            break_after_head: true,
            break_after_body_atom: false,
            break_after_colon: true,
            soft_flush_limit: 60,
            align_continuation_after_colon: true,
            indent_width: 4,
        }
    );
}

#[test]
/// Verifies basic formatting behavior across a mixed sample of inputs.
fn test_pass_new() {
    fmt_and_cmp(
        " 
 
 ", r#""#,
    );
    fmt_and_cmp(
        "% rna blub       ",
        r#"% rna blub
"#,
    );
    fmt_and_cmp(
        "% rna
% blub       ",
        r#"% rna
% blub
"#,
    );
    fmt_and_cmp(
        "%* multi  
    line
    comment  *%",
        r#"%* multi  
    line
    comment  *%
"#,
    );
    fmt_and_cmp(
        " gene(something).        ",
        r#"gene(something).
"#,
    );
    fmt_and_cmp(
        " gene(something).     % rna   ",
        r#"gene(something).

% rna
"#,
    );
    fmt_and_cmp(
        "% rna blub
   g:-t.   ",
        r#"% rna blub
g :- t.
"#,
    );
    fmt_and_cmp(
        "% fact block
 g(1).
 g(2). g(3).",
        r#"% fact block
g(1). g(2). g(3).
"#,
    );
    fmt_and_cmp(
        "%* fact block *%  
  g(1).   
g(2). g(3).",
        r#"%* fact block *%
g(1). g(2). g(3).
"#,
    );
    fmt_and_cmp(
        "%* fact block *%  
  g(1%*rna*%   ).   
g(2). g(3).",
        r#"%* fact block *%
g(1%*rna*%). g(2). g(3).
"#,
    );
    fmt_and_cmp(
        "% fact block1 
  g(1%*rna*%   ).  
g(2). g(3).%* fact block2 *%  t(1%*rna*%   ).  
t(2). t(3).",
        r#"% fact block1
g(1%*rna*%). g(2). g(3).

%* fact block2 *%
t(1%*rna*%). t(2). t(3).
"#,
    );
}

#[test]
/// Preserves disabled regions exactly while formatting surrounding source.
fn test_fmt_skip_basic_region() {
    fmt_and_cmp(
        "gene( something ).
%% fmt: off
  rna(  blub):-dna( x ).
%% fmt: on
seed( thing ).",
        r#"gene(something).
%% fmt: off
  rna(  blub):-dna( x ).
%% fmt: on
seed(thing).
"#,
    );
}

#[test]
/// Allows invalid clingo text inside disabled regions.
fn test_fmt_skip_invalid_region() {
    fmt_and_cmp(
        "gene(something).
%% fmt: off
This section will not be formatted
%% fmt: on
seed( thing ).",
        r#"gene(something).
%% fmt: off
This section will not be formatted
%% fmt: on
seed(thing).
"#,
    );
}

#[test]
/// Treats an unclosed off marker as raw until the end of the input.
fn test_fmt_skip_unclosed_region() {
    fmt_and_cmp(
        "gene( something ).  
%% fmt: off
This section will not be formatted
  gene( something ).",
        r#"gene(something).
%% fmt: off
This section will not be formatted
  gene( something )."#,
    );
}

#[test]
/// Recognizes marker lines with flexible whitespace while preserving them exactly.
fn test_fmt_skip_flexible_marker_whitespace() {
    fmt_and_cmp(
        "  %% fmt :   off   
This section will not be formatted
  %%   fmt: on   
gene( thing ).",
        r#"  %% fmt :   off   
This section will not be formatted
  %%   fmt: on   
gene(thing).
"#,
    );
}

#[test]
/// Resumes normal formatting after each on marker.
fn test_fmt_skip_resumes_formatting_after_on_marker() {
    fmt_and_cmp(
        "%% fmt: off
  raw(  region):-stays( untouched ).
%% fmt: on
gene( thing ). seed( thing ).
%% fmt: off
  another(  raw):-region( here ).
%% fmt: on
rna( blub ):-dna( x ).",
        r#"%% fmt: off
  raw(  region):-stays( untouched ).
%% fmt: on
gene(thing). seed(thing).
%% fmt: off
  another(  raw):-region( here ).
%% fmt: on
rna(blub) :- dna(x).
"#,
    );
}

#[test]
/// Ignores inline marker-looking comments instead of entering raw mode.
fn test_fmt_skip_ignores_inline_marker() {
    fmt_and_cmp(
        "gene(something). %% fmt: off
seed( thing ).",
        r#"gene(something).

%% fmt: off
seed(thing).
"#,
    );
}

#[test]
/// Ignores single-percent comments that look like formatter markers.
fn test_fmt_skip_requires_double_percent() {
    fmt_and_cmp(
        "% fmt: off
gene( thing ).",
        r#"% fmt: off
gene(thing).
"#,
    );
}

#[test]
/// Ignores double-percent comments that omit the required colon after fmt.
fn test_fmt_skip_requires_colon_after_fmt() {
    fmt_and_cmp(
        "%% fmt   off
gene( thing ).
%%   fmt on
seed( thing ).",
        r#"%% fmt   off
gene(thing).

%%   fmt on
seed(thing).
"#,
    );
}

#[test]
/// Ignores marker-looking text inside strings and block comments.
fn test_fmt_skip_ignores_string_and_block_comment_text() {
    fmt_and_cmp(
        "gene(\" %% fmt: off something something\"). seed(thing).
%* doc
%% fmt: off
*%
rna( blub ).",
        r#"gene(" %% fmt: off something something"). seed(thing).

%* doc
%% fmt: off
*%
rna(blub).
"#,
    );
}

#[test]
/// Verifies grouping and separation behavior for consecutive include directives.
fn test_include_section() {
    let source = "% Comment for include block
  #include \"file1.lp\".#include \"file2.lp\".     #include \"file3.lp\".
";
    let result = r#"% Comment for include block
#include "file1.lp".
#include "file2.lp".
#include "file3.lp".
"#;
    fmt_and_cmp(source, result);

    let source = "% Comment for include block
#include \"file1.lp\".#include \"file2.lp\".     #include \"file3.lp\". seed(fact).
";
    let result = r#"% Comment for include block
#include "file1.lp".
#include "file2.lp".
#include "file3.lp".

seed(fact).
"#;
    fmt_and_cmp(source, result);

    let source = "% Comment for include block
#include \"file1.lp\".#include \"file2.lp\".     #include \"file3.lp\". #show gene/3.
";
    let result = r#"% Comment for include block
#include "file1.lp".
#include "file2.lp".
#include "file3.lp".

#show gene/3.
"#;
    fmt_and_cmp(source, result);

    let source = "% Comment for include block
#include \"file1.lp\".#include \"file2.lp\".     #include \"file3.lp\". g :- root.
";
    let result = r#"% Comment for include block
#include "file1.lp".
#include "file2.lp".
#include "file3.lp".

g :- root.
"#;
    fmt_and_cmp(source, result);

    let source = "% Comment for include block
#include \"file1.lp\".#include \"file2.lp\".     #include \"file3.lp\". %comment
g:-root.
";
    let result = r#"% Comment for include block
#include "file1.lp".
#include "file2.lp".
#include "file3.lp".

%comment
g :- root.
"#;
    fmt_and_cmp(source, result);
}

#[test]
/// Verifies grouping and separation behavior for consecutive defined directives.
fn test_defined_section() {
    let source = "#defined dna_seq/2.#defined zygote/2.
    #defined genotype/1.
#defined organ/2. #defined plasmid/2.
";
    let result = r#"#defined dna_seq/2.
#defined zygote/2.
#defined genotype/1.
#defined organ/2.
#defined plasmid/2.
"#;
    fmt_and_cmp(source, result);

    let source = "% defined thing
#defined dna_seq/2.#defined genotype/2. seed(root).
";
    let result = r#"% defined thing
#defined dna_seq/2.
#defined genotype/2.

seed(root).
"#;
    fmt_and_cmp(source, result);

    let source = "#defined dna_seq/2.#defined zygote/2. % comment
g:-t.
";
    let result = r#"#defined dna_seq/2.
#defined zygote/2.

% comment
g :- t.
"#;
    fmt_and_cmp(source, result);
}

#[test]
/// Verifies grouping and separation behavior for additional directive blocks.
fn test_additional_directive_sections() {
    let source = "#external g.#external t.
#const u=1.#const r=2.
#project g(X).#project u(X).
";
    let result = r#"#external g.
#external t.

#const u=1.
#const r=2.

#project g(X).
#project u(X).
"#;
    fmt_and_cmp(source, result);

    let source = "#heuristic at.[1@1,true]#heuristic gc.[2@1,true]
#defined g/1.#defined u/2. stem(1).
";
    let result = r#"#heuristic at. [1@1, true]
#heuristic gc. [2@1, true]

#defined g/1.
#defined u/2.

stem(1).
"#;
    fmt_and_cmp(source, result);

    let source = "% directive block
#external g.#external t. % comment
#project g(X).#project u(X).
";
    let result = r#"% directive block
#external g.
#external t.

% comment
#project g(X).
#project u(X).
"#;
    fmt_and_cmp(source, result);
}

#[test]
/// Verifies that body-level commas are preferred over nested tuple commas.
fn test_soft_break_prefers_body_comma_over_tuple_comma() {
    fmt_and_cmp(
        "_chromosome(Parent,true)  :- _chromosome(Child,true), germ(Child), Child=(Parent,_).",
        r#"_chromosome(Parent, true) :-
    _chromosome(Child, true),
    germ(Child),
    Child = (Parent, _).
"#,
    );
}

#[test]
/// Verifies that body-level commas are preferred over nested semicolon wraps.
fn test_soft_break_prefers_body_comma_over_nested_semicolon() {
    fmt_and_cmp(
        "nucleus(X) :- ribosome_type(X, ((base; set), T)), respiration(T, _).",
        r#"nucleus(X) :- ribosome_type(X, ((base; set), T)),
    respiration(T, _).
"#,
    );
}

#[test]
/// Verifies that nested breaks are still used when no outer break exists.
fn test_soft_break_uses_nested_break_when_no_outer_break_exists() {
    fmt_and_cmp(
        "g(X) :- Child = (VeryLongParentIdentifierNameWithExtraPaddingAndMorePaddingAndEvenMore, AnotherVeryLongIdentifierNameWithExtraPaddingAndMorePaddingAndEvenMore).",
        r#"g(X) :-
    Child = (VeryLongParentIdentifierNameWithExtraPaddingAndMorePaddingAndEvenMore,
        AnotherVeryLongIdentifierNameWithExtraPaddingAndMorePaddingAndEvenMore).
"#,
    );
}

#[test]
/// Verifies that a long rule head still prefers a break at `:-`.
fn test_break_after_head_wins_after_head_soft_wrap() {
    fmt_and_cmp(
        "_expression_pythonEval(E, operation(OP, (val(T1, V1), (val(T2, V2), ())))) :- _computeIdx(E, OP), (T1; T2) = float, T1 = (float; int), T2 = (float; int).",
        r#"_expression_pythonEval(E, operation(OP, (val(T1, V1), (val(T2,
                    V2), ())))) :-
    _computeIdx(E, OP),
    (T1;
        T2) = float,
    T1 = (float;
        int),
    T2 = (float;
        int).
"#,
    );
}

#[test]
/// Verifies optional colon-based continuation alignment behavior.
fn test_optional_colon_continuation_alignment() {
    let source = "nucleotide(PX, P1) :- ribosome_group(PG, P1), ribosome_group(PG, PX), pore(P1, in, _), not pore(PX, in, _), #false : ribosome_group(PG, P2), P2 != PX,   not pore(P2, in, _).";

    fmt_with_options(
        source,
        r#"nucleotide(PX, P1) :-
    ribosome_group(PG, P1),
    ribosome_group(PG, PX),
    pore(P1, in, _),
    not pore(PX, in, _),
    #false : ribosome_group(PG, P2),
             P2 != PX,
             not pore(P2, in, _).
"#,
        &FormatOptions::default(),
    );

    fmt_with_options(
        source,
        r#"nucleotide(PX, P1) :-
    ribosome_group(PG, P1),
    ribosome_group(PG, PX),
    pore(P1, in, _),
    not pore(PX, in, _),
    #false : ribosome_group(PG, P2),
             P2 != PX,
             not pore(P2, in, _).
"#,
        &FormatOptions {
            align_continuation_after_colon: true,
            ..FormatOptions::default()
        },
    );

    fmt_with_options(
        "zygote(P1) :- zygote(P2), not pore(P2, _, _), #false : ribosome_group(PG, P3), pore(P3, in, _).",
        r#"zygote(P1) :-
    zygote(P2),
    not pore(P2, _, _),
    #false : ribosome_group(PG, P3),
             pore(P3, in, _).
"#,
        &FormatOptions {
            break_after_head: true,
            break_after_body_atom: false,
            break_after_colon: false,
            indent_width: 4,
            soft_flush_limit: 60,
            align_continuation_after_colon: true,
        },
    );

    fmt_with_options(
        "#show protein_signal_aux(@spores(C), E) : protein_signal_aux(C, E).",
        r#"#show protein_signal_aux(@spores(C), E) :
    protein_signal_aux(C, E).
"#,
        &FormatOptions {
            break_after_head: true,
            break_after_body_atom: false,
            break_after_colon: true,
            soft_flush_limit: 60,
            align_continuation_after_colon: true,
            indent_width: 4,
        },
    );

    fmt_with_options(
        "protein_cluster(C, 1) :- chromosome(C), #false : pore(P, (in; existence), _), P = (C, _).",
        r#"protein_cluster(C, 1) :- chromosome(C),
    #false : pore(P, (in; existence), _),
             P = (C, _).
"#,
        &FormatOptions {
            break_after_head: true,
            break_after_body_atom: false,
            break_after_colon: true,
            soft_flush_limit: 60,
            align_continuation_after_colon: true,
            indent_width: 4,
        },
    );
}

#[test]
/// Verifies legacy formatter output on a large regression sample.
fn test_pass_old() {
    let source = "% Derive (varying) atoms
gene(A):-spore(M),live(M,A).grow(A):-spore(M),gene(A),not live(M,A).
% Derive lower bound LB and upper bound UB for size of prime implicants
% - LB: minimum number of varying atoms s.t. interpretations don't exceed models
% - UB: minimum of number of varying atoms and number of non-models
allele(X):-X = #count{ A : grow(A) }.spores(Y):-Y = #count{ M : spore(M) }.:- spores(0). % must have some model
gastrin(Y,2**X,0) :-allele(X),spores(Y),1 < Y. % nothing varies if one model
gastrin(Y,Z/2,L+1):-gastrin(Y,Z,L),Y < Z.
genome(L,(X+F-|X-F|)/2):-allele(X),gastrin(Y,Z,L),not gastrin(Y,Z/2,L+1),
                          F = 2**X-Y.
% Select literals for prime implicant
  neuron(A,1)         :-gene(A),not grow(A).{ neuron(A,0..1) } < 2:-grow(A),not genome(0,0).
mutation(A):-neuron(A,V),grow(A).
% Check lower and upper bounds via \"Sinz counter\" on selected varying atoms
ovary(A,I):-grow(A),I = #count{ B : grow(B),B <= A },not genome(0,0).
protein(I,1)  :-ovary(A,I),genome(L,U),L <= I,mutation(A).protein(I,C+1):-ovary(A,I),genome(L,U),C < U,mutation(A),protein(I+1,C).protein(I,C)  :-ovary(A,I),genome(L,U),L < C+I,protein(I+1,C).
:- genome(L,U),0 < L,not protein(1,L).:- genome(L,U),ovary(A,I),mutation(A),protein(I+1,U).
% Derive models excluded by (some) selected literal
culture(M,A):-spore(M),neuron(A,0),live(M,A).culture(M,A):-spore(M),neuron(A,1),not live(M,A).
proteome(M):-culture(M,A).
% Check that all interpretations extending prime implicant are models
:- genome(L,U),allele(X),spores(Y),
   #sum{ 2**(X-Z) : Z = L+1..X,not protein(1,Z);
              1,M : proteome(M) } >= Y.
% Check that removing any literal of prime implicant yields some non-model
:- genome(L,U),allele(X),spores(Y),ovary(A,I),   #sum{ 2**(X-Z) : Z = L..X,not protein(1,Z+1);   1,M : culture(M,B),B != A } < Y.
% Display literals of prime implicant
#show neuron/2.
#show g(A) : t(A), field(AN).
#show neuron(\"root\",X).
              g(s).bp(x).
stamen(@dna((\"The @dna() function is flexible enough to take multi-line \",
             \"strings containing many placeholders: {} and \",
             \"{} and {} outputs\"), (X,Y,Z))) :- ova(X),pollen(Y),plastids(Z).
rna_tag(H, V) :- rna_tag(N,W) : seed(Identifier,bud(N,W));
   ribosome(N) : seed(Identifier, dna_x(\"strong\",N))%*jjj*%;
   %c1
   gen_map(F, T, Na, Index)
   %c0
   %c01
   : seed(Identifier, gen_map(\"strong\", F, T, Na, Index));
   %c2
   %c3
   not gen_map(F, _, Na, Index) : seed(Identifier, gen_map(\"weak\", F, Na, Index));  %* c2
    sss *%
   seed(Identifier,axon(H,V)).

    rna%aa
    %bp
    :-%aa
    %bp
     allele(X),
    #sum %aa
    %bp
    { %aa
    %bp
        2**(X-Z)%aa
    %bp
    :%aa
    %bp
            Z = L+1..X,%aa
    %bp
            not protein(1, Z); %aa
    %bp
        1, M :
            proteome(M)
    } %aa
    %bp
     >= %aa
    %bp
     Y %aa
    %bp
    ,spores(Y).

:- rna(1,2).
#external g.
#const u= \"Dd\".
#minimize{dna}.
#maximise {X:dna(X)}.
#include \"fail1.lp\".
#heuristic rna.[at,gc]
#defined rna/2.
#project g(\"s\",X).

#theory test {
    &u/1 : t, body;
    &r/0 : t, { < }, t, directive
}.
#edge(g,t).#edge(u,r).

";
    let result = r#"% Derive (varying) atoms
gene(A) :- spore(M), live(M, A).

grow(A) :- spore(M), gene(A), not live(M, A).

% Derive lower bound LB and upper bound UB for size of prime implicants
% - LB: minimum number of varying atoms s.t. interpretations don't exceed models
% - UB: minimum of number of varying atoms and number of non-models
allele(X) :- X = #count {
        A : grow(A)
    }.

spores(Y) :- Y = #count {
        M : spore(M)
    }.

 :- spores(0).

% must have some model
gastrin(Y, 2**X, 0) :- allele(X), spores(Y), 1 < Y.

% nothing varies if one model
gastrin(Y, Z/2, L+1) :- gastrin(Y, Z, L), Y < Z.

genome(L, (X+F- | X-F | )/2) :-
    allele(X),
    gastrin(Y, Z, L),
    not gastrin(Y, Z/2, L+1),
    F = 2**X-Y.

% Select literals for prime implicant
neuron(A, 1) :- gene(A), not grow(A).

{
    neuron(A, 0..1)
} < 2 :- grow(A), not genome(0, 0).

mutation(A) :- neuron(A, V), grow(A).

% Check lower and upper bounds via "Sinz counter" on selected varying atoms
ovary(A, I) :- grow(A), I = #count {
        B : grow(B), B <= A
    }, not genome(0, 0).

protein(I, 1) :-
    ovary(A, I),
    genome(L, U),
    L <= I,
    mutation(A).

protein(I, C+1) :-
    ovary(A, I),
    genome(L, U),
    C < U,
    mutation(A),
    protein(I+1, C).

protein(I, C) :-
    ovary(A, I),
    genome(L, U),
    L < C+I,
    protein(I+1, C).

 :- genome(L, U), 0 < L, not protein(1, L).

 :- genome(L, U), ovary(A, I), mutation(A), protein(I+1, U).

% Derive models excluded by (some) selected literal
culture(M, A) :- spore(M), neuron(A, 0), live(M, A).

culture(M, A) :- spore(M), neuron(A, 1), not live(M, A).

proteome(M) :- culture(M, A).

% Check that all interpretations extending prime implicant are models
 :- genome(L, U), allele(X), spores(Y), #sum {
        2**(X-Z) : Z = L+1..X, not protein(1, Z);
        1, M : proteome(M)
    } >= Y.

% Check that removing any literal of prime implicant yields some non-model
 :- genome(L, U), allele(X), spores(Y), ovary(A, I), #sum {
        2**(X-Z) : Z = L..X, not protein(1, Z+1);
        1, M : culture(M, B), B != A
    } < Y.

% Display literals of prime implicant
#show neuron/2.
#show g(A) : t(A), field(AN).
#show neuron("root", X).

g(s). bp(x).

stamen(@dna(("The @dna() function is flexible enough to take multi-line ",
            "strings containing many placeholders: {} and ",
            "{} and {} outputs"), (X, Y, Z))) :-
    ova(X),
    pollen(Y),
    plastids(Z).

rna_tag(H, V) :-
    rna_tag(N, W) : seed(Identifier, bud(N, W));
    ribosome(N) : seed(Identifier, dna_x("strong", N))%*jjj*%;
    %c1
    gen_map(F, T, Na, Index)%c0
    %c01
     : seed(Identifier, gen_map("strong", F, T, Na, Index));
    %c2
    %c3
    not gen_map(F, _, Na, Index) : seed(Identifier, gen_map("weak",
                F, Na, Index));
    %* c2
    sss *%seed(Identifier, axon(H, V)).

rna%aa
%bp
 :- %aa
    %bp
    allele(X), #sum %aa
    %bp
    {
        %aa
        %bp
        2**(X-Z)%aa
        %bp
         : %aa
            %bp
            Z = L+1..X, %aa
            %bp
            not protein(1, Z);
        %aa
        %bp
        1, M : proteome(M)
    }%aa
    %bp
     >= %aa
    %bp
    Y%aa
    %bp
    , spores(Y).

 :- rna(1, 2).

#external g.

#const u="Dd".

#minimize {
    dna
}.

#maximise {
    X : dna(X)
}.

#include "fail1.lp".

#heuristic rna. [at, gc]

#defined rna/2.

#project g("s", X).

#theory test {
    &u/1 : t, body;
    &r/0 : t, { < }, t, directive
}.

#edge(g, t). #edge(u, r).
"#;
    fmt_with_options(
        source,
        result,
        &FormatOptions {
            break_after_head: true,
            break_after_body_atom: false,
            break_after_colon: false,
            soft_flush_limit: 60,
            align_continuation_after_colon: false,
            indent_width: 4,
        },
    );
}

#[test]
/// Verifies configurable break options on representative short rules.
fn test_configurable_breaks() {
    let source = "g:-t,u. ovum(X) :- leaf(X) : cell(X), algae(X).";

    fmt_with_options(
        source,
        r#"g :- t, u.

ovum(X) :- leaf(X) : cell(X), algae(X).
"#,
        &FormatOptions {
            break_after_head: false,
            break_after_body_atom: false,
            break_after_colon: false,
            align_continuation_after_colon: false,
            indent_width: 4,
            soft_flush_limit: 60,
        },
    );

    fmt_with_options(
        source,
        r#"g :-
    t,
    u.

ovum(X) :- leaf(X) :
        cell(X),
        algae(X).
"#,
        &FormatOptions {
            break_after_head: true,
            break_after_body_atom: true,
            break_after_colon: true,
            align_continuation_after_colon: false,
            indent_width: 4,
            soft_flush_limit: 60,
        },
    );
}

#[test]
/// Verifies that `break_after_head` respects the configured soft flush limit.
fn test_break_after_head_respects_soft_flush_limit() {
    let source = "leaf(lalala):-cell(lalala).";

    fmt_with_options(
        source,
        r#"leaf(lalala) :-
    cell(lalala).
"#,
        &FormatOptions {
            break_after_head: true,
            break_after_body_atom: false,
            break_after_colon: false,
            align_continuation_after_colon: false,
            indent_width: 4,
            soft_flush_limit: 10,
        },
    );

    fmt_with_options(
        source,
        r#"leaf(lalala) :- cell(lalala).
"#,
        &FormatOptions {
            break_after_head: true,
            break_after_body_atom: false,
            break_after_colon: false,
            align_continuation_after_colon: false,
            indent_width: 4,
            soft_flush_limit: 80,
        },
    );

    fmt_with_options(
        "ribosome_assignment(A, V) :- _chromosome(A, V), organelle(A, _), plasmids(A).",
        r#"ribosome_assignment(A, V) :-
    _chromosome(A, V),
    organelle(A, _),
    plasmids(A).
"#,
        &FormatOptions {
            break_after_head: true,
            break_after_body_atom: false,
            break_after_colon: false,
            align_continuation_after_colon: false,
            indent_width: 4,
            soft_flush_limit: 60,
        },
    );

    fmt_with_options(
        "stem(P) :- germ(P), P = ((), _).",
        r#"stem(P) :-
    germ(P),
    P = ((), _).
"#,
        &FormatOptions {
            break_after_head: true,
            break_after_body_atom: true,
            break_after_colon: false,
            align_continuation_after_colon: true,
            indent_width: 4,
            soft_flush_limit: 60,
        },
    );
}

#[test]
/// Verifies that indentation width is configurable.
fn test_configurable_indent_width() {
    fmt_with_options(
        "ribosome_assignment(A, V) :- _chromosome(A, V), organelle(A, _), plasmids(A).",
        r#"ribosome_assignment(A, V) :-
  _chromosome(A, V),
  organelle(A, _),
  plasmids(A).
"#,
        &FormatOptions {
            break_after_head: true,
            break_after_body_atom: false,
            break_after_colon: false,
            align_continuation_after_colon: false,
            indent_width: 2,
            soft_flush_limit: 60,
        },
    );

    fmt_with_options(
        "zygote(P1) :- zygote(P2), not pore(P2, _, _), #false : ribosome_group(PG, P3), pore(P3, in, _).",
        r#"zygote(P1) :-
  zygote(P2),
  not pore(P2, _, _),
  #false : ribosome_group(PG, P3),
           pore(P3, in, _).
"#,
        &FormatOptions {
            break_after_head: true,
            break_after_body_atom: false,
            break_after_colon: false,
            align_continuation_after_colon: true,
            indent_width: 2,
            soft_flush_limit: 60,
        },
    );
}
