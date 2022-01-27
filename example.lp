% Derive (varying) atoms
atom(A):-model(M),true(M,A).vary(A):-model(M),atom(A),not true(M,A).
% Derive lower bound LB and upper bound UB for size of prime implicants
% - LB: minimum number of varying atoms s.t. interpretations don't exceed models
% - UB: minimum of number of varying atoms and number of non-models
varies(X):-X = #count{ A : vary(A) }.models(Y):-Y = #count{ M : model(M) }.:- models(0). % must have some model
minsize(Y,2**X,0) :-varies(X),models(Y),1 < Y. % nothing varies if one model
minsize(Y,Z/2,L+1):-minsize(Y,Z,L),Y < Z.
bounds(L,(X+F-|X-F|)/2):-varies(X),minsize(Y,Z,L),not minsize(Y,Z/2,L+1),
                          F = 2**X-Y.
% Select literals for prime implicant
  select(A,1)         :-atom(A),not vary(A).{ select(A,0..1) } < 2:-vary(A),not bounds(0,0).
selected(A):-select(A,V),vary(A).
% Check lower and upper bounds via "Sinz counter" on selected varying atoms
index(A,I):-vary(A),I = #count{ B : vary(B),B <= A },not bounds(0,0).
counter(I,1)  :-index(A,I),bounds(L,U),L <= I,selected(A).counter(I,C+1):-index(A,I),bounds(L,U),C < U,selected(A),counter(I+1,C).counter(I,C)  :-index(A,I),bounds(L,U),L < C+I,counter(I+1,C).
:- bounds(L,U),0 < L,not counter(1,L).:- bounds(L,U),index(A,I),selected(A),counter(I+1,U).
% Derive models excluded by (some) selected literal
exclude(M,A):-model(M),select(A,0),true(M,A).exclude(M,A):-model(M),select(A,1),not true(M,A).
excluded(M):-exclude(M,A).
% Check that all interpretations extending prime implicant are models
:- bounds(L,U),varies(X),models(Y),
   #sum{ 2**(X-Z) : Z = L+1..X,not counter(1,Z);
              1,M : excluded(M) } >= Y.
% Check that removing any literal of prime implicant yields some non-model
:- bounds(L,U),varies(X),models(Y),index(A,I),   #sum{ 2**(X-Z) : Z = L..X,not counter(1,Z+1);   1,M : exclude(M,B),B != A } < Y.
% Display literals of prime implicant
#show select/2.
              n(s).bb(x).
output(@fmt(("The @fmt() function is flexible enough to take multi-line ",
             "strings containing many placeholders: {} and ",
             "{} and {} outputs"), (X,Y,Z))) :- num(X),string(Y),constant(Z).
sel_vat(H, V) :- sel_vat(N,W) : cons(Identifier,var(N,W));
   subgraph(N) : cons(Identifier, has_x("strong",N))%*jjj*%;
   %c1
   has_con(F, T, Na, Index) 
   %c0
   %c01
   : cons(Identifier, has_con("strong", F, T, Na, Index));
   %c2
   %c3
   not has_con(F, _, Na, Index) : cons(Identifier, has_con("weak", F, Na, Index));  %* c2
    sss *%
   cons(Identifier,tail(H,V)).

    bla%aa 
    %bb  
    :-%aa 
    %bb
     varies(X),
    #sum %aa 
    %bb
    { %aa 
    %bb
        2**(X-Z)%aa 
    %bb 
    :%aa 
    %bb
            Z = L+1..X,%aa 
    %bb
            not counter(1, Z); %aa 
    %bb
        1, M :
            excluded(M)
    } %aa 
    %bb
     >= %aa 
    %bb
     Y %aa 
    %bb
    ,models(Y).