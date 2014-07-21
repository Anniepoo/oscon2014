/*
         much shorter quarreling children

         16 children are to be seated in a
         4 x 4 array of chairs.

         the children are 8 girls (numbered 1..8) and
         8 boys (numbered 9..16).

     1,3,5,8 think boys are ucky
         9,10,11,14 think girls are gross

         these pairs are enemies

         [[1,2], [4,6], [4,7], [4, 9],[9,11], [12, 14], [14,16]]

 */
:- use_module(library(clpfd)).

length_(Length, List) :- length(List, Length).

child_row(X) :- X ins 1..16 .

ww(X) :-
        write(X),
        write('/').

print_row(Row) :-
        maplist(ww, Row),
        nl.

children(Class) :-
        length(Class, 4),
        maplist(length_(4), Class),
        maplist(child_row , Class),
        maplist(row_compatible, Class),
        transpose(Class, TransClass),
        maplist(row_compatible, TransClass),
        flatten(Class, FlatClass),
        all_different(FlatClass),
        maplist(label, Class),
        maplist(print_row, Class).

row_compatible([A,B,C,D]) :-
        compatible(A, B),
        compatible(B, C),
        compatible(C, D).

compatible(A, B) :-
        not_enemy(A, B),
        not_enemy(B, A),
        sex_compatible(A, B),
        sex_compatible(B, A).

not_enemy(A, B) :-
        NotA #\= A #\/ NotB #\= B,
        tuples_in([[NotA, NotB]],
                    [[1,2], [4,6], [4,7], [4, 9],[9,11], [12, 14], [14,16]]).

sex_compatible(A, B) :-
        A in 1\/3\/5\/8 #==> B #=< 8,
        A in  9..11\/14 #==> B #> 8.
