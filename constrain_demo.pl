:- module(constrain_demo, [constrain_demo/1]).
:- use_module(library(clpfd)).

constrain_demo(X) :-
	X in 1..5,
	foo(X),
	label([X]).

foo(X) :- X in 3..7 .

