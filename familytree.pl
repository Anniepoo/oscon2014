:- module(familytree, [
	      male/1,
	      female/1,
	      parent/2,
	      father/2,
	      mother/2,
	      son/2,
	      daughter/2,
	      grandfather/2,
	      aunt/2,
	      uncle/2,
	      sister/2,
	      cousin/2,
	      ancestor/2,
	      brother/2
	  ]).


:- discontiguous male/1, female/1, parent/2.

% The good for nothing men in my family
% names lowercase to make them atoms. I could
% have used 'Dicky' and so on.
male(dicky).
male(randy).
male(mike).
male(don).
male(elmer).
male(blair).
male(grandpa_boger).

% and the sainted women who put up with them
female(anne).
female(rosie).
female(esther).
female(mildred).
female(greatgramma).
female(edith).

% The Ogborn Begats
parent(don, randy).
parent(don, mike).
parent(don, anne).
parent(rosie, randy).
parent(rosie, mike).
parent(rosie, anne).
parent(elmer, don).
parent(mildred, don).
parent(esther, rosie).
parent(esther, dicky).
parent(greatgramma, esther).
parent(randy, blair).
parent(grandpa_boger, rosie).
parent(esther, edith).

father(Dad, Kid) :-
	male(Dad),
	parent(Dad, Kid).
mother(Mom, Kid) :-
	female(Mom),
	parent(Mom, Kid).
son(X, Y) :-
	male(X),
	parent(Y, X).
daughter(X, Y) :-
	female(X),
	parent(Y, X).
grandfather(GrandDad, GrandKid) :-
	male(GrandDad),
	parent(GrandDad, Somebody),
	parent(Somebody, GrandKid).
aunt(Aunt, Kid) :-
	sister(Aunt, Dad),
	father(Dad, Kid).
aunt(Aunt, Kid) :-
	sister(Aunt, Mom),
	mother(Mom, Kid).
sister(Sister, Sibling) :-
	female(Sister),
	parent(Par, Sister),
	parent(Par, Sibling),
	Sister \= Sibling.
uncle(X, Y) :-
	brother(X, Par),
	parent(Par, Y).
cousin(X, Y) :-
	uncle(Unc ,  X),
	father(Unc, Y).
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :-
	parent(X, Somebody),
	ancestor(Somebody, Y).
brother(X, Y) :-
	male(X),
	parent(Somebody, X),
	parent(Somebody, Y),
	X \= Y.

:- user:use_module(familytree).
