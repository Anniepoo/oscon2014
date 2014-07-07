:- module(cool_facts, [
	      cause/2
	  ]).

:- discontiguous meta_cool/1, cause/2.

meta_cool(cause(feature, X)) :- valid_cool(X).
meta_cool(cause(feature, avoids(X))) :- valid_cool(X).

valid_cool(X) :- atom(X).
valid_cool(avoids(X)) :- valid_cool(X).
valid_cool(cause(X)) :- cause(X, Y), valid_cool(Y).
valid_cool(antipattern(X)) :- atom(X).


cause(in_memory_db,
      'accesses to local data structures are effectively accesses to the backing store').
cause(in_memory_db,
      'ease of replacing the backing store').
cause(in_memory_db,
      'single orm-ish point').
cause(in_memory_db,
       avoids(antipattern('stuffing slow changing data into tables (PHP OS projects syndrome)'))).

cause(iterator_on_structure,
      'eliminate the noise and bugs of explicit iteration').
cause(iterator_on_structure,
      'automagic looping over all the rows in html').
cause(backtracking,
      cause(iterator_on_structure)).
cause(backtracking,
      cause(no_control_structures)).
cause(no_control_structures, 'Code is much more readable without control structures. There\'s no nesting').

cause(multimodality, cause(small_api)).
cause(small_api,
      'Less time learning API').
cause(small_api,
      'Higher probability of knowing the API you need').
cause(small_api,
      'Code is more readable since you\'re looking at APIs you understand').
cause(multimodality, 'freebie code when you use a mode you didn\'t write that for').

cause(unification, destructuring).
cause(unification, 'DSLs everywhere').
cause(unification, 'more flexible arguments, not more APIs').
cause(unification, 'boundary case handling').
cause(unification, 'handle single cases - reduces errors').
cause(unification, 'case based reasoning').

cause(declarative, cause(auto_execution_strategy)).
cause(declarative, 'small expert systems everywhere').

cause(homoiconic, cause(serializable)).
cause(homoiconic, cause(ez_introspect)).
cause(ez_introspect, 'good IDE tools').
cause(ez_introspect, cause(term_expansion)).
cause(term_expansion, 'Yet more DSL\s - term_expansion and declaratives FTW').
cause(no_control_structures, cause(ez_introspect)).  % NOTE multiple

cause(serializable, 'Easy to persist').
cause(serializable, 'Easy to distribute').
cause(serializable, pengines).



