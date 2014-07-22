:- module(load, []).
/** <module> Load this to start the production environment

*/

% A bit of possibly excessive abstraction, we load the
% server but don't run it in strangeloop.pl
:-use_module(givetalk).
% make sure the handlers get loaded
:- ensure_loaded(simple_handlers).
:- ensure_loaded(html_handlers).
:- ensure_loaded(fancy_handlers).
:- ensure_loaded(workshop).
:- ensure_loaded(cannibalsandmissionaries).
:- ensure_loaded(urispec).
:- ensure_loaded(quarreling).

