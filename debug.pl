:- module(debug, []).
/** <module> Dev mode starter file

    Consult this file to start the system in dev mode

*/

% Make it easier to read 'codes' style strings
:- portray_text(true).

% pldoc is SWI-Prolog's equivilent of doxygen
% pldoc is served at / by default. We need to
% move pldoc's URI path so it doesn't interfere with the main
% application. Each SWI-Prolog process has a single global
% URI space for dispatch.

% abstract URI paths allow us to disconnect the served URI path
% from the name we refer to it by. By default, pldoc('foo/bar') maps
% to /foo/bar. We'll move pldoc to /help/source, so pldoc('foo/bar')
% will serve from /help/source/foo/bar

% First we import the abstract path stuff
:- use_module(library(http/http_path)).

% Now we define a new clause for the *multifile* predicate
% http:location/3. Default priority is 0, we use a higher
% priority to override pldoc's default location
% We define pldoc in terms of [system defined] root.
%
http:location(pldoc, root('help/source'), [priority(10)]).

% load our application server
% We start it first
% so it doesn't collide with pldoc
:-ensure_loaded(load).

% we don't actually start the server ourselves, in dev mode
% we piggyback on the pldoc server
%
% :- strangeloop_server.

% Now we can start pldoc. This starts our application server
% as well, a workaround for one server per process
:- doc_server(7777).

% Nice thing about SWI-Prolog, the interface to most
% development environment is fairly simple, so it's practical
% to set your environment for your own convenience. Here, we
% launch our first handler
:-www_open_url('http://localhost:7777/').

% and our pldoc home page
:-www_open_url('http://localhost:7777/help/source/').

% and the workshop page
:-www_open_url('http://localhost:7777/workshop').


% and bring up a module in the editor
:- edit('debug.pl').