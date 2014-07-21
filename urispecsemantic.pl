% uri syntax specification
% from RFC 3986
url(url(Scheme, Hierarchy, Query, Fragment)) -->
	scheme_name(Scheme),
	":",
	hierarchical_part(Hierarchy),
	query_part(Query),
	fragment_part(Fragment).

% to keep this short I'm only doing http.
% and not doing urlencoding
% and many other sins against the standard
scheme_name(http) -->
	"http".

hierarchical_part([path(Path) | Auth]) -->
	"//",
	authority(Auth),
	path(Path).

%%	authority(-Auth)// is det
%
%	binds an option list of credentials, domain and port
%
authority(Auth) -->
	opt_credentials(Cred),
	domain(Domain),
	opt_port(Port),
	{
	    append([Cred, [domain(Domain)], Port], Auth)
	}.

path([W | P]) -->
	"/",
	word(W),
	path(P).
path([]) --> "/".
path([]) --> [].

opt_credentials([user_name(UName), password(PW)]) -->
	word(UName),
	":",
	word(PW),
	"@".
opt_credentials([user_name(UName)]) -->
	word(UName),
	"@".
opt_credentials([]) --> [].

domain([TLD, Secondary | Tert] ) -->
	opt_tertiary_domain(Tert),
	word(Secondary),
	".",
	word(TLD).

opt_tertiary_domain([W | TD]) -->
	word(W),
	".",
	opt_tertiary_domain(TD).
opt_tertiary_domain([]) --> [].

opt_port([]) --> [].
opt_port([port(N)]) -->
	":",
	number(N).

query_part(QP) -->
	"?",
	query_pairs(QP).
query_part([]) --> [].

query_pairs([QP]) -->
	query_pair(QP).
query_pairs([QP | Rest]) -->
	query_pair(QP),
	"&",
	query_pairs(Rest).

query_pair(Key-Value) -->
	word(Key),
	"=",
	word(Value).

fragment_part([fragment(W)]) -->
	"#",
	word(W).
fragment_part([]) --> [].

number(N) -->
	digit(D),
	more_number(D, N).
more_number(D, N) -->
	number(R),
	{
	    N is D * 10 + R
	}.
more_number(D, D) --> [].

digit(D) -->
   [DC],
   {
       code_type(DC, digit(D))
   }.

valid_char(C) -->
	[C],
	{
	    code_type(C, alnum)
	}.
valid_char(C) -->
	[C],
	{
	    member(C, "!~^*()_-")
	}.


% word/1 will - an atom
word(A) -->
   wword(W),
   {
       atom_codes(A, W)
   }.

wword([C | W]) -->
	valid_char(C),
	more_word(W).
more_word([C |W]) -->
	valid_char(C),
	more_word(W).
more_word([]) --> [].

% demo post 7.0 backtick codes
parse_url(URL) :-
	phrase(url(URL), `http://google.com/path/to/something?x=3&y=2#frag`).


parse_bad_url(URL) :-
	phrase(url(URL), `http://googlecom/path/to/something?x=3&y=2#frag`).
