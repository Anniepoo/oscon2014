:- module(urispec, [url//0]).

% demo
parse_url :- phrase(url, `http://google.com/path/to/something?x=3&y=2#frag`).

parse_bad_url :- phrase(url, `htp://google.com/path/to/something?x=3&y=2#frag`).

% uri syntax specification
% from RFC 3986
url -->
	scheme_name,
	":",
	hierarchical_part,
	query_part,
	fragment_part.

% to keep this short I'm only doing http.
% and not doing urlencoding
% and probably other sins
scheme_name -->
	"http".

hierarchical_part -->
	"//",
	authority,
	path.

authority -->
	opt_credentials,
	domain,
	opt_port.

path -->
	"/",
	word,
	path.
path --> "/".
path --> [].

opt_credentials -->
	word,
	":",
	word,
	"@".
opt_credentials -->
	word,
	"@".
opt_credentials --> [].

domain -->
	opt_tertiary_domain,
	word,
	".",
	word.

opt_tertiary_domain -->
	word,
	".".
opt_tertiary_domain --> [].

opt_port --> [].
opt_port -->
	":",
	number.

query_part -->
	"?",
	query_pairs.
query_part --> [].

query_pairs -->
	query_pair.
query_pairs -->
	query_pair,
	"&",
	query_pairs.

query_pair -->
	word,
	"=",
	word.

fragment_part -->
	"#",
	word.
fragment_part --> [].

number -->
	digit,
	more_number.
more_number -->
	number.
more_number --> [].

digit --> "0"| "1"| "2"| "3"| "4"| "5"| "6"| "7"| "8"| "9".

valid_char --> "1"| "2"| "3"| "4"| "5"| "6"| "7"| "8"| "9"| "0"|
"!"| "~"| "^"| "*"| "("| ")"| "_"| "-"| "q"| "w"| "e"| "r"| "t"|
"y"| "u"| "i"| "o"| "p"| "a"| "s"| "d"| "f"| "g"| "h"| "j"| "k"|
"l"| "z"| "x"| "c"| "v"| "b"| "n"| "m"| "Q"| "W"| "E"| "R"| "T"|
"Y"| "U"| "I"| "O"| "P"| "A"| "S"| "D"| "F"| "G"| "H"| "J"| "K"|
"L"| "Z"| "X"| "C"| "V"| "B"| "N"| "M".

word -->
	valid_char,
	more_word.
more_word -->
	word.
more_word --> [].

