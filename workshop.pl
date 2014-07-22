:- module(workshop, [read_talk/0]).
/** <module> Orientation page for the workshop

*/

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
% html_resource inclusion
:- use_module(library(http/html_head)).
:- use_module(library(http/http_parameters)).
:- use_module(library(dcg/basics)).

:- dynamic user:section_done/1.

:- http_handler(root(sectiondone), done_handler, [id(sectiondone)]).

talk_length(40).

%%	done_handler(+Request:request) is det
%
%	handle the 'Done' button
%
done_handler(Request) :-
	http_parameters(Request,
	     [
		    section(Name, [])
	     ]),
	asserta(user:section_done(Name)),
	http_redirect(moved_temporary, location_by_id(workshop), Request).

:- http_handler(root(sectionundone), undone_handler, [id(sectionundone)]).

%%	undone_handler(+Request:request) is det
%
%	handle the 'Undo' button
%
undone_handler(Request) :-
	http_parameters(Request,
	     [
		    section(Name, [])
	     ]),
	retractall(user:section_done(Name)),
	http_redirect(moved_temporary, location_by_id(workshop), Request).

:- http_handler(root(restart), restart_handler, [id(restart)]).

restart_handler(Request) :-
	talk_length(TL),
	 http_parameters(Request,
		[ restart(Restart, [between(0,TL), default(-1)])
		]),
	restart_time(Restart),
	http_redirect(moved_temporary, location_by_id(workshop), Request).



:- http_handler(root(workshop), workshop_page, [id(workshop)]).

/**  workshop_page(+Request:request)// is det

    handler pred for the page with the workshop lesson plan

*/
workshop_page(_Request) :-
	reply_html_page(
	    title('OSCON 2014'),
	    [
		\html_requires('/help/source/res/pldoc.css'),
		\html_requires('/help/source/res/pldoc.js'),
		\html_requires('/f/workshop.css'),
		\start_form,
		hr([]),
		\sections
	    ]).

start_form -->
	{
             talk_length(TL)
        },
	html([
	    form(action('/restart'), [
		 label(for(restart), 'Restart this many minutes into workshop:'),
		 input([type(number), maxlength(3), min(0), max(TL),
			name(restart), value(0)], []),
		 input([type(submit), name(ok), value(restart)], [])
		 ])
	]).

%%	sections// is det
%
%	generates the list of sections in the workshop
%
sections -->
	{
	    time_remaining(Time),
            talk_length(TL),
	    MissionTime is floor(TL - Time),
            bagof(Name, A^B^section_head(Name, A, B), Names),
            time_restricted_unfinished_sections(Time, Names, WeWillCover, _),
            !
        },
	html([p([\wall_time, ' time remains ', Time])]),
	section(MissionTime, Names, WeWillCover, false).


%%	section(+StartAt:time, +Names:list, +WeWillCover:list,
%	+CurrentFound:atom)// is det
%
%	generate a set of sections recursively
%
%	@arg StartAt Mission time at which we start section
%	@arg Names   List of sections
%	@arg WeWillCover  List of those sections we'll cover
%			  we might run out of time
%	@arg CurrentFound if false we haven't found the current section
%	                  if true, we already have
%
% we will do this case
section(StartAt, [Name | T], WeWillCover, CurrentFound) -->
	{
	   member(Name, WeWillCover),
	   section_head(Name, Label, File),
           mission_wall(StartAt, Wall),
	   timing(Name, Timing),
           NextStartsAt is StartAt + Timing,
           atomic_list_concat(['HTTPrequest(\'/help/source/edit?file=', File, '\')'], AFile),
           (   bagof(Desc, subsection(Name, Desc), Subsections) ->
               true
           ;
               Subsections = []
           ),
           (   CurrentFound = false ->
	       SectionClass = [section, willdo, current]
	   ;
	       SectionClass = [section, willdo]
	   )
        },
	html(div(class(SectionClass), [
		     p([\done_section(Name),
			span(class(wall), Wall),
			span(class(sectionlabel), Label),
			Timing, ' mins',
			a([class(editlink), href('#'), onClick(AFile)],
			  'See The File')
		       ]),
		     ul(\subsections(Subsections)),
		     \section_images(Name, [])
		 ])
	),
	section(NextStartsAt, T, WeWillCover, true).

% done case
section(StartsAt, [Name | T], WeWillCover, CurrentFound) -->
	{
           user:section_done(Name),
	   section_head(Name, Label, File),
	   timing(Name, Timing),
           atomic_list_concat(['HTTPrequest(\'/help/source/edit?file=', File, '\')'], AFile),
           (   bagof(Desc, subsection(Name, Desc), Subsections) ->
               true;
               Subsections = []
           )
        },
	html(div(class([section, done]), [
		     p([\done_section(Name),
			span(class([sectionlabel, done]), Label),
			     Timing, ' mins',
			     a([class(editlink), href('#'), onClick(AFile)], 'See The File')
			    ]),
		     ul(\subsections(Subsections)),
		     \section_images(Name, [])
		 ])
	),
	section(StartsAt, T, WeWillCover, CurrentFound).

% won't do case
section(StartsAt, [Name | T], WeWillCover, CurrentFound) -->
	{
	   section_head(Name, Label, File),
	   timing(Name, Timing),
           atomic_list_concat(['HTTPrequest(\'/help/source/edit?file=', File, '\')'], AFile),
           (   bagof(Desc, subsection(Name, Desc), Subsections) ->
               true;
               Subsections = []
           )
        },
	html(div(class([section, notime]), [
		     p([\done_section(Name),
			span(class([sectionlabel, notime]), Label),
			     Timing, ' mins',
			     a([class(editlink), href('#'), onClick(AFile)], 'See The File')
			    ]),
		     ul(\subsections(Subsections)),
		     \section_images(Name, [])
		 ])
	),
	section(StartsAt, T, WeWillCover, CurrentFound).
section(_, [], _, _) --> [].

%%	subsections(+List:list)// is det
%
%	generates a list of the subsections from the arglist
%
subsections([]) --> [].
subsections([H|T]) -->
	html(li(H)),
	subsections(T).

%%	section_images(+Name:atom, +Done:list)// is det
%
section_images(Name, Done) -->
	{
	   % suprise - we're in Prolog, this is a loop!
           section_image(Name, File, Alt),
	   \+ member(File, Done)
        },
	html(\img_fix([src('/f/'+File), class(illustration), alt(Alt)], [])),
	section_images(Name, [File | Done]).
section_images(_, _) --> [].

show_jan_bug.

img_fix(Attribs, Contents) -->
	{
           show_jan_bug
        },
	html(img(Attribs, Contents)).

img_fix(Attribs, Contents) -->
	{
    % Hack! html_write:attributes poorly exposed
	   member(src(SrcPath+SrcFile), Attribs),
           random(N),
	   format(atom(UniqSrc), '~w~w?r=~w', [SrcPath, SrcFile, N]),
	   select(src(_), Attribs, src(UniqSrc), NewAttribs)
        },
	    html(img(NewAttribs, Contents)).
img_fix(Attribs, Contents) -->
	{
            member(src(Src), Attribs),
            random(N),
	    format(atom(UniqSrc), '~w?r=~w', [Src, N]),
	    select(src(Src), Attribs, src(UniqSrc), NewAttribs)
        },
	    html(img(NewAttribs, Contents)).
img_fix(Attribs, Contents) -->
	    html(img(Attribs, Contents)).

done_section(Name) -->
	{
	  \+ user:section_done(Name)
        },
	html([
	    form([class(doneform), action('/sectiondone')], [
		 input([type(hidden), name(section), value(Name)], []),
		 input([type(submit), name(ok), value(done)], [])
		 ])
	]).
done_section(Name) -->
	{
	  user:section_done(Name)
        },
	html([
	    form([class(doneform), action('/sectionundone')], [
		 input([type(hidden), name(section), value(Name)], []),
		 input([type(submit), name(ok), value('undo')], [])
		 ])
	]).


mission_start(5).

%%	mission_wall(+T:number, +Wall:atom)// is det
%
%	convert time in mission time (minutes since start of workshop)
%	to wall clock time starting at mission_start
%
mission_wall(T, Wall) :-
	mission_start(Start),
	Hours is T // 60 + Start,
	Mins is T mod 60,
	format(atom(Wall), '~w:~|~`0t~d~2+', [Hours, Mins]).

%%	wall_time// is det
%
%	inclusion displaying wall time
wall_time -->
	{
	     get_time(Now),
             latest_start_time(Start),
             Mission is floor(Now - Start) // 60,
	     mission_wall(Mission, Wall)
        },
	html([
	    span(Wall)
	]).

%%	time_restricted_unfinished_sections(+TimeAvailable:number,
%	+Sections:list, -BestSections:list, -Score:number) is det
%
%	from among Sections select BestSections s.t. Score is maximized
%	and TimeAvailable is not exceeded and we aren't doing any that
%	have already been done per user:section_done
%
time_restricted_unfinished_sections(TimeAvailable, Sections, BestSections, Score) :-
	not_done_sections(Sections, NotDoneSections),
	time_restricted_sections(TimeAvailable, NotDoneSections, BestSections, Score).

not_done_sections([], []).
not_done_sections([H|T], NotDoneTail) :-
	user:section_done(H),
	!,
	not_done_sections(T, NotDoneTail).
not_done_sections([H|T], [H|NotDoneTail]) :-
	not_done_sections(T, NotDoneTail).


%%	time_restricted_sections(+TimeAvailable:number, +Sections:list,
%	-BestSections:list, -Score:number) is det
%
%	from among Sections select BestSections s.t. Score is maximized
%	and TimeAvailable is not exceeded
%

% base case - no more sections
time_restricted_sections(_, [], [], 0).

% we have enough time to do all the rest -
% checking for this speeds up algorithm
time_restricted_sections(TimeAvailable, Sections, Sections, TotalScore) :-
	total_time(Sections, 0, TotalTime),
	TotalTime =< TimeAvailable,
	total_score(Sections, 0, TotalScore).

% not enough time to do this section by itself
time_restricted_sections(TimeAvailable, [H|T], BestSections, Score) :-
	timing(H, ThisTime),
	TimeAvailable < ThisTime, !,
	time_restricted_sections(TimeAvailable, T, BestSections, Score).

% otherwise try with and without H and choose the better
time_restricted_sections(TimeAvailable, [H|T], BestSections, Score) :-
	timing(H, ThisTime),
	priority(H, ThisScore),
	RemainingTime is TimeAvailable - ThisTime,
	time_restricted_sections(TimeAvailable, T, BestNoSections, NoScore),
	time_restricted_sections(RemainingTime, T, BestYesSections, YesTScore),
	YesScore is YesTScore + ThisScore,
	(   YesScore > NoScore
	->
	    Score = YesScore,
	    BestSections = [H | BestYesSections]
	;
	    Score = NoScore,
	    BestSections = BestNoSections
	).

%%	total_time(+List:list, +Seed:number, -Time:number) is det
%
%	 sum the times associated with the names in List,
%	 Usually pass in 0 as Seed
%
total_time([], T, T).
total_time([H|T], InTime, OutTime) :-
	timing(H, ThisTime),
	NT is InTime + ThisTime,
	total_time(T, NT, OutTime).

%%	total_score(+List:list, +Seed:number, -Score:number) is det
%
%	@arg List   list of section names
%	@arg Seed   seed to start from  (usually 0)
%	@arg Score  sum of the priorities of items in list
%
total_score([], S, S).
total_score([H|T], InScore, OutScore) :-
	priority(H, Score),
	NS is Score + InScore,
	total_score(T, NS, OutScore).

:- dynamic user:start_time/1.

user:start_time(1406075400).

%%	restart_time(+Time:int) is det
%
%	if -1 do nothing
%	otherwise restart timer at this many minutes into talk
%
restart_time(-1).
restart_time(T) :-
	get_time(Now),
	Start is Now - T * 60,
	retractall(user:start_time(_)),
	asserta(user:start_time(Start)).
restart_time(_).

%%	time_remaining(-Time) is det
%
%	time remaining in the workshop in minutes
%	prior to start reports
%	after end reports 0
%
time_remaining(Time) :-
	get_time(Now),
	latest_start_time(Start),
	Now < Start,
	talk_length(Time).
time_remaining(Time) :-
	get_time(Now),
	latest_start_time(Start),
	talk_length(TL),
	End is Start + TL * 60,
	Now > End,
	Time = 0.
time_remaining(Time) :-
	get_time(Now),
	latest_start_time(Start),
	talk_length(TL),
	Time is (Start + TL * 60 - Now) / 60.

%%	latest_start_time(-Time:int) is det
%
%	return only the most recently set time
%
latest_start_time(Time) :-
	user:start_time(Time), !.

:- dynamic
	section_head/3,
	timing/2,
	priority/2,
	subsection/2,
	section_image/3.

:- initialization read_talk.

read_talk :-
	retractall(workshop:section_head(_, _, _)),
	retractall(workshop:timing(_, _)),
	retractall(workshop:priority(_, _)),
	retractall(workshop:subsection(_, _)),
	retractall(workshop:section_image(_, _, _)),
	phrase_from_file(lecture(none), 'oscontalk4.txt').

lecture(_) --> eos.
lecture(Section) --> blank, lecture(Section).
lecture(_) -->
	"*",!,
	lazy_list_location(L),
	string_without("\n", RestOfLine),
	eol,
	{
	    gensym(sect, NewSection),
	    atom_codes(A, RestOfLine),
	    assertz(section_head(NewSection, A, 'workshop.pl')),
	    format('~w section ====== ~w~n', [L, A])
	},
	lecture(NewSection).
lecture(Section) -->
	"^t=",!,
	lazy_list_location(L),
	integer(Time),
	whites,
	eol,
	{
	   assertz(timing(Section, Time)),
	   format('~w ~w time = ~w~n', [L, Section, Time])
	},
	lecture(Section).
lecture(Section) -->
	"^p=",!,
	lazy_list_location(L),
	integer(Priority),
	whites,
	eol,
	{
	   assertz(priority(Section, Priority)),
	   format('~w ~w priority = ~w~n', [L, Section, Priority])
	},
	lecture(Section).
lecture(Section) -->
	string_without("\n", SubSect),
	lazy_list_location(L),
	eol,
	{
	    atom_codes(A, SubSect),
	    assertz(subsection(Section, A)),
	    format('~w subsection ~w~n', [L, A])
	},
	lecture(Section).

eol --> "\n".
eol --> eos.

/*
%%	section_head(-Name:atom, -Desc:atom, -File:atom) is det
%
section_head(intro, 'Welcome', 'cannibalsandmissionaries.pl').
timing(intro, 3).
priority(intro, 100).
subsection(intro, 'How many people here are programmers?').
subsection(intro, 'If you learned Prolog in college').
subsection(intro, 'Real language.').
subsection(intro, 'There are a lot of large commercial sites running Prolog, notably big chunks of the phone system and the largest trade clearing house in New Zealand. I’ve personally written web applications in SWI-Prolog, as well as batch scripts, systems to manipulate RDF triples, a web spider that looks for patterns, an AI system that runs a bunch of virtual people in a virtual world, a mobile game, and pretty much everything else I could get away with writing in Prolog in the last 5 years or so.').
subsection(intro, 'weird. no control structures, is not object oriented, executes backwards, and you don\'t have to specify the order of execution. All this is neat!').
*/

