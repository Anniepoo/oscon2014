OSCON 2014
===========

Materials for the OSCON workshop 'SWI-Prolog for the Real World'

Speakrs
~~~~~~~

Anne Ogborn (annie66us@yahoo.com)

Description
~~~~~~~~~~~

Savvy functional programmers are discovering logic programming, and SWI-Prolog's vast niftiness. Come watch Annie run her debugger in reverse, directly execute syntax specifications, and lets the computer figure out it's own darn execution strategy. Be amazed as Annie constrains variables and shrinks her APIs. Ooh and Aah at the many libraries, nifty web framework and clean environment.


40-minute conference session

Topics: Emerging Languages

Abstract
~~~~~~~~

Most programmers encounter Prolog in a programming languages survey course, where the emphasis is on toy logic puzzles. Unfortunately, this introduction perpetuates a stereotype of Prolog as a special purpose inference engine rather than what it is, a powerful general purpose programming language well adapted to agile programming.

SWI-Prolog is a solid, reliable programming environment used in many mission critical high volume applications. This talk will concentrate first on concrete advantages of logic languages, and then on features of SWI-Prolog that would make it a great programming environment even if not for the
logic programming advantage.

Annie will demonstrate many of SWI-Prolog’s nifty features, including parsing as fundamental operation, constraint programming, and the metanifty web framework.

Version
~~~~~~~

7.1.17 or greater  -  note that this is the *development* release

We want to show the latest stuff!

Coverage
~~~~~~~~

Savvy functional programmers are discovering logic programming, and SWI-Prolog's vast niftiness. 

 * debugger retry
 * lack of control structures
 * direct grammar execution
 * computer derived execution strategy
 * constraint systems
 * multimodal - the API shrinker
 * DCG based web framework
 * IDE  (edit/1)


Homoiconicity

Statelessness

Nondeterminism
   - reduces code size
   - input and output aren't fixed, vastly reducing API size
         (consider append, which is also prefix, suffix, and member)
   - declarative code can just define the problem, not define the steps needed to reach it.
   - has-a and has-many are the same thing.
   - error handling 'built in'
   - lack of control structures encourage metaprogramming
   - Partial structures, generate and test

Knowledge and facts - Knowledge lets us:
   - handle complexly structured data, data with exceptions, general rules.
   - schemaless DB is effectively a base type.
   - structure data as you go
   - code isn't just convertable to data, code IS data.
   - case based reasoning reduces coupling.
   - 'family tree' type reasoning is powerful for real world tasks too.

matching based calls
   - Closer to a true abstract type system than multimethods
   - radical destructuring, 'coding in the head'
   - passing structured data means not needing to know format everywhere.
   - post object world.

good data types
    - structure-as-you-go data is a type, like lisp.
    - String and StringBuffer without the pain

parsing as a fundamental operation
   - death to XML
   - we don't need regex's

 
 
 Original Java User Group talk
 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



1.I'll show you through a bit of prolog code, then talk about why the paradigm's interesting.

Learning a 'weird' language teaches you more than learning a similar one. Prolog's got the cake for weird.
Prolog is the only commonly used logic programming language.

2. BRIEF intro to 
capitals start variables
lowercase starts atoms
unification, resolution, and backtracking.
Trace into cannibals2nocomment, quickly to canoeCarries


Nondeterminism
   - reduces code size
   - input and output aren't fixed, vastly reducing API size
         (consider append, which is also prefix, suffix, and member)
   - declarative code can just define the problem, not define the steps needed to reach it.
   - has-a and has-many are the same thing.
   - error handling 'built in'
   - lack of control structures encourage metaprogramming
   - Partial structures, generate and test

Knowledge and facts - Knowledge lets us:
   - handle complexly structured data, data with exceptions, general rules.
   - schemaless DB is effectively a base type.
   - structure data as you go
   - code isn't just convertable to data, code IS data.
   - case based reasoning reduces coupling.
   - 'family tree' type reasoning is powerful for real world tasks too.

matching based calls
   - Closer to a true abstract type system than multimethods
   - radical destructuring, 'coding in the head'
   - passing structured data means not needing to know format everywhere.
   - post object world.

good data types
    - structure-as-you-go data is a type, like lisp.
    - String and StringBuffer without the pain

parsing as a fundamental operation
   - death to XML
   - we don't need regex's


   
Session Start: Fri May 13 19:03:33 2011
Session Ident:##prolog
[19:03] ->> You joined channel ##prolog
[19:03] ->> Topic is: PROLOG => PROgrammation in LOGique: (wiki will be back soon) | SLOW MOTION CHANNEL (we all have jobs): Ask your question, then wait; check back often to see if anybody answered | Tutorial: Learn Prolog Now! <http://cs.union.edu/~striegnk/learn-prolog-now/>, <http://cs.union.edu/~striegnk/learn-prolog-now/html/prolog-notes.pdf>
[19:03] ->> Topic set by ski on 9/1/2009 6:18:07 PM
[19:03] ->> Channel Modes are: +nt 
[19:03] ->> Channel created on 8/27/2009 10:20:18 PM
[19:03] ->> Attempting to join #logicmoo
[19:04] <Anniepoo> I have to talk to a bunch of Java programmers about why Prolog is cool in a few days
[19:05] <Anniepoo> Wondering what I can say to boil it all down
[19:08] <ski> i'm not sure, but multi-modedness (aka reversability) , constraints , automatic search with backtracking are cool
[19:09] <cehteh> Anniepoo: "No NULL pointer exceptions"
[19:09] <Anniepoo> I have reversability on the list, but could use a better theoretical grasp of what 'multi-moded' means in this context
[19:09] <Anniepoo> LOL
[19:10] <Anniepoo> @Cehteh, I have 'less code for edge cases'
[19:10] <Anniepoo> and that'll be the first thing I mention
[19:10] <Anniepoo> 'knowledge, not just facts'
[19:11] <Anniepoo> I'm making the argument that many 'real programmer' programming tasks end up looking like small AI tasks
[19:11] <Anniepoo> like wiring up all the parts of a GUI.
[19:13] <Anniepoo> is something you can do with a small expert system.
[19:27] <ski> maybe also parsing ? .. there's DCG, though typically if you want efficient parsing, you have to do some lay-on-hands (remove left-recursion and needless ambiguity (lack of indexing)) -- it's easier than doing recursive descent in Java anyway; i suppose it depends on whether it is easier than yacc-style parsing
[19:27] <ski> multi-modedness just means that multiple modes of a predicate is supported
[19:27] <Anniepoo> certainly I'd include DCG's
[19:28] <Anniepoo> ski, so you mean the foo(a(X)) :-- blah blah.
[19:28] <Anniepoo> foo(b(X)) :-- blort blort.
[19:29] <Anniepoo> s/:--/:-/
[19:29] <Anniepoo> it's good to have a name for that, thanks
[19:29] <ski> in terms of Pascal/Ada, you can have procedures `foo_to_bar(in,out)' and `foo_from_bar(out,in)' (not exact syntax), while in Prolog you can merge those into a single predicate foo_bar/2 with two modes (+,-),(-,+)
[19:29] <Anniepoo> ah!
[19:29] <Anniepoo> ok
[19:29] <ski> often you can even use the exact same code for the different modes -- sometimes you need to special case some modes
[19:29] <Anniepoo> yes, that's great
[19:30] <ski> so, in the simple cases, one often calls this "reversability"
[19:30] <ski> in more complex cases, there's more "directions" than "forward" and "backward", so "multi-modedness" is a better term then
[19:31] <Anniepoo> 8cD  Yes - and down the line this vastly reduces the size of API's
[19:32] <Anniepoo> I note that I'm not that experienced a Prolog programmer, but I have about 33% of the prolog API's memorized.
[19:32] <Anniepoo> I wrote Java for 10 years. I don't have anywhere NEAR that fraction memorized
[19:32] <ski> append/3 would replace `(++)',`isPrefix',`stripPrefix',`isSuffix',`stripSuffix',`splits' in Haskell
[19:33] <Anniepoo> (arguably there are more libraries to do things in Java, but I mean core libs)
[19:33] <ski> (`stripSuffix' and `splits' doesn't exist, but at least `stripSuffix' ought to exist)
[19:33] <ski> (or s/isPrefix/isPrefixOf/,s/isSuffix/isSuffixOf/)
[19:33] <Anniepoo> and you have to remember all those names
[19:34] <ski> of course, many predicates will have just one main mode (with a few implied modes)
[19:35] <Anniepoo> Prolog is closer than Clojure to a true abstract type system
[19:35] <ski> but often enough you get useful multi-moded predicates where you actually use more than one main mode
[19:36] <Anniepoo> Clojure has to sort of jaw in control structures. Prolog truly doesn
[19:36] <Anniepoo> doesn't need them most of the time
[19:36] <Anniepoo> (admittedly the repeat fail loop looks like a jawed in control structure)
[19:36] <ski> (similarly, many predicates are deterministic (or semi-deterministic), so you don't always use the backtracking, but often enough it's useful -- depending on application of course)
[19:37] <Anniepoo> backtracking's useful anywhere you'd iterate in an imperative language
[19:38] <Anniepoo> (though many times iteration is a crutch for the lack of map-reduce)
[19:38] <ski> btw, i usually try to rewrite failure-driven loops from `( generate(X),side_effect(X),fail ; true )' to `forall( generate(X) , side_effect(X) )'
[19:39] <Anniepoo> ok, forall's an explicit iterator
[19:54] * augur (~augur@208-59-167-26.c3-0.slvr-ubr1.lnh-slvr.md.cable.rcn.com) Quit ( Remote host closed the connection )
[19:54] <Anniepoo> .
[19:54] <ski> i suppose maplist/[2,3,4] and include/3,exclude/3,partition/[4,5] can also be seen as (external) iterators
[19:55] <Anniepoo> sure, those are all part of 'map' in the map-reduce paradigm
[19:56] <ski> also there's do/2 at <http://eclipseclp.org/doc/userman/umsroot023.html>,<http://eclipseclp.org/doc/bips/kernel/control/do-2.html> in ECLiPSe Prolog <http://eclipseclp.org/>
[19:57] <Anniepoo> swipl puts all those in a little library, called the 'apply predicates library'
[19:57] <Anniepoo> 11.2 library(apply):  Apply predicates on a list
[19:57] <ski> (that one is more similar to various looping macros in lisp systems, such as <http://mumble.net/~campbell/scheme/foof-loop.txt>)
[20:02] <Anniepoo> 8cD  I love ##prolog - it's the intellectual equivilent of drinking from a firehose.
