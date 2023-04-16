% A grammar that covers most of the examples in COMP3411 lectures
% lines 4-153 were already provided by course staff

:- dynamic(history/1).

sentence(VP) --> noun_phrase(Number, Actor), verb_phrase(Actor, Number, VP).

noun_phrase(plural, set(NP1, NP2)) --> np1(_, NP1), [and], noun_phrase(_, NP2).
noun_phrase(Number, NP1) --> np1(Number, NP1).

np1(Number, thing(Noun, Properties)) -->
	determiner(Number, _),
	adjp(Properties),
	noun(Number, Noun).
np1(Number, thing(Noun, [PP | Properties])) -->
	determiner(Number, _),
	adjp(Properties),
	noun(Number, Noun),
	pp(Number, PP).
np1(Number, thing(Name, [])) -->
	proper_noun(Number, _, Name).
np1(Number, personal(Pro)) -->
	pronoun(Number, _, Pro).
np1(Number1, possessive(Pos, NP)) -->
	possessive_pronoun(Number1, _, Pos), noun_phrase(_, NP).
np1(Number, object(Noun)) -->
	num(Number), noun(Number, Noun).

adjp([Adj]) --> adjective(Adj).
adjp([]) --> [].

verb_phrase(Actor, Number, event(V, [actor(Actor) | Adv])) -->
	verb(Number, V),
	adverb(Adv).
verb_phrase(Actor, Number, event(V, [actor(Actor), object(NP) | Adv])) -->
	verb(Number, V),
	noun_phrase(_, NP),
	adverb(Adv).
verb_phrase(Actor, Number, event(V, [actor(Actor), object(NP), PP])) -->
	verb(Number, V),
	noun_phrase(_, NP),
	pp(Number, PP).
verb_phrase(Actor, Number, event(V, [actor(Actor), PP])) -->
	verb(Number, V),
	pp(_, PP).

pp(_, PP) --> prep(NP, PP), noun_phrase(_, NP).

% The next set of rules represent the lexicon

prep(NP, object(NP)) --> [of].
prep(NP, object(NP)) --> [to].
prep(NP, instrument(NP)) --> [with].
prep(NP, object(NP)) --> [in].
prep(NP, object(NP)) --> [for].

determiner(singular, det(a)) --> [a].
determiner(_, det(the)) --> [the].
determiner(plural, det(those)) --> [those].
determiner(_, _) --> [].

pronoun(singular, masculine, he) --> [he].
pronoun(singular, feminine, she) --> [she].
pronoun(singular, neutral, that) --> [that].
pronoun(plural, neutral, those) --> [those].
pronoun(singular, neutral, Pro) --> [Pro], {member(Pro, [i, someone, it])}.
pronoun(plural, neutral, Pro) --> [Pro], {member(Pro, [they, some])}.

possessive_pronoun(singular, masculine, his) --> [his].
possessive_pronoun(singular, feminine, her) --> [her].

prep(of) --> [of].
prep(to) --> [to].
prep(with) --> [with].
prep(in) --> [in].
prep(for) --> [for].

num(singular) --> [one].
num(plural) --> [two];[three];[four];[five];[six];[seven];[eight];[nine];[ten].

noun(singular, Noun) --> [Noun], {thing(Noun, Props), member(number(singular), Props)}.
noun(plural, Noun) --> [Noun], {thing(Noun, Props), member(number(plural), Props)}.

proper_noun(singular, Gender, Name) -->
	[Name],
	{
		thing(Name, Props), member(isa(person), Props), member(gender(Gender), Props)
	}.
proper_noun(singular, neutral, france) --> [france].

adjective(prop(Adj)) --> [Adj], {member(Adj, [red,green,blue])}.

verb(_, Verb) --> [Verb], {member(Verb, [lost,found,did,gave,looked,saw,forgot,is])}.
verb(singular, Verb) --> [Verb], {member(Verb, [scares,hates])}.
verb(plural, Verb) --> [Verb], {member(Verb, [scare,hate])}.

adverb([adv(too)]) --> [too].
adverb([]) --> [].

% You may chose to use these items in the database to provide another way
% of capturing an objects properties.

thing(john, [isa(person), gender(masculine), number(singular)]).
thing(sam, [isa(person), gender(masculine), number(singular)]).
thing(bill, [isa(person), gender(masculine), number(singular)]).
thing(jack, [isa(person), gender(masculine), number(singular)]).
thing(monet, [isa(person), gender(masculine), number(singular)]).

thing(mary, [isa(person), gender(feminine), number(singular)]).
thing(annie, [isa(person), gender(feminine), number(singular)]).
thing(sue, [isa(person), gender(feminine), number(singular)]).
thing(jill, [isa(person), gender(feminine), number(singular)]).

thing(wallet, [isa(physical_object), gender(neutral), number(singular)]).
thing(car, [isa(physical_object), gender(neutral), number(singular)]).
thing(book, [isa(physical_object), gender(neutral), number(singular)]).
thing(telescope, [isa(physical_object), gender(neutral), number(singular)]).
thing(pen, [isa(physical_object), gender(neutral), number(singular)]).
thing(pencil, [isa(physical_object), gender(neutral), number(singular)]).
thing(cat, [isa(physical_object), gender(neutral), number(singular)]).
thing(mouse, [isa(physical_object), gender(neutral), number(singular)]).
thing(man, [isa(physical_object), gender(neutral), number(singular)]).
thing(bear, [isa(physical_object), gender(neutral), number(singular)]).

thing(cats, [isa(physical_object), gender(neutral), number(plural)]).
thing(mice, [isa(physical_object), gender(neutral), number(plural)]).
thing(men, [isa(physical_object), gender(neutral), number(plural)]).
thing(bears, [isa(physical_object), gender(neutral), number(plural)]).

thing(capital, [isa(abstract_object), gender(neutral), number(singular)]).

thing(france, [isa(place), gender(neutral), number(singular)]).

event(lost, [actor(_), object(_), tense(past)]).
event(found, [actor(_), object(_), tense(past)]).
event(saw, [actor(_), object(_), tense(past)]).
event(forgot, [actor(_), object(_), tense(past)]).
event(scares, [actor(_), object(_), tense(present), number(singular)]).
event(scare, [actor(_), object(_), tense(present), number(plural)]).
event(hates, [actor(_), object(_), tense(present), number(singular)]).
event(hate, [actor(_), object(_), tense(present), number(plural)]).
event(gave, [actor(Person1), recipient(Person2), object(_), tense(past)]) :- Person1 \= Person2.

personal(i, [number(singular), gender(neutral)]).
personal(he, [number(singular), gender(masculine)]).
personal(she, [number(singular), gender(feminine)]).
personal(it, [number(singular), gender(neutral)]).
personal(that, [number(singular), gender(neutral)]).
personal(those, [number(plural), gender(neutral)]).
personal(they, [number(plural), gender(neutral)]).

possessive(his, [number(singular), gender(masculine)]).
possessive(her, [number(singular), gender(feminine)]).

% You have to write this:
% get the event
process(LogicalForm, Ref1, Ref2) :-
	functor(LogicalForm, event, 2),
	asserta(history(LogicalForm)),
	arg(2, LogicalForm, [Agent | [Object]]),
	process(Agent, Ref1, Ref3),
	process(Object, Ref3, Ref2).

% get the thing
process(Agent, Ref1, Ref3) :-
	functor(Agent, actor, 1),
	arg(1, Agent, Thing),
	process(Thing, Ref1, Ref3).

% get the personal
process(Agent, Ref1, Ref3) :-
	functor(Agent, actor, 1),
	arg(1, Agent, Personal),
	process(Personal, Ref1, Ref3).

% get the name e.g john
process(Thing, _, _) :-
	functor(Thing, thing, 2),
	arg(1, Thing, Name),
	thing(Name, Plist),
	member(gender(_), Plist),
	asserta(history(thing(Name,Plist))).

% get the personal pronouns e.g. he, and check whether it matches the histories
process(Personal, Ref1, Ref3):-
	functor(Personal, personal, 1),
	arg(1, Personal, Pronoun0),
	personal(Pronoun0, Props0),
	member(gender(Gender0), Props0),
	member(number(Number0), Props0),
	history(thing(Ref5, Props3)),
	member(gender(Gender0),Props3),
	member(number(Number0), Props3),
	append(Ref1, [Ref5], Ref3).

% get the possessive
process(Object, Ref3, Ref2):-
	functor(Object, object, 1),
	arg(1, Object, Possessive),
	process(Possessive, Ref3, Ref2).

% get the possesive pronoun and its thing and check whether it matches the histories
process(Possessive, Ref3, Ref2):-
	functor(Possessive, possessive, 2),
	arg(1, Possessive, Pronoun),
	possessive(Pronoun,Props1),
	member(gender(Gender), Props1),
	member(number(Number), Props1),
	history(thing(Ref, Props2)),
	member(gender(Gender), Props2),
	member(number(Number), Props2),
	append(Ref3, [Ref], Ref2),
	arg(2, Possessive, Thing2),
	process(Thing2, Ref3, Ref2).

% get the name of the possessive e.g. his
process(Thing2, _, _):-
	functor(Thing2, thing, 2),
	arg(1, Thing2, Name2),
	thing(Name2, Plist2),
	member(gender(_), Plist2),
	asserta(history(thing(Name2,Plist2))).

run(S, Refs) :-
	sentence(X, S, []), !,
	writeln(X),
	process(X, [], Refs),
	listing(history/1).
	