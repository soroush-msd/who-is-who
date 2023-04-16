# who-is-who
Extending a definite clause grammar to Perform reference resolution in Prolog.

Run the program in SWI-Prolog by calling the predicate **run(Sentence,[RefList])**:
```
run(S, Refs) :-
	sentence(X, S, []), !,
	writeln(X),
	process(X, [], Refs),
	listing(history/1).
```
This predicate parses the input sentence (S) using the Sentence predicate. It then prints out the output sentence. The Process predicate then takes the parsed sentence and returns a list of references (Refs) corresponding to the list of pronouns in the sentence.


```
?- run([jack,found,his,telescope], Refs).
event(found,[actor(thing(jack,[])),object(possessive(his,thing(telescope,[])))])
:- dynamic history/1.

history(thing(telescope, [isa(physical_object), gender(neutral), number(singular)])).
history(thing(jack, [isa(person), gender(masculine), number(singular)])).
history(event(found, [actor(thing(jack, [])), object(possessive(his, thing(telescope, [])))])).
history(thing(wallet, [isa(physical_object), gender(neutral), number(singular)])).
history(thing(john, [isa(person), gender(masculine), number(singular)])).
history(event(lost, [actor(thing(john, [])), object(possessive(his, thing(wallet, [])))])).

Refs = [jack] 
```
