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
?- run([jack,lost,his,telescope], Refs).
event(lost,[actor(thing(jack,[])),object(possessive(his,thing(telescope,[])))])
:- dynamic history/1.

history(thing(telescope, [isa(physical_object), gender(neutral), number(singular)])).
history(thing(jack, [isa(person), gender(masculine), number(singular)])).
history(event(lost, [actor(thing(jack, [])), object(possessive(his, thing(telescope, [])))])).

Refs = [jack] 
```
From above, 'his' is a pronoun that is referring back to 'jack'. Therefore, Refs returns a list of references that has only one element inside ('jack').

Now if we run the query below:

```
?- run([he,looked,for,it], X).
event(looked,[actor(personal(he)),object(personal(it))])
:- dynamic history/1.

history(event(looked, [actor(personal(he)), object(personal(it))])).
history(thing(telescope, [isa(physical_object), gender(neutral), number(singular)])).
history(thing(jack, [isa(person), gender(masculine), number(singular)])).
history(event(lost, [actor(thing(jack, [])), object(possessive(his, thing(telescope, [])))])).

X = [jack, telescope] 
```
The output list contains two elements, 'jack' and 'telescope', as the input sentence has two references 'he' and 'it' that refer to those pronouns respectively.
