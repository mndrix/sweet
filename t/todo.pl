:- use_module(library(sweet)).

:- use_module(library(tap)).

zero(throws(todo)) :-
    todo.

one(throws("Important code goes here")) :-
    todo("Important code goes here").

two(throws(foo)) :-
    _ = A,
    todo(foo, A).
