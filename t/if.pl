:- use_module(library(sweet)).

:- use_module(library(tap)).

two_true :-
    if(true,true).

two_failing :-
    if(fail,throw(oops)).

three_true :-
    if(true,true,throw(oops)).

three_failing :-
    if(fail,throw(oops),true).
