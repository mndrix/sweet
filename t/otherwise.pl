:- use_module(library(sweet)).

:- use_module(library(tap)).

easy :-
    ( false ->
        throw("Should never see this")
    ; otherwise ->
        true
    ).
