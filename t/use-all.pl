:- use_module(library(sweet)).

% make sure that we only see predicates we have imported
:- set_prolog_flag(autoload,false).

% import all predictaes from a module
:- use random.

:- use_module(library(tap)).


imported_from :-
    predicate_property(random(_),imported_from(random)).

calling :-
    random(F),
    float(F).
