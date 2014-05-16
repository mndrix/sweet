:- use_module(library(sweet)).

% make sure that we only see predicates we have imported
:- set_prolog_flag(autoload,false).

% import a single predicate from a module
:- use random -> random/1.
:- use lists -> append/{2,3}.

:- use_module(library(tap)).


imported_from :-
    predicate_property(random(_),imported_from(random)),
    predicate_property(append(_,_),imported_from(lists)),
    predicate_property(append(_,_,_),imported_from(lists)).

calling :-
    random(F),
    float(F),

    append([a],[b],[a,b]),

    append([[a],[b]], [a,b]).
