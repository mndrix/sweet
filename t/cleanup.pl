:- use_module(library(sweet)).

reset_global_variable_ :-
    b_setval(foo,start),
    cleanup(b_setval(foo,clean)),
    b_getval(foo,Val),
    Val == start.  % clean up hasn't happened yet

b_consval(Name,Value) :-
    b_getval(Name,Old),
    b_setval(Name,[Value|Old]).

lifo_ :-
    b_setval(foo,[]),
    cleanup(b_consval(foo,a)),
    cleanup(b_consval(foo,b)),
    b_getval(foo,Val),
    Val == [].

:- use_module(library(tap)).

% test behavior
reset_global_variable :-
    reset_global_variable_,
    b_getval(foo,Val),
    Val == clean.  % clean up has happened

lifo :-
    lifo_,
    b_getval(foo,Val),
    Val == [a,b].


% test macro expansion

three_small_parts :-
     Old = (a,cleanup(b),c),
     sweet:cleanup_macro(Old,New),
     New == (a,call_cleanup(c,b)).

first_goal :-
    Old = (cleanup(a),b),
    sweet:cleanup_macro(Old,New),
    New == call_cleanup(b,a).

final_goal :-
    Old = (a,cleanup(b)),
    sweet:cleanup_macro(Old,New),
    New == (a,call(b)).

all_alone :-
    Old = cleanup(a),
    sweet:cleanup_macro(Old,New),
    New == call(a).

multiple_cleanings :-
    Old = (a,cleanup(b),cleanup(c),d),
    sweet:cleanup_macro(Old,New),
    New == (
        a,
        call_cleanup(
            call_cleanup(d,c),
            b
        )
    ).

bare_conditional :-
    Old = (
        ( a -> b,cleanup(c),d
        ; e,cleanup(f),g
        )
    ),
    sweet:cleanup_macro(Old,New),
    New == (
        ( a -> b, call_cleanup(d,c)
        ; e, call_cleanup(g,f)
        )
    ).

mixed_conditional :-
    Old = (
        prelude,
        ( a -> b,cleanup(c),d
        ; e,cleanup(f),g
        )
    ),
    sweet:cleanup_macro(Old,New),
    New == (
        prelude,
        ( a -> b, call_cleanup(d,c)
        ; e, call_cleanup(g,f)
        )
    ).
