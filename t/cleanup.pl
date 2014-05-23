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

reset_global_variable :-
    reset_global_variable_,
    b_getval(foo,Val),
    Val == clean.  % clean up has happened

three_small_parts :-
     Old = (a,cleanup(b),c),
     sweet:cleanup_macro(Old,New),
     New == setup_call_cleanup(a,c,b).

first_goal :-
    Old = (cleanup(a),b),
    sweet:cleanup_macro(Old,New),
    New == setup_call_cleanup(true,b,a).

final_goal :-
    Old = (a,cleanup(b)),
    sweet:cleanup_macro(Old,New),
    New == setup_call_cleanup(a,true,b).

all_alone :-
    Old = cleanup(a),
    sweet:cleanup_macro(Old,New),
    New == setup_call_cleanup(true,true,a).

multiple_cleanings :-
    Old = (a,cleanup(b),cleanup(c),d),
    sweet:cleanup_macro(Old,New),
    New == setup_call_cleanup(
               a,
               setup_call_cleanup(true,d,c),
               b
           ).

lifo :-
    lifo_,
    b_getval(foo,Val),
    Val == [a,b].
