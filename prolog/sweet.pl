:- module(sweet, [ if/2
                 , if/3
                 , in/2
                 , otherwise/0
                 , todo/0
                 , todo/1
                 , todo/2
                 , op(990,xfy,in)
                 ]).

:- use_module(library(error), [must_be/2]).


%% if(:Condition, :Action)
%
%  Same as =|(Condition->Action;true)|=. For historic reasons, the
%  standard conditional predicate fails if the condition fails. In many
%  circumstances, one would prefer a noop in that case.
:- meta_predicate if(0,0).
if(Condition, Action) :-
    ( call(Condition) -> call(Action) ; true ).

%% if(:Condition, :Action, :Else)
%
%  Same as =|(Condition->Action;Else)|=. When modifying code that
%  started with if/2, one often gets a cleaner "diff" by using if/3
%  instead of restructuring the whole code to use =|-> ;|=.
%
%  For example,
%
%      @@ -1,3 +1,4 @@
%       if( thing
%         , stuff
%      +  , other
%         )
%
%  vs
%
%      @@ -1,3 +1,4 @@
%      -if( thing
%      -  , stuff
%      -  )
%      +( thing ->
%      +    stuff
%      +; other
%      +)
%
%  This is important for those who view commits as a means of
%  communication between developers rather than just an artificat of
%  version control tools.
:- meta_predicate if(0,0,0).
if(Cond,Action,Else) :-
    (call(Cond)->call(Action);call(Else)).


%% in(?X, +Xs)
%
%  True if X is contained in Xs. For convenience, in/2 is exported as an
%  operator: =|X in [1,2,3]|=. Xs must be nonvar. The following
%  types for Xs are supported natively:
%
%    * list - X is an element of the list
%    * dict - X is =|Key-Val|= pair for each entry
%    * library(assoc) - X is =|Key-Val|= pair for each entry
%    * library(rbtrees) - X is =|Key-Val|= pair for each entry
%
%  In each case, if X or Key is ground, a lookup operation (or
%  memberchk/2) is performed. If X or Key is unbound, each
%  member of Xs is iterated on backtracking.
%
%  To add in/2 support for your own types, add a clause to the multifile
%  predicate =|sweet:has_member(+Xs,?X)|=. As soon as your clause is
%  certain that Xs is of the right type, please call !/0. This keeps
%  in/2 deterministic where possible.
in(X, Xs) :-
    must_be(nonvar, Xs),
    has_member(Xs, X).

:- multifile has_member/2.
has_member([H|T], X) :-
    !,
    ( var(X) ->
        ( X=H ; has_member(T,X) )
    ; X=H ->
        true
    ; otherwise ->
        has_member(T, X)
    ).
has_member(Dict, X) :-
    is_dict(Dict),
    !,
    X = Key-Val,
    get_dict(Key,Dict,Val).
has_member(Assoc, X) :-
    current_predicate(assoc:is_assoc/1), % proceed if library(assoc) loaded
    assoc:is_assoc(Assoc),
    !,
    X = Key-Val,
    ( var(Key) ->
        assoc:gen_assoc(Key,Assoc,Val)
    ; X = Key-Val ->
        get_assoc(Key,Assoc,Val)
    ).
has_member(Rb, X) :-
    current_predicate(rbtrees:is_rbtree/1), % proceed if library(rbtrees) loaded
    rbtrees:is_rbtree(Rb),
    !,
    X = Key-Val,
    rbtrees:rb_in(Key,Val,Rb).


%% otherwise is det.
%
%  AKA =|true|=. This alias is helpful for maintaining visual similarity
%  for the final clause of a chained if-then-else construct. For
%  example:
%
%      ( foo(X) ->
%          do_foo_stuff
%      ; bar(X) ->
%          do_bar_stuff
%      ; otherwise ->
%          do_default_stuff
%      ).
%
%  This predicate is identical to quintus:otherwise/0. It's included
%  here for environments in which autoload is disabled and one doesn't
%  want to add =|:- use_module(library(quintus), [otherwise/0])|=.
otherwise.


%% todo
%
%  Throws the exception =|todo|=. This is convenient during rapid
%  development to mark code that will be written later. If the predicate
%  is accidentally executed, it throws an exception so you can view the
%  stack trace, implement proper code and resume execution.
%
%  For exmaple,
%
%      ( stuff ->
%          handle_the_common_case
%      ; otherwise ->
%          todo
%      )
%
%  Using todo/0, todo/1 or todo/2 also provides a useful semantic
%  distinction compared to throw/1. Static analysis tools might prevent
%  commits or deployment for unfinished code.
todo :-
    throw(todo).

%% todo(+Note)
%
%  Like todo/0 with a Note. Note can be used to leave yourself a
%  reminder of what this code is supposed to do once it's implemented.
%  If todo/1 is executed, it throws an exception.
todo(Explanation) :-
    throw(Explanation).


%% todo(+Note, ?Extra)
%
%  Like todo/1 but provides space for something Extra. This is often a
%  list of variables that will eventually participate in the code that's
%  to be written. This style prevents singleton warnings during
%  development. If todo/2 is executed, Extra is not included in the
%  exception.
%
%  For example,
%
%      ( stuff ->
%          handle_the_common_case(X)
%      ; otherwise ->
%          todo("set a meaningful default", [X])
%      )
todo(Note,_Extra) :-
    throw(Note).
