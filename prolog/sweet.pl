:- module(sweet, [ if/2
                 , if/3
                 , otherwise/0
                 , todo/0
                 , todo/1
                 , todo/2
                 ]).


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
