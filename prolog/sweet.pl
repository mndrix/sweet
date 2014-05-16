:- module(sweet, [ if/2
                 , if/3
                 , otherwise/0
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
otherwise :-
    true.
