:- module(sweet, [ otherwise/0
                 ]).

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
