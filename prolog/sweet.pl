:- module(sweet, [ cleanup/1
                 , if/2
                 , if/3
                 , in/2
                 , otherwise/0
                 , todo/0
                 , todo/1
                 , todo/2
                 , (use)/1
                 , op(990,xfy,in)
                 , op(1150,fx,use)
                 ]).

:- use_module(library(apply), [maplist/3]).
:- use_module(library(error), [must_be/2]).
:- use_module(library(lambda)).


% true if module being loaded wants our macros expanded
wants_sweetner :-
    prolog_load_context(module, Module),
    Module \== sweet, % don't sweeten ourselves
    current_predicate(Module:otherwise/0),  % prevent autoloading
    predicate_property(Module:otherwise,imported_from(sweet)).


%% cleanup(:Goal) is det.
%
%  Sugar for call_cleanup/2. It's like ignore(once(Goal)) but postponed
%  until the clause is finished. The surrounding context provides the
%  call goal. For example,
%
%      file_codes(File,Codes) :-
%          open(File,read,Stream),
%          cleanup(close(Stream)),
%          read_stream_to_codes(Stream, Codes).
%
%  If a clause calls cleanup/1 multiple times, the cleanup steps are
%  performed in LIFO order. If you need your code to be safe against
%  asynchronous interrupts, use setup_call_cleanup/3 instead.
:- meta_predicate cleanup(1).
cleanup(Goal) :-
    throw(sweet("cleanup/1 macro not expanded", Goal)).

% cleanup_macro(+Goal:callable,-Rewritten:callable)
cleanup_macro(Old,New) :-
    cleanup_macro_(Old,New),
    Old \== New.

cleanup_macro_(cleanup(Cleanup),Rewritten) :-
    % cleanup/1 as final goal in a clause
    !,
    Rewritten = ignore(once(Cleanup)).
cleanup_macro_((cleanup(Cleanup),Call0),Rewritten) :-
    % cleanup/1 with a goal after it (common case)
    !,
    cleanup_macro_(Call0,Call),
    Rewritten = call_cleanup(Call,Cleanup).
cleanup_macro_((A0,B0),Rewritten) :-
    % A0 \= cleanup(_)
    !,
    cleanup_macro_(A0,A),
    cleanup_macro_(B0,B),
    Rewritten = (A,B).
cleanup_macro_((A->B0;C0),Rewritten) :-
    !,
    cleanup_macro_(B0,B),
    cleanup_macro_(C0,C),
    Rewritten = (A->B;C).
cleanup_macro_((A*->B0;C0),Rewritten) :-
    !,
    cleanup_macro_(B0,B),
    cleanup_macro_(C0,C),
    Rewritten = (A*->B;C).
cleanup_macro_(Goal,Goal).


%% if(:Condition, :Action)
%
%  Same as =|(Condition->Action;true)|=. For historic reasons, the
%  standard conditional predicate fails if the condition fails. In many
%  circumstances, one would prefer a noop in that case.
:- meta_predicate if(0,0).
if(Condition, Action) :-
    throw(sweet("if/2 macro not expanded",Condition,Action)).

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
    throw(sweet("if/3 macro not expanded",Cond,Action,Else)).


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
        ( T=[] ->
            X=H
        ; otherwise ->
            (X=H ; has_member(T,X) )
        )
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
:- '$hide'(sweet:otherwise/0).


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


%% use(+ModuleImportOptions)
%
%  Macros for importing modules. SWI Prolog's module system is powerful
%  and well designed. Unfortunately, the syntax is highly repetitive in
%  the common case (importing library predicates). The following macros
%  simplify import declarations making them easier to read. Future
%  releases will add some additional, optional power to the module
%  system.
%
%  These macros are best understood through a series of examples. In
%  each example, the first line shows the sweetened version. The
%  following comment shows how the macro expands.
%
%      :- use random.
%      % :- use_module(library(random)).
%
%      :- use random -> random/1.
%      % :- use_module(library(random), [random/1]).
%
%      :- use lists -> append/{2,3}.
%      % :- use_module(library(lists), [append/2,append/3]).
%
%      :- use path(baz).
%      % :- use_module(path(baz)).
%      % path(baz) could be anything supported by file_search_path/2
%
%      :- use my(foo) -> bar/0.
%      % :- use_module(foo, [bar/0]).
%      % my/1 is an escape hatch to pass a term straight through to
%      % use_module.  Named "my" because it's typically used for
%      % accessing modules relative to the local directory.
use(Options) :-
    throw(sweet("use/1 macro not expanded", Options)).

% translate our module spec into a use_module file term
spec_to_file(my(File),File) :-
    !.
spec_to_file(Module,library(Module)) :-
    atom(Module),
    !.
spec_to_file(Module,Module).

% "foo/1,bar/2" into "[foo/1,bar/2]"
pred_conj_to_list((Pattern,Rest),AllIndicators) :-
    pred_pattern_to_list(Pattern,Indicators),
    append(Indicators,RestIndicators,AllIndicators),
    pred_conj_to_list(Rest,RestIndicators).
pred_conj_to_list(Pattern,Indicators) :-
    pred_pattern_to_list(Pattern,Indicators).

% "foo/{2,3}" into "[foo/2,foo/3]"
pred_pattern_to_list(Name/Arity,[Name/Arity]) :-
    integer(Arity).
pred_pattern_to_list(Name/{ArityConj},Indicators) :-
    xfy_list(',',ArityConj,ArityList),
    maplist(\Arity^Indicator^(Indicator=Name/Arity),ArityList,Indicators).

% copied from library(list_util) to avoid the dependency
xfy_list(Op, Term, [Left|List]) :-
    Term =.. [Op, Left, Right],
    xfy_list(Op, Right, List),
    !.
xfy_list(_, Term, [Term]).

% define macro expansions as an easily testable predicate
macro(
  term,
  (:- use Spec -> PredicateConj),
  (:- use_module(File, PredicateList) )
) :-
    spec_to_file(Spec,File),
    pred_conj_to_list(PredicateConj,PredicateList).
macro(
  term,
  (:- use Spec),
  (:- use_module(File))
) :-
    spec_to_file(Spec,File).
macro(
  goal,
  if(Cond,Action),
  (Cond->Action;true)
).
macro(
  goal,
  if(Cond,Action,Else),
  (Cond->Action;Else)
).

% expand use/1 macros
user:term_expansion(Old,New) :-
    wants_sweetner,
    macro(term,Old,New).

% expand goal macros
user:goal_expansion(Old,New) :-
    wants_sweetner,
    macro(goal,Old,New).

% expand cleanup/1 macros
user:term_expansion((Head:-Old),(Head:-New)) :-
    wants_sweetner,
    cleanup_macro(Old,New).
