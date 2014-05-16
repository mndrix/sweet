:- use_module(library(sweet)).
:- use_module(library(assoc)).
:- use_module(library(rbtrees)).

:- use_module(library(tap)).

% lists
list_check_start :-
    a in [a,l,l].

list_check_middle :-
    a in [c,a,t].

list_check_end :-
    a in [d,c,b,a].

list_check_duplicate :-
    e in [b,e,e,f].

list_iterate :-
    aggregate(count, X^(X in [1,2,3]), N),
    N == 3.


% dicts
dict_check :-
    foo-bar in _{foo:bar, alpha:a, one:1}.

dict_get_value :-
    foo-Bar in _{foo:bar, alpha:a, one:1},
    Bar == bar.

dict_iterate :-
    setof(K=V,K-V in _{foo:bar,alpha:a},EqPairs),
    EqPairs = [alpha=a,foo=bar].


% assocs
assoc_check :-
    list_to_assoc([a-1,c-3,b-2], Assoc),
    c-3 in Assoc.

assoc_get_value :-
    list_to_assoc([a-1,c-3,b-2], Assoc),
    c-Three in Assoc,
    Three == 3.

assoc_iterate :-
    list_to_assoc([a-1,c-3,b-2], Assoc),
    setof(K=V,K-V in Assoc,EqPairs),
    EqPairs = [a=1,b=2,c=3].


% rbtrees
rbtree_check :-
    list_to_rbtree([a-1,c-3,b-2], Rb),
    c-3 in Rb.

rbtree_get_value :-
    list_to_rbtree([a-1,c-3,b-2], Rb),
    c-Three in Rb,
    Three == 3.

rbtree_iterate :-
    list_to_rbtree([a-1,c-3,b-2], Rb),
    setof(K=V,K-V in Rb,EqPairs),
    EqPairs = [a=1,b=2,c=3].
