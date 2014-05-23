# Synopsis

    :- use_module(library(sweet)).

    ?- (false -> throw(oops); otherwise -> writeln(ok)).
    ok
    true.

    ?- if(fail,throw(oops)), if(true,writeln(ok)).
    ok
    true.

    ?- forall(X in [1,2,3], writeln(ok)).
    ok
    ok
    ok
    true.

    :- use lists -> append/{2,3}, last/2.
    % same as use_module(library(lists),[append/2,append/3,last/2]).

# Description

The `sweet` pack adds some convenient syntactic sugar to Prolog. The
main goal is to reduce the amount of redundant code that one must read
(and write). Some of the predicates and macros are simple aesthetic
adjustments (like otherwise/0). Others, like in/2, define powerful
interfaces which can be extended by library authors.

# Changes in this Version

  * Avoid infinite loops while loading code

# Installation

Using SWI-Prolog 6.3 or later:

    ?- pack_install(sweet).

This module uses [semantic versioning](http://semver.org/).

Source code available and pull requests accepted at
http://github.com/mndrix/sweet

@author Michael Hendricks <michael@ndrix.org>
@license unlicense
