# Synopsis

    :- use_module(library(sweet)).
	?- (false -> throw(oops); otherwise -> writeln(ok)).
    ok
    true.

    ?- if(fail,throw(oops)), if(true,writeln(ok)).
    ok
    true.

# Description

The `sweet` pack adds some convenient syntactic sugar to Prolog. The
main goal is to reduce the amount of redundant code that one must read
(and write). Some of the predicates and macros are simple aesthetic
adjustments (like otherwise/0).

# Changes in this Version

  * First public release

# Installation

Using SWI-Prolog 6.3 or later:

    ?- pack_install(sweet).

This module uses [semantic versioning](http://semver.org/).

Source code available and pull requests accepted at
http://github.com/mndrix/sweet

@author Michael Hendricks <michael@ndrix.org>
@license unlicense
