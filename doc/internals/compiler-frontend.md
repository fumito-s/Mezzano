# Notes on the first iteration of the compiler (aka "the compiler")

## Design

Common Lisp source code is accepted, parsed, and canonicalized. It is transformed into
a tree of abstract syntax objects (AST). Various optimizations are performed on the tree.

## Function properties, future directions

The compiler need to know various things about functions in the world. Currently these
properties are scattered around various global variables and a few hard-coded lists.
These should all be found and centralized.

Non-exhaustive list of properties that exist for a function:

* name
* constant-fold-mode (unconditional or arg type match or commutative-arithmetic)
* transforms
* pure-functions
* pure-functions-at-low-safety
* inline-info
* macro
* compiler-macro
* declared type
* associated fref
* mod-n-arithmetic
* atomic-struct-slots/etc
* architecture-specific builtins
* backend primitives(?)
* plist

There's been some small work towards this in the form of `function-info`.
