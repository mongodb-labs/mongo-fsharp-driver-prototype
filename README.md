mongo-fsharp-driver-prototype
=============================

Make using MongoDB from F# more natural by defining new operators that
are more idiomatic to the language.

Ideas for more convenient query writing
---------------------------------------

  - curried query builder for use with `|>` operator
  - type provider for `database`.`collection`.`field`... access
  - code quotations for succinct expressions, e.g. `<`, `>`, `=`
  - extend computation expressions for defining queries, e.g. `unwind`
