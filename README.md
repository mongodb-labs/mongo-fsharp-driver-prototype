MongoDB F# Driver Prototype
===========================

This is a prototype MongoDB driver for F#. The goal of this driver is to make 
using MongoDB from F# more natural by defining new operators that are 
idiomatic to the language.

The API and implementation are currently subject to change at any time. You 
must not use this driver in production as it is still under development and 
is in no way supported by 10gen. We absolutely encourage you to experiment 
with it and provide us feedback on the API, design, and implementation. Bug 
reports and suggestions for improvements are welcomed, as are pull requests.

Dependencies
------------
- F# 3.0

Building
--------

The F# driver has been developed on top of the new [Core .NET driver](https://github.com/mongodb/mongo-csharp-driver/tree/v2.0). 
This new *Core .Net driver* is still in development and hence unavailable on 
NuGet. This branch has been setup as a submodule. This is intended to change 
in the future.

    git submodule update --init
    <compile mongo-csharp-driver>
    <compile mongo-fsharp-driver-prototype>

Ideas for more convenient query writing
---------------------------------------

  - curried query builder for use with `|>` operator
  - type provider for `collection`.`field`... access
  - code quotations for succinct expressions, e.g. `<`, `>`, `=`
  - extend computation expressions for defining queries, e.g. `unwind`

### Structure of the type provider

Lives in the namespace `FSharp.MongoDB.Driver.TypeProvider`.

Need to redefine the `MongoConnection` to accept settings via type
parameters, similar to the `SqlDataConnection` module.

Connects to the server and provides a static method `GetDataContext()`
that returns a `MongoServer` type. ~~In addition, the data context also
provides instance properties erased as `MongoDatabase` types with names
as those available from the server.~~ Due to how authentication works
on a database level, rather than on a server level, only plan to
display collection names and document fields.

These wrapped types will also provide additional instance properties
erased as `MongoCollection` types with names as those contained in the
database.

Probably employ a similar strategy for the documents themselves.

High-level API
--------------

Implemented on top of the restructured core .NET driver.

  * MongoDatabase = has properties to explore contained collections
  * MongoCollection = actor-style client for executing commands on a
                      collection
