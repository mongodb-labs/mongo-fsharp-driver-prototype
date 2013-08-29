MongoDB F# Driver Prototype
===========================

This is a prototype MongoDB driver written for F#. The goal of this
driver is to make using MongoDB from F# more natural by defining new
ways to express database/collection operations that are idiomatic to
the language.

#### Special Notes

The API and implementation are currently subject to change at any time.
You **must not** use this driver in production, as it is still under
development and is in no way supported by MongoDB, Inc.

We absolutely encourage you to experiment with it and provide us
feedback on the API, design, and implementation. Bug reports and
suggestions for improvements are welcomed, as are pull requests.

Dependencies
------------

  * F# 3.0

Building
--------

The F# driver has been developed on top of the refactored [Core .NET
driver](https://github.com/mongodb/mongo-csharp-driver/tree/v2.0).
This new ***Core .Net driver*** is still in development as well, and
hence unavailable on NuGet. Thus, the branch has been setup as a
submodule. This is intended to change in the future.

    git submodule update --init
    <compile mongo-csharp-driver>
    <compile mongo-fsharp-driver-prototype>

License
-------

[Apache v2.0](LICENSE)
