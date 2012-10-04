CurryDB
=======

CurryDB: Simple, Polymorphic, Persistent, Transactional, In-memory Key/Value
Database

[![Build Status](https://secure.travis-ci.org/tanakh/CurryDB.png?branch=master)](http://travis-ci.org/tanakh/CurryDB)

# Install

Pre requirement:

* recent (>=2012.2.0.0) [Haskell Platform](http://www.haskell.org/platform/)

Clone repository or unpack archive, then follow below instructions:

~~~ {bash}
$ cabal update
$ cabal install
~~~

# Features

CurryDB is a Simple, Polymorphic, Persistent and Transactional
in-memory key/value database.

## Simple and modern

Very simple implementation by using many modern abstraction techniques.
For example,
[Conduit (a.k.a. Iteratee I/O)](http://hackage.haskell.org/package/conduit)
for efficient and highly abstract network programming,
Lightweight threads and STM for robust concurrent programming.
And following features also helping.

## Polymorphic

CurryDB is polymorphic DB.
This means it can contain any data structure you want to store.

For demonstrating this feature, memcacned and redis protocol support
are included.

## Persistent

CurryDB uses
[persistent data structures](http://en.wikipedia.org/wiki/Persistent_data_structure)
(a.k.a. purely functional data structures)
such as
[hash array mapped tries (HAMT)](http://en.wikipedia.org/wiki/Hash_array_mapped_trie)
and [finger trees](http://en.wikipedia.org/wiki/Finger_tree).

It enables simple concurrent implementation and get rid of *any*
troublesome rocks. The code is fully concurrent.

## Transactional

CurryDB supports full transactional features (ACID).
It uses [software transactional memory (STM)](http://en.wikipedia.org/wiki/Software_transactional_memory) for this purpose.
This permits to write very concise codes.
