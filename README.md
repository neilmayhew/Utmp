# utmp

A Haskell library and utilities for reading and writing Linux utmp files.

The `Storable` instances are for use with data returned by C APIs such as
`getutent`. The `Binary` instances are for reading and writing utmp files
directly, which is typically more convenient than using the (highly stateful)
C APIs.

See `CountsByDay.hs` for an example of how to use the library.

Neil Mayhew
