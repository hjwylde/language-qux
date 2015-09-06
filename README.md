# language-qux

[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/1.0.0/active.svg)](http://www.repostatus.org/#active)
[![Release](https://img.shields.io/github/release/qux-lang/language-qux.svg)](https://github.com/qux-lang/language-qux/releases)

Qux is an experimental language developed from the ground up with the aim of supporting extended
    static checks at compile time.
This package provides tools for working with it (parsing, printing, type checking and running)
    within Haskell code.

### Aims

Qux is designed as an imperative, functional programming language with extended static checks for
    program verification.
The goal is to be able to write pre- and post-conditions as part of type and method contracts that
    will be verifiable by mathematical means (e.g., a SMT solver or theorem prover).

Inspiration for a language with extended static checks has come from David J. Pearce's language,
    [Whiley](https://github.com/whiley "Whiley").
During my honours year I contributed a solution for verifying Whiley's method contracts using an
    external SMT solver (Z3).
For those interested in reading this rather long report, see
    [here](http://homepages.ecs.vuw.ac.nz/~djp/files/HenryWyldeENGR489.pdf).

### Tasks for v1

The language is in it's very initial stages of development and thus has very little features!
There are a few core tasks in mind that need to be achieved before v1 can come about:

* Key language features (assignment, strings, packages and imports).
* Pre- and post-conditions on methods (using the `requires` and `ensures` keywords).
* Compilation and verification of method contracts.
* An intermediary language that retains contract information (see the
    [quux language](https://github.com/qux-lang/language-quux "quux")).
* Compilation to LLVM.

Of course, there will be minor tasks to acompany this release, such as documentation on the language features.
The above merely mark the core functionality required!

