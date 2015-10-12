## Changelog

#### Upcoming in master

* Added in name resolution.
* Added in import statements.
* Added in `external` function attribute.
* Added in exceptions from name resolver.

#### v0.2.0.0

* Added type checking.
* Added required `module` statement.
* Removed the interpreter.
* Added in compilation to LLVM (v3.4) for integers and booleans.

#### v0.1.1.3

* Bumped the minimum version of base to 4.8.

#### v0.1.1.2

* Added the Control.Applicative import to files as base 4.7 doesn't import the required operators in
    the Prelude.

#### v0.1.1.1

* Bumped the minimum version of pretty to 1.1.2.

#### v0.1.1.0

* Fixed a bug in the interpreter that prevented evaluating local variables.

#### v0.1.0.1

* Relaxed the minimum version of base to 4.7.

#### v0.1.0.0

This is the initial release of the Qux language library!
It includes a very basic parser, printer, type checker and interpreter.

