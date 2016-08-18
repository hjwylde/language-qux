# Changelog

### Upcoming

*Major*

* Added in name resolution. ([#15](https://github.com/hjwylde/language-qux/issues/15))
* Added in import statements. ([#8](https://github.com/hjwylde/language-qux/issues/8))
* Added in `external` function attribute. ([#16](https://github.com/hjwylde/language-qux/issues/16))
* Added in `Char` type and character literals. ([#10](https://github.com/hjwylde/language-qux/issues/10))
* Added in external type declarations. ([#27](https://github.com/hjwylde/language-qux/issues/27))
* Updated to use LLVM 3.5. ([#7](https://github.com/hjwylde/language-qux/issues/7))
* Fixed external function declarations from requiring parameter names. ([#41](https://github.com/hjwylde/language-qux/issues/41))

*Minor*

* Added in exceptions from the name and type resolvers. ([#13](https://github.com/hjwylde/language-qux/issues/13), [#19](https://github.com/hjwylde/language-qux/issues/19))

### v0.2.0.0

*Major*

* Added type checking.
* Added required `module` statement. ([#2](https://github.com/hjwylde/language-qux/issues/2))
* Removed the interpreter.
* Added in compilation to LLVM (v3.4) for integers and booleans. ([#5](https://github.com/hjwylde/language-qux/issues/5))

### v0.1.1.3

*Revisions*

* Bumped the minimum version of base to 4.8.

### v0.1.1.2

*Revisions*

* Added the Control.Applicative import to files as base 4.7 doesn't import the required operators in
    the Prelude.

### v0.1.1.1

*Revisions*

* Bumped the minimum version of pretty to 1.1.2.

### v0.1.1.0

*Minor*

* Fixed a bug in the interpreter that prevented evaluating local variables.

### v0.1.0.1

*Revisions*

* Relaxed the minimum version of base to 4.7.

### v0.1.0.0

This is the initial release of the Qux language library!
It includes a very basic parser, printer, type checker and interpreter.
