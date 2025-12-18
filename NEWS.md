# CHANGES IN testit VERSION 0.15

- The error location reporting stopped working in the previous version and was fixed in this version.

# CHANGES IN testit VERSION 0.14

- `test_pkg()` is compatible with R < 3.3.0 (if the function `.traceback()` is unavailable, the error location will not be printed).

- Re-licensed the package to MIT.

# CHANGES IN testit VERSION 0.13

- The global option `options(testit.cleanup = TRUE)` (`TRUE` is the default) can be used to clean up additional files or directories generated in the `tests/` directory during the test process.

# CHANGES IN testit VERSION 0.12

- `test_pkg()` installs the package before running tests when it is called from a non-interactive R session that is not launched by `R CMD check`, e.g., when you run tests in RStudio via `Ctrl/Cmd + Shift + T`, so you will not have to install the package manually (`Ctrl/Cmd + Shift + B`) before running tests.

# CHANGES IN testit VERSION 0.11

- The package license was changed from GPL to GPL-3 (@jayvdb #8).

# CHANGES IN testit VERSION 0.10

- `test_pkg()` also looks for tests under the `tests/testit/` directory. This makes it easier for you to run `test_pkg()` under the root directory of the package, and you don't need to change the working directory to `tests/`.

# CHANGES IN testit VERSION 0.9

- Added a new argument `silent` to has_error() (thanks, @StevenMMortimer, #6).

# CHANGES IN testit VERSION 0.8

- When `%==%` is used inside assert(), a message will be printed if the value is not TRUE, to show the values of the LHS and RHS, respectively.

# CHANGES IN testit VERSION 0.7

- provided an alternative way to write assertions of the form assert('fact', {(condition_2); (condition_2)}); see ?testit::assert for more information

# CHANGES IN testit VERSION 0.6

- test_pkg() runs package tests with top-level environment being set to the namespace of the tested package (thanks, @kalibera, #3)

- all test scripts (test-*.R) are assumed to be encoded in UTF-8 if they contain multibyte characters

# CHANGES IN testit VERSION 0.5

- added an infix operator `%==%` as an alias of identical() (in RStudio, you can use an add-in to insert the text `%==%`)

- test_pkg() will print out the filename of the R script that errored

# CHANGES IN testit VERSION 0.4

- the `fact` argument of `assert()` is optional now: all arguments of `assert()` can be test conditions

# CHANGES IN testit VERSION 0.3

- the test files have to be named of the form test-*.R (or test-*.r), i.e. they have to use the prefix test-

- the test environment is always cleaned (all objects removed) before the next test is run

# CHANGES IN testit VERSION 0.2.1

- fixed a test that failed under R 2.15.x because the argument keep.source did not exist in parse()

# CHANGES IN testit VERSION 0.2

- assert() does not use base::stopifnot() any more; a tailored version of stopifnot() is used now; see ?assert for the differences between this version and base::stopifnot(); in particular, assert(fact, logical(0)) will fail but stopifnot(logical(0)) will not

# CHANGES IN testit VERSION 0.1

- this is the first version of testit; the source code is hosted on Github: https://github.com/yihui/testit

- added functions assert(), test_pkg(), has_error() and has_warning()

