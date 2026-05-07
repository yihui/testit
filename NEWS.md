# CHANGES IN testit VERSION 0.19

- `assert()` now includes the actual value of the failed expression in the error message, making it easier to diagnose assertion failures (e.g., `(x %==% y) is not TRUE but FALSE`).

- `has_message()`, `has_warning()`, and `has_error()` gained a `message` argument for matching the condition message via `grepl()`, and `...` for passing additional arguments to `grepl()` (e.g., `fixed = TRUE`, `ignore.case = TRUE`).

- Added support for helper files in tests. Helper files matching `helper*.R` are sourced into a dedicated environment before tests run, and their objects are available to all tests and snapshots.

- Fixed a bug in `test_snaps()` where a code block without an output block would incorrectly claim a later output block (belonging to a subsequent code block) instead of inserting a new one.

- `has_error()` is now always silent (no longer prints error messages to the console), and its `silent` argument has been removed.

- `test_pkg()` now suppresses messages and warnings from test scripts, helper files, and snapshot code evaluation. Diagnostic output (diffs, error locations) is emitted via `message()` so it can be suppressed by callers if needed.

- Error messages now include precise source locations (file and line number). `assert()` reports the exact line of the failing sub-expression, and `test_pkg()` reports error locations in test scripts, helper files, and snapshot code blocks.

- `test_pkg()` now unloads DLLs from the temporary library before cleanup, so the temporary `R-lib-*` directories can be properly removed for packages with compiled code.

# CHANGES IN testit VERSION 0.18

- Snapshot tests now support both ```` ```r ```` and ```` ```{r} ```` R code fences, and snapshot updates preserve the original fence style.

- Added and updated snapshot tests (including `mini_diff()` output snapshots) and refreshed snapshot testing documentation/examples.

# CHANGES IN testit VERSION 0.17

- Made `test_pkg()` compatible with R < 3.3.0 (again).

# CHANGES IN testit VERSION 0.16

- Added support for snapshot testing with Markdown files; see https://pkg.yihui.org/testit/#snapshot-testing for more information.

# CHANGES IN testit VERSION 0.15

- The error location reporting stopped working in the previous version and was fixed in this version.

- The error location will come with a clickable link on it if the environment supports ANSI links.

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
