# testit

<!-- badges: start -->

[![R-CMD-check](https://github.com/yihui/testit/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/yihui/testit/actions/workflows/R-CMD-check.yaml)
[![Downloads from the RStudio CRAN
mirror](https://cranlogs.r-pkg.org/badges/testit)](https://cran.r-project.org/package=testit)
[![Codecov test
coverage](https://codecov.io/gh/yihui/testit/graph/badge.svg)](https://app.codecov.io/gh/yihui/testit)

<!-- badges: end -->

**testit** is a minimal testing package for R with zero dependencies. If you can
write an R expression that returns `TRUE`, you can write a test with **testit**.

## Installation

From CRAN:

``` r
install.packages('testit')
```

Development version:

``` r
install.packages('testit', repos = 'https://yihui.r-universe.dev')
```

## Quick start

The core idea: wrap conditions in parentheses `()` inside `assert()`. If the
condition is `TRUE`, the test passes silently. If it is `FALSE`, you get an
error with the message you provided.

``` r
library(testit)

assert('one plus one is two', {
  (1 + 1 == 2)
})
```

You can put multiple conditions in one `assert()` call, and mix in setup code
(lines without parentheses are just evaluated normally):

``` r
assert('basic arithmetic works', {
  x = 1 + 1
  (x == 2)
  (x > 0)
})
```

To test if two objects are identical, use `%==%` (a shortcut for `identical()`).
When a `%==%` comparison fails inside `assert()`, you get a helpful diff showing
what was different:

``` r
assert('identical comparison', {
  (c(1, 2, 3) %==% 1:3)
})
```

## How it works

**testit** has one rule: an expression in parentheses `()` is a test condition
that must evaluate to `TRUE`. Everything else is ordinary R code.

``` r
assert('a descriptive message about what you are testing', {
  # setup code (no parentheses = not checked)
  x = sqrt(4)

  # test conditions (parentheses = must be TRUE)
  (x == 2)
  (x > 0)
  (is.numeric(x))
})
```

That's it. No special matchers to memorize, no DSL to learn.

## Testing for errors, warnings, and messages

Use `has_error()`, `has_warning()`, and `has_message()` to verify that code
signals the expected condition:

``` r
assert('errors are caught correctly', {
  (has_error(log('a')))
  (has_error(stop('oops'), 'oops'))  # also match the error message
  (!has_error(log(1)))               # no error here
})

assert('warnings are caught correctly', {
  (has_warning(warning('watch out')))
  (has_warning(1:2 + 1:3, 'longer object'))
})
```

## Snapshot testing

Snapshot tests let you verify printed output by recording it in a Markdown file.
Place files named `test-*.md` in your `tests/testit/` directory:

```` markdown
# Printing a sequence

```r
1:5
```

```
[1] 1 2 3 4 5
```
````

The R code block (```` ```r ````) is executed and its output is compared to the
following plain code block (```` ``` ````). If the output changes, the test
fails with a diff. To accept the new output, run:

``` r
test_pkg(update = TRUE)
```

You can write explanatory text anywhere in the file -- only code blocks are
evaluated.

## Setting up tests in your package

1.  Create the directory `tests/testit/` in your package.

2.  Write test files named `test-*.R` (and optionally `test-*.md` for
    snapshots).

3.  Create `tests/test-all.R` with:

``` r
library(testit)
test_pkg('yourpkg')
```

`library(testit)` is required because test scripts call `assert()` and other
**testit** functions directly. Without it, you will get errors like "could not
find function `assert`".

That's all. `R CMD check` will run your tests automatically. In RStudio, you can
also press `Ctrl/Cmd + Shift + T` to run tests during development. For this to
work, go to the menu `Build > Configure Build Tools...` and make sure "Use
devtools package functions if available" is **unchecked** (otherwise RStudio
will try to use **testthat** conventions instead).

### Helper files

If you have shared setup code, put it in files named `helper*.R` (e.g.,
`helper.R`, `helper-utils.R`) in the same test directory. They are sourced
before any test file runs.

Global helpers in the parent directory (e.g., `tests/helper.R`) are also
sourced -- before the subdirectory-level helpers. This is useful when you have
multiple test subdirectories (e.g., `tests/testit/`, `tests/test-cran/`) and
want to share common utilities across all of them.

### Accessing internal functions

`test_pkg()` runs your tests inside the package namespace, so you can call
non-exported (internal) functions directly -- no need for the `:::` operator.

## Comparison with testthat

**testit** is intentionally minimal. There is one rule to remember: parentheses
around an expression means "this must be `TRUE`." There are no matchers like
`expect_equal` vs. `expect_identical` vs. `expect_equivalent` to choose between.

The tradeoff is that you get fewer bells and whistles (no mocking, no parallel
execution). If you want a simple, dependency-free testing tool that stays out of
your way, **testit** is a good fit.

If you want to convert your existing **testthat** tests to **testit**, you may
take a look at this post <https://yihui.org/en/2026/05/testthat-to-testit/>.
If you use AI assistance, this should be a straightforward task.

--------------------------------------------------------------------------------

<img src="https://i.imgur.com/sDsgmfj.jpeg" alt="Xunzi" align="right" width="100"/>

Although he did not really mean it, [Xunzi](https://en.wikipedia.org/wiki/Xunzi)
said something that happens to apply well to unit testing:

> 不积跬步，无以至千里；不积小流，无以成江海。

(A journey of a thousand miles begins with a single step; a great river is
formed from many small streams.)

This package is free and open source software, licensed under MIT.
