# testit

<!-- badges: start -->

[![R-CMD-check](https://github.com/yihui/testit/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/yihui/testit/actions/workflows/R-CMD-check.yaml)
[![Downloads from the RStudio CRAN
mirror](https://cranlogs.r-pkg.org/badges/testit)](https://cran.r-project.org/package=testit)
[![Codecov test
coverage](https://codecov.io/gh/yihui/testit/graph/badge.svg)](https://app.codecov.io/gh/yihui/testit)

<!-- badges: end -->

This package provides three simple functions:

-   `assert(fact, ...)`: think of it as `message(fact)` + `stopifnot(...)`

-   `test_pkg(package)`: runs tests with all objects (exported or non-exported)
    in the package namespace directly available, so no need to use the
    triple-colon `package:::name` for non-exported objects

-   `snapshot(name, expr)`: capture and compare plain-text output for snapshot
    testing

## Why?

Because it is tedious to type these commands repeatedly in tests:

``` r
message('checking if these numbers are equal...')
stopifnot(all.equal(1, 1+1e-10), 10*.1 == 1)

message('checking if a non-exported function works...')
stopifnot(is.character(package:::utility_foo(x = 'abcd', y = 1:100)))
```

With the two simple functions above, we type six letters (`assert`) instead of
sixteen (`message` + `stopifnot`), and `assert` is also a more intuitive
function name for testing purposes (you *assert* a fact followed by evidence):

``` r
assert('These numbers are equal', {

  (all.equal(1, 1 + 1e-10))

  (10 * .1 == 1)

})

assert('A non-exported function works', {
  res = utility_foo(x = 'abcd', y = 1:100)
  (is.character(res))
})

assert('T is TRUE and F is FALSE by default, but can be changed', {
  (T == TRUE )
  (F == FALSE)

  T = FALSE
  (T == FALSE)
})
```

## Snapshot testing

The `snapshot()` function provides a simple way to test output that would be
tedious to check with assertions:

``` r
# Capture and compare printed output
snapshot('my_output', {
  print(data.frame(x = 1:3, y = letters[1:3]))
})

# Snapshots are stored as plain text files in _snapshots/
# - To create/update: set TESTIT_UPDATE_SNAPSHOTS=true
# - Files are human-readable and easy to review in version control
```

## R CMD check

Put the tests under the directory `pkg_name/tests/testit/` (where `pkg_name` is
the root directory of your package), and write a `test-all.R` under
`pkg_name/tests/`:

``` r
library(testit)
test_pkg('pkg_name')
```

That is all for `R CMD check`. For package development, you can
`Ctrl/Cmd + Shift + T` to run tests.

## Installation

Stable version on CRAN:

``` r
install.packages('testit')
```

Development version:

``` r
remotes::install_github('yihui/testit')
```

## More

How about [**testthat**](https://CRAN.R-project.org/package=testthat)? Well,
this package is far less sophisticated than **testthat**. There is nothing fancy
in this package. I do not use **testthat** by myself because I'm too lazy to
learn the new vocabulary (`testthat::expect_xxx`). For **testit**, I do not need
to think if I should use `expect_equal`, `expect_equivalent`, or
`expect_identical`; I just write test conditions in parentheses that are
expected to return `TRUE`. That is the one and only rule to remember.

There is no plan to add new features or reinvent anything in this package. It is
an intentionally tiny package with zero dependencies.

<img src="https://i.imgur.com/sDsgmfj.jpeg" alt="Xunzi" align="right" width="100"/>

Although he did not really mean it, [Xunzi](https://en.wikipedia.org/wiki/Xunzi)
said something that happens to apply well to unit testing:

> 不积跬步，无以至千里；不积小流，无以成江海。

This package is free and open source software, licensed under MIT.
