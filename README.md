# testit

[![Build Status](https://travis-ci.org/yihui/testit.svg)](https://travis-ci.org/yihui/testit)
[![Coverage Status](https://coveralls.io/repos/github/yihui/testit/badge.svg?branch=master)](https://coveralls.io/github/yihui/testit?branch=master)
[![Downloads from the RStudio CRAN mirror](https://cranlogs.r-pkg.org/badges/testit)](https://cran.r-project.org/package=testit)

This package provides two simple functions (30 lines of code in total):

- `assert(fact, ...)`: think of it as `message(fact)` + `stopifnot(...)`

- `test_pkg(package)`: runs tests with all objects (exported or
  non-exported) in the package namespace directly available, so no need to
  use the triple-colon `package:::name` for non-exported objects

## Why?

Because it is tedious to type these commands repeatedly in
tests:

```r
message('checking if these numbers are equal...')
stopifnot(all.equal(1, 1+1e-10), 10*.1 == 1)

message('checking if a non-exported function works...')
stopifnot(is.character(package:::utility_foo(x = 'abcd', y = 1:100)))
```

With the two simple functions above, we type six letters (`assert`) instead
of sixteen (`message` + `stopifnot`), and `assert` is also a more intuitive
function name for testing purposes (you _assert_ a fact followed by evidence):

```r
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

## R CMD check

Put the tests under the directory `pkg_name/tests/testit/` (where `pkg_name`
is the root directory of your package), and write a `test-all.R` under
`pkg_name/tests/`:

```r
library(testit)
test_pkg('pkg_name')
```

That is all for `R CMD check`. For package development, it is recommended to
use [**devtools**](https://cran.r-project.org/package=devtools). In
particular, `Ctrl/Cmd + Shift + L` in RStudio makes all objects in a package
visible to you, and you can run tests interactively.

## Installation

Stable version on CRAN:

```r
install.packages('testit')
```

Development version:

```r
devtools::install_github('yihui/testit')
```

## More

How about [**testthat**](https://CRAN.R-project.org/package=testthat)? Well,
this package is far less sophisticated than **testthat**. There is nothing
fancy in this package. Please do consider **testthat** if your tests require
more granularity. I myself do not use **testthat** because I'm too lazy to learn
the new vocabulary (`testthat::expect_xxx`). For **testit**, I do not need to
think if I should use `expect_equal`, `expect_equivalent`, or
`expect_identical`; I just write test conditions in parentheses that are expected to return `TRUE`. That
is the only single rule to remember.

There is no plan to add new features or reinvent anything in this package.
It is an intentionally tiny package.

<img src="https://i.imgur.com/sDsgmfj.jpg" align="right" width="100" alt="Xunzi" />

Although he did not really mean it,
[Xunzi](https://en.wikipedia.org/wiki/Xunzi) said something that happens to
apply well to unit testing:

> 不积跬步，无以至千里；不积小流，无以成江海。

This package is free and open source software, licensed under GPL-3.
