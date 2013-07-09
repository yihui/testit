# testit

[![Build Status](https://travis-ci.org/yihui/testit.png)](https://travis-ci.org/yihui/testit)

This package provides two simple functions (30 lines of code in total):

- `assert(fact, ...)`: think of it as `message(fact)` + `stopifnot(...)`

- `test_pkg(package)`: runs tests with all objects (exported or
  non-exported) in the package namespace directly available, so no need to
  use `package:::name` for non-exported objects

## Why?

The reason is laziness. It is tedious to type these commands repeatedly in
tests:

```ruby
message('checking if these numbers are equal...')
stopifnot(all.equal(1, 1+1e-10), 10*.1 == 1)

message('checking if a non-exported function works...')
stopifnot(is.character(package:::utility_foo(x = 'abcd', y = 1:100)))
```

With the two simple functions above, we type six letters (`assert`) instead
of sixteen (`message` + `stopifnot`), and `assert` is also a more intuitive
function name for testing purposes (you _assert_ a fact followed by evidence):

```ruby
assert(
  'these numbers are equal',
  all.equal(1, 1+1e-10), 10*.1 == 1
)

assert(
  'a non-exported function works',
  is.character(utility_foo(x = 'abcd', y = 1:100))
)
```

## R CMD check

Put the tests under the directory `pkg_name/tests/testit/` (where `pkg_name`
is the root directory of your package), and write a `test-all.R` under
`pkg_name/tests/`:

```ruby
library(testit)
test_pkg('pkg_name')
```

That is all for `R CMD check`. For package development, it is recommended to
use [**devtools**](http://cran.r-project.org/package=devtools). In
particular, `Ctrl + Shift + L` in RStudio makes all objects in a package
visible to you, and you can play with the tests freely.

## Installation

Stable version on CRAN:

```ruby
install.packages('testit')
```

Development version:

```ruby
library(devtools)
install_github('testit', 'yihui')
```

## More

How about [**testthat**](http://cran.r-project.org/package=testthat)? Well,
this package is far less sophisticated than **testthat**. There is nothing
fancy in this package. Please do consider **testthat** if your tests require
more granularity.

There is no plan to add new features or reinvent anything in this package.
It is an intentionally tiny package.

<img src="http://i.imgur.com/sDsgmfj.jpg" align="right" width="100" />

Although he did not really mean it,
[Xunzi](http://en.wikipedia.org/wiki/Xunzi) said something that happens to
apply well to unit testing:

> 不积跬步，无以至千里；不积小流，无以成江海。

This package is free and open source software, licensed under GPL.
