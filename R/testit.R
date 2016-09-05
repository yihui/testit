#' Assertions with a message
#'
#' The function \code{assert()} was built from \code{\link{stopifnot}()}. It
#' emits a message in case of errors, which can be a helpful hint for diagnosing
#' the errors (\code{stopifnot()} only prints the possibly truncated source code
#' of the expressions).
#' @param fact a message for the assertions when any of them fails; treated the
#'   same way as expressions in \code{...} if it is not a character string,
#'   which means you do not have to provide a message to this function
#' @param ... any number of R expressions, presumably to return vectors of
#'   \code{TRUE}'s (if \code{FALSE} is returned anywhere, an error will show up)
#' @return Invisible \code{NULL} if all expressions returned \code{TRUE},
#'   otherwise an error is signalled and the user-provided message is emitted.
#' @note The internal implementation of \code{stopifnot()} is different with the
#'   function in R \pkg{base}: (1) the custom message \code{fact} is emitted if
#'   an error occurs (2) \code{assert()} requires the logical values to be
#'   non-empty (3) if \code{...} contains a compound expression in \code{{}}
#'   which returns \code{FALSE} (e.g., \code{if (TRUE) {1+1; FALSE}}), the first
#'   and the last but one line of the source code from \code{\link{deparse}()}
#'   are printed in the error message, otherwise the first line is printed
#' @export
#' @examples assert('one equals one', 1==1)
#' assert('seq and : produce equal sequences', seq(1L, 10L) == 1L:10L)
#' assert('seq and : produce identical sequences', identical(seq(1L, 10L), 1L:10L))
#'
#' # multile tests
#' T=FALSE; F=TRUE
#' assert('T is bad for TRUE, and so is F for FALSE', T!=TRUE, F!=FALSE)
#'
#' # a mixture of tests
#' assert("Let's pray all of them will pass", 1==1, 1!=2, letters[4]=='d', rev(rev(letters))==letters)
#'
#' # logical(0) cannot pass assert(), although stopifnot() does not care
#' try(assert('logical(0) cannot pass', 1==integer(0)))
#' stopifnot(1==integer(0)) # it's OK!
#'
#' # a compound expression
#' try(assert('this if statement returns TRUE', if(TRUE){x=1;x==2}))
#'
#' # no message
#' assert(!FALSE, TRUE, is.na(NA))
assert = function(fact, ...) {
  fact_char = is.character(fact)
  n = length(ll <- if (fact_char) list(...) else list(fact, ...))
  if (n == 0L) return(invisible())
  mc = match.call(); if (fact_char) mc[['fact']] = NULL
  for (i in 1L:n) if (!all_true(r <- ll[[i]])) {
    if (fact_char) message('assertion failed: ', fact)
    stop(sprintf(ngettext(length(r), '%s is not TRUE', '%s are not all TRUE'),
                 deparse_key(mc[[i + 1]])), call. = FALSE, domain = NA)
  }
}

#' @description The infix operator \code{\%==\%} is simply an alias of the
#'   \code{\link{identical}()} function to make it slightly easier and intuitive
#'   to write test conditions. \code{x \%==\% y} is the same as
#'   \code{identical(x, y)}.
#' @param x,y two R objects to be compared
#' @rdname assert
#' @export
`%==%` = function(x, y) identical(x, y)

#' Run the tests of a package in its namespace
#'
#' The main purpose of this function is to expose the namespace of a package
#' when running tests, which allows one to use non-exported objects in the
#' package without having to resort to the triple colon \code{\link{:::}} trick.
#'
#' The tests are assumed to be under the \file{testit/} directory by default,
#' and this function also looks for the \file{tests/testit/} directory under the
#' package installation directory when the user-provided \code{dir} does not
#' exist. The test scripts must be named of the form \samp{test-*.R}; other R
#' scripts will not be treated as test files (but may also be useful, e.g. you
#' can \code{\link{source}()} them in tests).
#'
#' For \command{R CMD check}, this means the test R scripts (\file{test-*.R} or
#' \file{test-*.r}) are under \file{pkg_root/tests/testit/}. The R scripts are
#' executed with \code{\link{sys.source}} in the namespace of the package to be
#' tested; when an R script is executed, the working directory is the same as
#' the directory containing this script, and all existing objects in the test
#' environment will be removed before the code is executed.
#' @param package the package name
#' @param dir the directory of the test files; by default, it is the directory
#'   \file{testit/} under the current working directory
#' @return \code{NULL}. All test files are executed, unless an error occurs.
#' @seealso The \pkg{testthat} package (much more sophisticated).
#' @export
#' @examples \dontrun{test_pkg('testit')}
test_pkg = function(package, dir = 'testit') {
  library(package, character.only = TRUE)
  path = available_dir(c(dir, system.file('tests', 'testit', package = package)))
  rs = list.files(path, '^test-.+[.][rR]$', full.names = TRUE)
  # make all objects in the package visible to tests
  env = new.env(parent = getNamespace(package))
  for (r in rs) {
    rm(list = ls(env, all.names = TRUE), envir = env)
    withCallingHandlers(
      sys.source.topenv(r, envir = env, top.env = getNamespace(package)),
      error = function(e) {
        message(r, ':')
      }
    )
  }
}

#' Check if an R expression produces warnings or errors
#'
#' The two functions \code{has_warning()} and \code{has_error()} check if an
#' expression produces warnings and errors, respectively.
#' @param expr an R expression
#' @return A logical value.
#' @export
#' @rdname has_message
#' @examples has_warning(1+1); has_warning(1:2+1:3)
#'
#' has_error(2-3); has_error(1+'a')
has_warning = function(expr) {
  warn = FALSE
  op = options(warn = -1); on.exit(options(op))
  withCallingHandlers(expr, warning = function(w) {
    warn <<- TRUE
    invokeRestart('muffleWarning')
  })
  warn
}
#' @export
#' @rdname has_message
has_error = function(expr) {
  inherits(try(force(expr), silent = !interactive()), 'try-error')
}
