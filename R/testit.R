#' Assertions with an optional message
#'
#' The function \code{assert()} was inspired by \code{\link{stopifnot}()}. It
#' emits a message in case of errors, which can be a helpful hint for diagnosing
#' the errors (\code{stopifnot()} only prints the possibly truncated source code
#' of the expressions).
#'
#' For the \code{...} argument, it should be a single R expression wrapped in
#' \code{{}}. This expression may contain multiple sub-expressions. A
#' sub-expression is treated as a test condition if it is wrapped in \code{()}
#' (meaning its value will be checked to see if it is a logical vector
#' containing any \code{FALSE} values) , otherwise it is evaluated in the normal
#' way and its value will not be checked. If the value of the last
#' sub-expression is logical, it will also be treated as a test condition.
#' @param fact A message for the assertions when any of them fails; treated the
#'   same way as expressions in \code{...} if it is not a character string,
#'   which means you are not required to provide a message to this function.
#' @param ... An R expression; see Details.
#' @return For \code{assert()}, invisible \code{NULL} if all expressions
#'   returned \code{TRUE}, otherwise an error is signaled and the user-provided
#'   message is emitted. For \code{\%==\%}, \code{TRUE} or \code{FALSE}.
#' @note The internal implementation of \code{assert()} is different with the
#'   \code{stopifnot()} function in R \pkg{base}: (1) the custom message
#'   \code{fact} is emitted if an error occurs; (2) \code{assert()} requires the
#'   logical values to be non-empty (\code{logical(0)} will trigger an error);
#'   (3) if \code{...} contains a compound expression in \code{{}} that returns
#'   \code{FALSE} (e.g., \code{if (TRUE) {1+1; FALSE}}), the first and the last
#'   but one line of the source code from \code{\link{deparse}()} are printed in
#'   the error message, otherwise the first line is printed; (4) the arguments
#'   in \code{...} are evaluated sequentially, and \code{assert()} will signal
#'   an error upon the first failed assertion, and will ignore the rest of
#'   assertions.
#' @export
#' @examples
#' library(testit)
#' assert('T is bad for TRUE, and so is F for FALSE', {T=FALSE;F=TRUE
#' (T!=TRUE)  # note the parentheses
#' (F!=FALSE)})
#'
#' assert('A Poisson random number is non-negative', {
#' x = rpois(1, 10)
#' (x >= 0)
#' (x > -1)  # () is optional because it's the last expression
#' })
assert = function(fact, ...) {
  opt = options(testit.asserting = TRUE); on.exit(options(opt), add = TRUE)
  mc = match.call()
  # match.call() uses the arg order in the func def, so fact is always 1st arg
  fact = NULL
  if (is.character(mc[[2]])) {
    fact = mc[[2]]; mc = mc[-2]
  }
  one = one_expression(mc)
  assert2(fact, if (one) mc[[2]][-1] else mc[-1], parent.frame(), !one)
}

# whether the argument of a function call is a single expression in {}
one_expression = function(call) {
  length(call) == 2 && length(call[[2]]) >= 1 && identical(call[[c(2, 1)]], as.symbol('{'))
}

assert2 = function(fact, exprs, envir, all = TRUE) {
  n = length(exprs)
  for (i in seq_len(n)) {
    expr = exprs[[i]]
    val = eval(expr, envir = envir, enclos = NULL)
    # special case: fact is an expression instead of a string constant in assert()
    if (is.null(fact) && all && i == 1 && is.character(val)) {
      fact = val; next
    }
    # check all values in case of multiple arguments, o/w only check values in ()
    if (all || (i == n && is.logical(val)) ||
        (length(expr) >= 1 && identical(expr[[1]], as.symbol('(')))) {
      if (all_true(val)) next
      if (!is.null(fact)) message('assertion failed: ', fact)
      stop(sprintf(
        ngettext(length(val), '%s is not TRUE', '%s are not all TRUE'),
        deparse_key(expr)
      ), call. = FALSE, domain = NA)
    }
  }
}

#' @description The infix operator \code{\%==\%} is simply an alias of the
#'   \code{\link{identical}()} function to make it slightly easier and intuitive
#'   to write test conditions. \code{x \%==\% y} is the same as
#'   \code{identical(x, y)}. When it is used inside \code{assert()}, a message
#'   will be printed if the returned value is not \code{TRUE}, to show the
#'   values of the LHS (\code{x}) and RHS (\code{y}) via \code{\link{str}()},
#'   which can be helpful for you to check why the assertion failed.
#' @param x,y two R objects to be compared
#' @rdname assert
#' @import utils
#' @export
`%==%` = function(x, y) {
  res = identical(x, y)
  if (!res && isTRUE(getOption('testit.asserting', FALSE))) {
    mc = match.call()
    info = paste(capture.output({
      cat(deparse_key(mc[[2]]), '(LHS) ==>\n')
      str(x)
      cat('----------\n')
      str(y)
      cat('<== (RHS)', deparse_key(mc[[3]]), '\n')
    }), collapse = '\n')
    message(info)
  }
  res
}

#' Run the tests of a package in its namespace
#'
#' The tests are executed in a clean environment with the namespace of the
#' package to be tested as the parent environment, which means you can use
#' non-exported objects in the package without having to resort to the triple
#' colon \code{\link{:::}} trick.
#'
#' The tests are assumed to be under the \file{testit/} or \file{tests/testit/}
#' directory by default (depending on your working directory is the package root
#' directory or the \file{tests/} directory). The test scripts must be named of
#' the form \samp{test-*.R} (or \samp{test-*.md} for snapshot tests); other
#' files will not be treated as test files (but may also be useful, e.g. you can
#' \code{\link{source}()} other scripts in tests).
#'
#' When a test is executed, the working directory is the same as the directory
#' containing this test, and all existing objects in the test environment will
#' be removed before the code is executed.
#'
#' See \url{https://pkg.yihui.org/testit/#snapshot-testing} for more details
#' about snapshot testing.
#' @param package The package name. By default, it is detected from the
#'   \file{DESCRIPTION} file if exists.
#' @param dir The directory of the test files; by default, it is the directory
#'   \file{testit/} or \file{tests/testit/} under the current working directory,
#'   whichever exists. You can also specify a custom directory.
#' @param update If \code{TRUE}, update snapshot files with actual output
#'   instead of comparing. If \code{NA} (the default), update snapshot files
#'   only if they are tracked by GIT (so you can view the diffs in GIT and
#'   decide whether to accept or discard the changes). If \code{FALSE}, never
#'   update snapshot files and always compare. For \code{NA} and \code{FALSE},
#'   if the snapshot test fails, it will throw an error with a message showing
#'   the location of the failed test. For \code{TRUE}, it will update the
#'   snapshot file and never throw an error.
#' @return \code{NULL}. All test files are executed, unless an error occurs.
#' @note All test scripts must be encoded in UTF-8 if they contain any multibyte
#'   characters.
#' @export
#' @examples \dontrun{test_pkg('testit')}
test_pkg = function(package = pkg_name(), dir = c('testit', 'tests/testit'), update = NA) {
  # install the source package before running tests when this function is called
  # in a non-interactive R session that is not `R CMD check`
  install = !.env$installed && !interactive() &&
    is.na(Sys.getenv('_R_CHECK_PACKAGE_NAME_', NA)) && package == pkg_name()
  if (install) {
    .env$lib_old = lib_old = .libPaths()
    .env$lib_new = lib_new = tempfile('R-lib-', '.'); dir.create(lib_new)
    res = system2(
      file.path(R.home('bin'), 'R'), c(
        'CMD', 'INSTALL', paste0('--library=', lib_new),
        '--no-help', '--no-staged-install', '--no-test-load', '..'
      )
    )
    if (res == 0) {
      .libPaths(c(lib_new, lib_old))
      .env$installed = TRUE
      if (!is.na(i <- match(paste0('package:', package), search())))
        detach(pos = i, unload = TRUE, force = TRUE)
    }
  }

  path = available_dir(dir)
  fs = list.files(path, full.names = TRUE)
  # clean up new files/dirs generated during testing
  if (getOption('testit.cleanup', TRUE)) on.exit({
    unlink(setdiff(list.files(path, full.names = TRUE), fs), recursive = TRUE)
  }, add = TRUE)
  rs = fs[grep('^test-.+[.][rR]$', basename(fs))]
  ms = fs[grep('^test-.+[.]md$', basename(fs))]
  wd = getwd()

  # make all objects in the package visible to tests
  env = new.env(parent = getNamespace(package))
  for (r in rs) {
    rm(list = ls(env, all.names = TRUE), envir = env)
    withCallingHandlers(
      sys.source2(r, envir = env, top.env = getNamespace(package)),
      error = function(e) {
        z = if (exists('.traceback', baseenv(), inherits = FALSE)) .traceback(5)
        if (length(z) == 0) return()
        z = z[[1]]
        n = length(z)
        s = if (!is.null(srcref <- attr(z, 'srcref')))
          error_loc(attr(srcref, 'srcfile')$filename, srcref[1], wd)
        cat('Error from', z[1], if (n > 1) '...', s, '\n')
      }
    )
  }

  # run snapshot tests from markdown files
  test_snaps(ms, env, update)
}

# add ANSI link on file path if supported
error_loc = function(x, line = 1, wd = '.') {
  if (!length(x)) return()
  if (!isTRUE(as.logical(Sys.getenv('RSTUDIO_CLI_HYPERLINKS'))))
    return(sprintf(' at %s#%d', x, line))
  full = normalizePath(if (file.exists(x)) x else file.path(wd, x), '/')
  sprintf(' at \033]8;line = %d:col = 1;file://%s\a%s#%d\033]8;;\a', line, full, x, line)
}

#' Check if an R expression produces warnings or errors
#'
#' The two functions \code{has_warning()} and \code{has_error()} check if an
#' expression produces warnings and errors, respectively.
#' @param expr an R expression
#' @param silent logical: should the report of error messages be suppressed?
#' @return A logical value.
#' @export
#' @rdname has_message
#' @examples has_warning(1+1); has_warning(1:2+1:3)
#'
#' has_error(2-3); has_error(1+'a'); has_error(stop("err"), silent = TRUE)
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
has_error = function(expr, silent = !interactive()) {
  inherits(try(force(expr), silent = silent), 'try-error')
}
