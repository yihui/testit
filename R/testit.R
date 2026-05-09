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
#' assert('T is bad for TRUE, and so is F for FALSE', {
#'   T = FALSE; F = TRUE
#'   (T != TRUE)  # note the parentheses
#'   (F != FALSE)
#' })
#'
#' assert('A Poisson random number is non-negative', {
#'   x = rpois(1, 10)
#'   (x >= 0)
#'   (x > -1)  # () is optional because it's the last expression
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
  assert2(
    fact, if (one) mc[[2]][-1] else mc[-1], parent.frame(), !one,
    assert_loc(sys.call(), one)
  )
}

# whether the argument of a function call is a single expression in {}
one_expression = function(call) {
  length(call) == 2 && length(call[[2]]) >= 1 && identical(call[[c(2, 1)]], as.symbol('{'))
}

# get error location info for assert(): file, start line, and per-expression offsets
assert_loc = function(call, one) {
  sr = getSrcref(call)
  if (is.null(sr)) return()
  sf = attr(sr, 'srcfile')
  file = sf$filename
  if (file.exists(file)) file = norm_path(file)
  src = getSrcLines(sf, sr[1], sr[3])
  if (!one) return(list(file = file, lines = rep(sr[1], length(call) - 1)))
  # parse the {} body to find relative line numbers of sub-expressions
  body_lines = src[-c(1, length(src))]
  body_exprs = if (length(body_lines))
    tryCatch(parse(text = body_lines, keep.source = TRUE), error = function(e) NULL)
  body_sr = if (!is.null(body_exprs)) attr(body_exprs, 'srcref')
  lines = if (is.null(body_sr)) sr[1] else
    vapply(body_sr, function(s) sr[1] + s[1], integer(1))
  list(file = file, lines = lines)
}

assert2 = function(fact, exprs, envir, all = TRUE, loc = NULL) {
  .env$equ_info = NULL
  on.exit(.env$equ_info <- NULL, add = TRUE)
  n = length(exprs)
  errs = NULL
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
      if (all_true(val)) { .env$equ_info = NULL; next }
      info = c(
        if (!is.null(fact)) paste0('assertion failed: ', fact),
        if (length(.env$equ_info)) paste(.env$equ_info, collapse = '\n')
      )
      s = if (!is.null(loc)) error_loc(loc$file, loc$lines[min(i, length(loc$lines))])
      errs = c(errs, paste0(paste(c(info, sprintf(
        ngettext(length(val), '%s is not TRUE', '%s are not all TRUE'),
        deparse_key(expr)
      )), collapse = '\n'), ' but ', deparse_one(val), s))
      .env$equ_info = NULL
    }
  }
  if (length(errs)) stop(paste(errs, collapse = '\n'), call. = FALSE, domain = NA)
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
  if (!res && getOption('testit.asserting', FALSE)) {
    mc = match.call()
    sx = capture.output(str(x))
    sy = capture.output(str(y))
    info = paste(c(
      paste(deparse_key(mc[[2]]), '(LHS) ==>'), sx, '----------', sy,
      paste('<== (RHS)', deparse_key(mc[[3]]))
    ), collapse = '\n')
    # show deparse diff only when str() is uninformative (identical for both)
    if (identical(sx, sy)) {
      diff = deparse_diff(x, y)
      if (length(diff))
        info = paste(c(info, '', 'Detailed diff (- LHS, + RHS):', diff), collapse = '\n')
    }
    .env$equ_info = c(.env$equ_info, info)
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
#' Helper files named \samp{helper*.R} (e.g., \samp{helper.R},
#' \samp{helper-utils.R}) are sourced before test files and remain available
#' to all tests, allowing you to define shared utility functions.
#'
#' When a test is executed, the working directory is the same as the directory
#' containing this test, and all existing objects in the test environment will
#' be removed before the code is executed (except for helper functions).
#'
#' See \url{https://pkg.yihui.org/testit/#snapshot-testing} for more details
#' about snapshot testing.
#' @param package The package name. By default, it is detected from the
#'   \file{DESCRIPTION} file if exists.
#' @param dir The directory of the test files. If \code{NULL} (the default), the
#'   directory \file{testit/} or \file{tests/testit/} under the current working
#'   directory is used (whichever exists). You can also specify a custom
#'   directory.
#' @param filter An optional regular expression to select a subset of test
#'   files. Only files whose names (without directory) match the pattern will be
#'   run. For example, \code{filter = "parse"} runs only test files with
#'   \dQuote{parse} in their names.
#' @param update If \code{TRUE}, update snapshot files with actual output
#'   instead of comparing. If \code{NA} (the default), update snapshot files
#'   only if they are tracked by GIT (so you can view the diffs in GIT and
#'   decide whether to accept or discard the changes). If \code{FALSE}, never
#'   update snapshot files and always compare. For \code{NA} and \code{FALSE},
#'   if the snapshot test fails, it will throw an error with a message showing
#'   the location of the failed test. For \code{TRUE}, it will update the
#'   snapshot file and never throw an error.
#' @return \code{NULL}. All test files are executed and errors are collected; if
#'   any tests fail, a single error is thrown at the end with all failure
#'   messages combined.
#' @note The \pkg{testit} package must be loaded (e.g., via
#'   \code{library(testit)}) before calling \code{test_pkg()}, because test
#'   scripts typically call \code{\link{assert}()} and other \pkg{testit}
#'   functions directly without the \code{testit::} prefix. Simply using
#'   \code{testit::test_pkg()} without loading the package first will result in
#'   errors like \dQuote{could not find function "assert"}.
#'
#' All test scripts must be encoded in UTF-8 if they contain any multibyte
#'   characters.
#' @export
#' @examples \dontrun{
#' library(testit)
#' test_pkg('testit')
#' }
test_pkg = function(package = pkg_name(), dir = NULL, filter = NULL, update = NA) {
  # install the source package before running tests when this function is called
  # in a non-interactive R session that is not `R CMD check`
  pkg_root = if (file.exists('DESCRIPTION')) '.' else if (file.exists('../DESCRIPTION')) '..'
  install = !interactive() &&
    is.na(Sys.getenv('_R_CHECK_PACKAGE_NAME_', NA)) &&
    !is.null(pkg_root) && package == pkg_name() &&
    pkg_needs_install(pkg_root, package)
  if (install) {
    .env$lib_old = lib_old = .libPaths()
    dir.create(lib_new <- tempfile('R-lib-'))
    .env$lib_new = norm_path(lib_new)
    message(
      "Installing '", package, "' to ", short_temp(lib_new), ' for testing... ',
      appendLF = FALSE
    )
    res = system2(
      file.path(R.home('bin'), 'R'), c(
        'CMD', 'INSTALL', paste0('--library=', lib_new),
        '--no-help', '--no-staged-install', '--no-test-load', pkg_root
      ), stdout = FALSE, stderr = FALSE
    )
    message(if (res == 0) {
      .libPaths(c(lib_new, lib_old))
      if (!is.na(i <- match(paste0('package:', package), search())))
        detach(pos = i, unload = TRUE, force = TRUE)
      'Done.'
    } else 'Failed.')
  }

  if (is.null(dir)) dir = c('testit', 'tests/testit')
  if (identical(pkg_root, '.')) dir = c(dir, file.path('tests', dir))
  path = available_dir(dir)
  td = paste0(norm_path(getwd()), '/')
  op = options(testit.test_dir = td); on.exit(options(op), add = TRUE)
  fs = list.files(path, full.names = TRUE)
  # clean up new files/dirs generated during testing
  if (getOption('testit.cleanup', TRUE)) on.exit({
    unlink(setdiff(list.files(path, full.names = TRUE), fs), recursive = TRUE)
  }, add = TRUE)
  rs = fs[grep('^test-.+[.][rR]$', basename(fs))]
  ms = fs[grep('^test-.+[.]md$', basename(fs))]
  if (!is.null(filter)) {
    rs = rs[grep(filter, basename(rs))]
    ms = ms[grep(filter, basename(ms))]
  }
  hs = fs[grep('^helper.*[.][rR]$', basename(fs))]

  # source helpers into a dedicated environment; tests inherit from it
  ns = getNamespace(package)
  henv = new.env(parent = ns)
  env = new.env(parent = henv)
  errs = NULL
  # run each file, print the relative path, and collect error messages
  run_files = function(files, helper = FALSE, snap = FALSE) for (f in files) {
    if (!helper) {
      message('Testing ', sub(td, '', f, fixed = TRUE), '... ', appendLF = FALSE)
      rm(list = ls(env, all.names = TRUE), envir = env)
    }
    err = if (snap) test_snap(f, env, update) else
      quietly(sys.source2(f, envir = if (helper) henv else env, top.env = ns))
    if (!helper) message(if (length(err) == 0) 'OK' else 'FAILED')
    errs <<- c(errs, err)
  }
  throw = function() {
    if (length(errs)) stop(paste(errs, collapse = '\n'), call. = FALSE)
  }

  # run helper scripts
  run_files(hs, TRUE); throw()

  # run test scripts and snapshots
  run_files(rs); run_files(ms, snap = TRUE); throw()
}

# evaluate expr; on error, append source location to the error message and re-throw
loc_stop = function(expr) {
  loc = NULL
  tryCatch(withCallingHandlers(expr, error = function(e) {
    if (!exists('.traceback', baseenv(), inherits = FALSE)) return()
    for (skip in 0:20) {
      z = .traceback(skip)
      if (length(z) == 0) break
      sr = attr(z[[1]], 'srcref')
      if (!is.null(sr)) {
        loc <<- error_loc(attr(sr, 'srcfile')$filename, sr[1])
        break
      }
    }
  }), error = function(e) {
    msg = conditionMessage(e)
    if (!is.null(loc) && !grepl(' at .+#\\d+', msg)) msg = paste0(msg, loc)
    stop(msg, call. = FALSE)
  })
}

# add ANSI link on file path if supported
error_loc = function(x, line = 1) {
  if (!length(x)) return()
  if (!file.exists(x)) return(sprintf(' at %s#%d', x, line))
  full = norm_path(x)
  d = getOption('testit.test_dir')
  n = nchar(d)
  rel = if (!is.null(d) && starts_with(full, d)) substring(full, n + 1) else full
  if (!isTRUE(as.logical(Sys.getenv('RSTUDIO_CLI_HYPERLINKS'))))
    return(sprintf(' at %s#%d', rel, line))
  sprintf(' at \033]8;line = %d:col = 1;file://%s\a%s#%d\033]8;;\a', line, full, rel, line)
}

#' Check if an R expression produces messages, warnings, or errors
#'
#' The functions \code{has_message()}, \code{has_warning()}, and
#' \code{has_error()} check if an expression produces messages, warnings, and
#' errors, respectively. When the \code{message} argument is provided, the
#' condition message is matched against it via \code{\link{grepl}()}.
#' @param expr an R expression
#' @param message optionally, a string (fixed or regex pattern) to match against
#'   the condition message. If provided, the function returns \code{TRUE} only
#'   when the condition is signaled \emph{and} the message matches.
#' @param ... additional arguments passed to \code{\link{grepl}()} for matching
#'   \code{message} against the condition message (e.g., \code{fixed = TRUE} for
#'   fixed string matching, or \code{ignore.case = TRUE}). Note that \code{fixed
#'   = TRUE} is the default.
#' @return A logical value.
#' @export
#' @rdname has_message
#' @examples
#' has_message(message('hello'))
#' has_message(1 + 1)
#' has_message(message('hello world'), 'hello')
#'
#' has_warning(1 + 1)
#' has_warning(1:2 + 1:3)
#' has_warning(1:2 + 1:3, 'longer object length')
#'
#' has_error(2 - 3)
#' has_error(1 + 'a')
#' has_error(stop('err'), 'err')
#' has_error(stop('error occurred'), 'error')
has_message = function(expr, message = NULL, ...) {
  msg_text = NULL
  withCallingHandlers(expr, message = function(m) {
    msg_text <<- paste0(c(msg_text, conditionMessage(m)), collapse = '')
    invokeRestart('muffleMessage')
  })
  match_cond(msg_text, message, ...)
}
#' @export
#' @rdname has_message
has_warning = function(expr, message = NULL, ...) {
  warn_text = NULL
  op = options(warn = -1); on.exit(options(op))
  withCallingHandlers(expr, warning = function(w) {
    warn_text <<- paste0(c(warn_text, conditionMessage(w)), collapse = '')
    invokeRestart('muffleWarning')
  })
  match_cond(warn_text, message, ...)
}
#' @export
#' @rdname has_message
has_error = function(expr, message = NULL, ...) {
  tryCatch({
    expr; FALSE
  }, error = function(e) {
    match_cond(conditionMessage(e), message, ...)
  })
}

match_cond = function(text, message, ...) {
  if (is.null(text)) FALSE else if (is.null(message)) TRUE else
    grepl2(message, text, ...)
}

grepl2 = function(..., fixed = TRUE) grepl(..., fixed = fixed)

quietly = function(expr) {
  withCallingHandlers(
    expr,
    message = function(m) invokeRestart('muffleMessage'),
    warning = function(w) invokeRestart('muffleWarning')
  )
}

