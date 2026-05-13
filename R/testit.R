#' Assert that conditions are true, with an informative failure message
#'
#' Test that one or more conditions are `TRUE`. If any condition fails, an error
#' is raised with the `fact` message, making it easy to identify which test
#' failed and why. This is the primary function for writing tests with
#' **testit**.
#'
#' The recommended usage is to pass a single expression wrapped in `{}` as the
#' second argument. Inside `{}`, any **statement-level** sub-expression wrapped
#' in parentheses `()` is treated as a test condition -- its value is checked
#' and must be `TRUE`. Parentheses used for grouping within a larger expression
#' (e.g., `(a + b) * c`) are not checked. Sub-expressions *without* parentheses
#' are ordinary R code (e.g., variable assignments or setup steps) and are never
#' checked.
#'
#' `()` tests work inside `if`, `for`, `while`, and `repeat` bodies. Internally,
#' `assert()` walks the expression tree and transforms statement-level `()` into
#' checks before evaluating the entire block in one frame (so `on.exit()` works
#' as expected).
#' @param fact A character string describing what is being tested. This message
#'   is shown when an assertion fails, so make it descriptive (e.g., `'log()
#'   returns correct values'`). If `fact` is not a character string, it is
#'   treated as a test expression (i.e., the message is optional).
#' @param ... An R expression wrapped in `{}`; see Details.
#' @return Invisible `NULL` if all conditions pass. If any condition fails, an
#'   error is signaled that includes the `fact` message and the expression that
#'   failed. For `%==%`, `TRUE` or `FALSE`.
#' @note Key differences from [stopifnot()]:
#'
#' - `assert()` shows your custom `fact` message on failure, making errors
#'   easier to diagnose.
#' - `logical(0)` (empty logical) is treated as a failure, not a pass.
#' - All conditions are evaluated even if earlier ones fail; all failures are
#'   reported together in a single error message.
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
#'   (x > -1)
#' })
#'
#' # () works inside control structures too
#' assert('conditional test', {
#'   if (requireNamespace('base', quietly = TRUE)) (1 + 1 == 2)
#' })
assert = function(fact, ...) {
  opt = options(testit.asserting = TRUE); on.exit(options(opt), add = TRUE)
  mc = match.call()
  msg = if (is.character(fact)) {
    mc = mc[-2]; fact
  }
  one = length(mc) == 2 && length(mc[[2]]) >= 1 &&
    identical(mc[[c(2, 1)]], as.symbol('{'))
  expr = if (one) mc[[2]] else
    as.call(c(list(as.symbol('{')), lapply(as.list(mc[-1]), function(x) call('(', x))))
  assert_exec(msg, transform_assert(expr), parent.frame())
}

# indices of body sub-expressions for each control-flow construct
.assert_body_idx = list('if' = 3:4, 'for' = 4L, 'while' = 3L, 'repeat' = 2L)

# walk AST to replace statement-level ( with .testit_check
transform_assert = function(expr) {
  if (!is.call(expr)) return(expr)
  head = expr[[1]]
  if (identical(head, as.symbol('('))) {
    expr[[1]] = as.symbol('.testit_check')
  } else if (identical(head, as.symbol('{'))) {
    for (i in seq_along(expr)[-1]) expr[[i]] = transform_assert(expr[[i]])
  } else if (is.symbol(head)) {
    for (i in intersect(.assert_body_idx[[as.character(head)]], seq_along(expr)))
      expr[[i]] = transform_assert(expr[[i]])
  }
  expr
}

assert_exec = function(fact, expr, envir) {
  errs = NULL
  .env$equ_info = NULL
  on.exit(.env$equ_info <- NULL, add = TRUE)
  e = new.env(parent = envir)
  e[['.testit_check']] = function(val) {
    if (!all_true(val)) {
      sc = sys.call()
      ec = sc[[2]]
      info = if (length(.env$equ_info)) one_string(.env$equ_info)
      sr = attr(sc, 'srcref')
      loc = if (!is.null(sr)) error_loc(attr(sr, 'srcfile')$filename, sr[1])
      errs <<- c(errs, paste0(one_string(c(info, sprintf(
        ngettext(length(val), '%s is not TRUE', '%s are not all TRUE'),
        deparse_key(ec)
      ))), ' but ', deparse_one(val), loc))
    }
    .env$equ_info = NULL
    val
  }
  eval(expr, envir = e)
  if (length(errs)) {
    header = if (!is.null(fact)) paste0('-- Assertion failed: ', fact, ' --') else
      '-- Assertion failed --'
    n = length(errs)
    body = if (n == 1) paste0('   ', strsplit(errs, '\n')[[1]]) else {
      pad = nchar(n) + 5L
      vapply(seq_along(errs), function(i) {
        lines = strsplit(errs[i], '\n')[[1]]
        lines[-1] = paste0(one_string(rep(' ', pad), ''), lines[-1])
        paste0('   ', sprintf(paste0('%', nchar(n), 'd'), i), '. ', one_string(lines))
      }, '')
    }
    stop_errs(one_string(c(header, body)), check = FALSE)
  }
}

#' @description The infix operator `%==%` is a shortcut for [identical()] that
#'   provides helpful diagnostics on failure. `x %==% y` returns `TRUE` if `x`
#'   and `y` are identical, and `FALSE` otherwise. When used inside `assert()`,
#'   a failing `%==%` comparison will display both values via [str()] so you can
#'   see exactly what differed.
#' @param x,y Two R objects to be compared for identity.
#' @rdname assert
#' @import utils
#' @export
`%==%` = function(x, y) {
  res = identical(x, y)
  if (!res && getOption('testit.asserting', FALSE)) {
    mc = match.call()
    sx = capture.output(str(x)); sy = capture.output(str(y))
    nx = length(sx); ny = length(sy)
    dx = deparse_key(mc[[2]]); dy = deparse_key(mc[[3]])
    show_diff = (nx + ny > 10L) && length(d <- mini_diff(sx, sy, 50L))
    info = one_string(if (show_diff) {
      c('Structure diff:', paste(dx, '(- LHS) vs', dy, '(+ RHS):'), d)
    } else {
      sx = trunc_vec(sx, 10L, nx); sy = trunc_vec(sy, 10L, ny)
      c(paste(dx, '(LHS) ==>'), sx, '----------', sy, paste('<== (RHS)', dy))
    })
    # show deparse diff only when str() is uninformative (identical for both)
    if (identical(sx, sy)) {
      diff = deparse_diff(x, y)
      if (length(diff))
        info = one_string(c(info, '', 'Detailed diff (- LHS, + RHS):', diff))
    }
    .env$equ_info = c(.env$equ_info, info)
  }
  res
}

trunc_vec = function(x, n, N) if (N > n) c(head(x, n), '...') else x

#' Run all tests for a package
#'
#' Discover and execute test files (`test-*.R` and `test-*.md`) for a package.
#' Tests are run inside the package namespace, so you can call internal
#' (non-exported) functions directly without the `:::` operator.
#'
#' Test files are looked up in the `testit/` or `tests/testit/` directory by
#' default. Files must be named `test-*.R` for regular tests or `test-*.md` for
#' snapshot tests. Other files in the directory are ignored (but you can
#' [source()] them from your tests if needed).
#'
#' Helper files named `helper*.R` (e.g., `helper.R`, `helper-utils.R`) are
#' sourced before any test file runs. Objects defined in helpers are available
#' to all tests.
#'
#' Each test file runs in a clean environment (previous test objects are
#' removed), and the working directory is set to the directory containing the
#' test file.
#'
#' See <https://pkg.yihui.org/testit/#sec:snapshot-testing> for more details
#' about snapshot testing.
#' @param package The package name. By default, it is detected from the
#'   `DESCRIPTION` file.
#' @param dir The directory containing test files. If `NULL` (the default),
#'   `testit/` or `tests/testit/` under the current working directory is used
#'   (whichever exists). You can also pass a custom path.
#' @param filter An optional regular expression to select a subset of test
#'   files. Only files whose names match the pattern will be run. For example,
#'   `filter = "plot"` runs only test files with "plot" in their names.
#' @param update Controls snapshot file behavior:
#'   - `TRUE`: always update snapshot files with actual output (never errors).
#'   - `NA` (default): update only if the file is tracked by Git (so you can
#'     review diffs before accepting).
#'   - `FALSE`: never update; always compare and error on mismatch.
#' @return Invisible `NULL`. If any tests fail, a single error is thrown at the
#'   end with all failure messages combined.
#' @note You must call `library(testit)` before `test_pkg()`. Test scripts use
#'   [assert()] and other **testit** functions without the `testit::` prefix, so
#'   the package needs to be on the search path. Without `library(testit)`, you
#'   will get "could not find function" errors.
#'
#'   All test scripts must be encoded in UTF-8 if they contain multibyte
#'   characters.
#'
#'   When `filter` or `update` are not explicitly provided, `test_pkg()` checks
#'   `commandArgs(TRUE)` for command-line arguments: `--filter=PATTERN` sets the
#'   filter, and `--update` sets `update = TRUE`. This allows you to pass these
#'   options via `Rscript tests/*.R --filter=PATTERN --update` without modifying
#'   individual `test_pkg()` calls.
#' @export
#' @examples
#' \dontrun{
#' library(testit)
#' test_pkg('testit')
#' }
test_pkg = function(package = pkg_name(), dir = NULL, filter = NULL, update = NA) {
  args = parse_args()
  if (missing(filter)) filter = args$filter
  if (missing(update)) update = args$update
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
  # run helper scripts
  run_files(hs, TRUE); stop_errs(errs)

  # run test scripts and snapshots
  run_files(rs); run_files(ms, snap = TRUE); stop_errs(errs)
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

#' Test whether an expression signals a condition
#'
#' Check if evaluating an expression produces a message, warning, or error.
#' These functions are designed to be used inside [assert()] to verify that code
#' signals the expected conditions. Optionally, you can match against the
#' condition's text to ensure the *right* message/warning/error was signaled.
#' @param expr An R expression to evaluate.
#' @param message An optional string to match against the condition text. Uses
#'   fixed (literal) matching by default. If provided, the function returns
#'   `TRUE` only when the condition is signaled *and* the message matches.
#' @param ... Additional arguments passed to [grepl()] for matching (e.g.,
#'   `fixed = FALSE` to use regex, or `ignore.case = TRUE`). Note that
#'   `fixed = TRUE` is the default.
#' @return `TRUE` if the condition was signaled (and the message matched, if
#'   provided), `FALSE` otherwise.
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

grepl2 = function(..., fixed = TRUE, ignore.case = FALSE) {
  if (ignore.case && fixed) fixed = FALSE
  grepl(..., fixed = fixed, ignore.case = ignore.case)
}

quietly = function(expr) {
  withCallingHandlers(
    expr,
    message = function(m) invokeRestart('muffleMessage'),
    warning = function(w) invokeRestart('muffleWarning')
  )
}

