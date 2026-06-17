library(testit)

assert('assert works', {
  (1 == 1)
})

# Okay, that is kind of cheating
assert('assert() should signal an error if a condition does not hold', {
  (has_error(assert('this should produce an error', 1 == 2)))
})

# a meaningless test in terms of R (failure is irrelevant to Frequentist or Bayesian)
has_error(assert('Frequentists must be correct (http://xkcd.com/1132/): the sun has exploded!', {
  (sample(6, 2) == c(6, 6))
}))

# fail logical(0)
assert('assert() should stop on logical(0)', {
  (has_error(assert('1 equals integer(0)', 1 == integer(0))))
})

assert('the infix operator %==% works', {
  (1 %==% 1)
  (!(1 %==% 1L))
})

assert('has_message() works', {
  (has_message(message('hello')))
  (!has_message(1 + 1))
  (has_message(message('hello world'), 'hello'))
  (!has_message(message('hello world'), 'bye'))
  (has_message(message('Hello World'), 'hello', ignore.case = TRUE))
})

assert('has_warning() works', {
  (has_warning(warning('An intentional warning')))
  (has_warning((function() {1:2 + 1:3})()))
  (has_warning(warning('longer object'), 'longer'))
  (!has_warning(warning('something'), 'else'))
  (has_warning(warning('Longer Object'), 'longer', ignore.case = TRUE))
})

assert('has_error() works', {
  (has_error(stop('An intentional error')))
  (has_error(1 + 'a'))
  (has_error(stop('error occurred'), 'error'))
  (!has_error(stop('oops'), 'different'))
  (has_error(stop('Error Occurred'), 'error', ignore.case = TRUE))
})

assert('has_error() works without message matching', {
  (has_error(stop('An intentional error')))
  (!has_error(1 + 1))
})

assert('tests can be written in () in a single {}', {
  (1 == 1L)

  z = 1:10
  (rev(z) %==% 10:1)
})

assert('() works inside control structures', {
  if (TRUE) (1 == 1)
  for (i in 1:3) (i > 0)
  # prove that () inside if/for actually triggers checks (not just grouping)
  (has_error(assert('if body', { if (TRUE) (1 == 2) }), '1 == 2'))
  (has_error(assert('for body', { for (i in 1) (i == 0) }), 'i == 0'))
})

assert('assert() handles non-symbol call heads (e.g., obj$method())', {
  env = list2env(list(x = 1))
  # $ calls and [[ calls should not confuse the AST walker
  (env$x %==% 1)
  l = list(f = function() TRUE)
  (l$f())
  (l[['f']]())
})

assert('assert() treats a non-string first arg as an expression (fact-as-expression)', {
  # when fact is not a character literal, assert2 detects fact=val at i==1
  (has_error(assert({x = 'fact msg'; x}, 1 == 2)))
})

assert('%==% emits diagnostic info on failure inside assert()', {
  # trigger the %==% failure message branch
  msg = tryCatch(
    assert('check %==% message', { (1 %==% 2) }),
    error = function(e) conditionMessage(e)
  )
  (grepl('not TRUE but FALSE', msg))
})

assert('error_loc() returns NULL for empty input', {
  (error_loc(character(0)) %==% NULL)
})

# test error_loc() outside assert() to allow on.exit() to work properly
local({
  old = Sys.getenv('RSTUDIO_CLI_HYPERLINKS', unset = NA)
  Sys.setenv(RSTUDIO_CLI_HYPERLINKS = 'false')
  on.exit(if (is.na(old)) Sys.unsetenv('RSTUDIO_CLI_HYPERLINKS') else
    Sys.setenv(RSTUDIO_CLI_HYPERLINKS = old))
  loc = error_loc('test.R', 5)
  assert('error_loc() formats location without ANSI when env var is not set', {
    (loc %==% ' at test.R#5')
  })
})

local({
  old = Sys.getenv('RSTUDIO_CLI_HYPERLINKS', unset = NA)
  Sys.setenv(RSTUDIO_CLI_HYPERLINKS = 'TRUE')
  on.exit(if (is.na(old)) Sys.unsetenv('RSTUDIO_CLI_HYPERLINKS') else
    Sys.setenv(RSTUDIO_CLI_HYPERLINKS = old))
  f = tempfile(fileext = '.R'); writeLines('x', f)
  loc = error_loc(f, 3)
  assert('error_loc() formats ANSI link when RSTUDIO_CLI_HYPERLINKS is TRUE', {
    (grepl('\033]8;', loc))
    (grepl('#3', loc))
  })
})

assert('assert() captures all failures, not just the first', {
  msg = tryCatch(
    assert('multi', { (1 == 2); (1 == 0) }),
    error = function(e) conditionMessage(e)
  )
  (grepl('1 == 2', msg))
  (grepl('1 == 0', msg))
  # multi-argument form
  msg2 = tryCatch(
    assert('multi arg', 1 == 2, 1 == 0),
    error = function(e) conditionMessage(e)
  )
  (grepl('1 == 2', msg2))
  (grepl('1 == 0', msg2))
})

assert('stop_errs() throws a short summary when message exceeds warning.length', {
  op = options(warning.length = 200L)
  msgs = c(
    paste(rep('x', 100), collapse = ''),
    paste(rep('y', 100), collapse = ''),
    paste(rep('z', 100), collapse = '')
  )
  err = tryCatch(stop_errs(msgs), error = function(e) conditionMessage(e))
  options(op)
  (err %==% '3 tests failed (see details above)')
})

assert('stop_errs() throws full message when it fits in warning.length', {
  msgs = c('error 1', 'error 2', 'error 3')
  err = tryCatch(stop_errs(msgs), error = function(e) conditionMessage(e))
  (grepl('error 1', err))
  (grepl('error 2', err))
  (grepl('error 3', err))
})

assert('assert() error includes precise line number of failing () expression', {
  f = tempfile(fileext = '.R')
  writeLines(c(
    'library(testit)',
    'assert("loc test", {',
    '  x = 1',
    '  (x == 2)',
    '})'
  ), f)
  msg = tryCatch(
    sys.source(f, envir = new.env(parent = .GlobalEnv), keep.source = TRUE),
    error = function(e) conditionMessage(e)
  )
  # line 4 is where (x == 2) lives
  (grepl('#4', msg))
})

assert('helper functions are available in tests', {
  (is_true(1 == 1))
  (!is_true(1 == 2))
})

assert('global helpers from parent directory are sourced', {
  d = tempfile()
  dir.create(sub_dir <- file.path(d, 'testit'), recursive = TRUE)
  # global helper in parent
  writeLines('global_val = 42', file.path(d, 'helper-global.R'))
  # local helper in subdir
  writeLines('local_val = 99', file.path(sub_dir, 'helper-local.R'))
  # test file that uses both helpers
  writeLines(c(
    'library(testit)',
    'assert("globals and locals", {',
    '  (identical(global_val, 42))',
    '  (identical(local_val, 99))',
    '})'
  ), file.path(sub_dir, 'test-check.R'))
  writeLines(c('Package: testit', 'Version: 0.0.1'), file.path(d, 'DESCRIPTION'))
  owd = setwd(d); on.exit(setwd(owd))
  # skip reinstall by faking R CMD check context
  old_env = Sys.getenv('_R_CHECK_PACKAGE_NAME_', unset = NA)
  Sys.setenv('_R_CHECK_PACKAGE_NAME_' = 'testit')
  on.exit({
    if (is.na(old_env)) Sys.unsetenv('_R_CHECK_PACKAGE_NAME_') else
      Sys.setenv('_R_CHECK_PACKAGE_NAME_' = old_env)
  }, add = TRUE)
  (!has_error(test_pkg('testit', dir = 'testit')))
})
