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
  (!silence(1 %==% 1L))
})

assert('has_warning() works', {
  (has_warning(warning('An intentional warning')))
  (has_warning((function() {1:2 + 1:3})()))
})

assert('has_error() works', {
  (has_error(stop('An intentional error')))
  (has_error(1 + 'a'))
})

assert('has_error() can suppress error message', {
  (has_error(stop('An intentional error'), silent = TRUE))
  (has_error(1 + 'a', silent = FALSE))
})

assert('tests can be written in () in a single {}', {

  (1 == 1L)

  z = 1:10
  (rev(z) %==% 10:1)

  !!TRUE

})

assert('assert() treats a non-string first arg as an expression (fact-as-expression)', {
  # when fact is not a character literal, assert2 detects fact=val at i==1
  (has_error(assert({x = 'fact msg'; x}, 1 == 2), silent = TRUE))
})

assert('%==% emits diagnostic info on failure inside assert()', {
  # trigger the %==% failure message branch
  msg = tryCatch(
    assert('check %==% message', { (1 %==% 2) }),
    error = function(e) conditionMessage(e)
  )
  (grepl('not TRUE', msg))
})

assert('has_error() with silent = FALSE prints the error message', {
  # cover line 254: cat('Error: ', ...)
  out = capture.output(res <- has_error(stop('visible error'), silent = FALSE))
  (res)
  (any(grepl('visible error', out)))
})

assert('error_loc() returns NULL for empty input', {
  (error_loc(character(0)) %==% NULL)
})

assert('has_error() returns FALSE for non-error with silent = FALSE', {
  (!has_error(1 + 1, silent = FALSE))
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
