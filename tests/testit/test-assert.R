library(testit)

assert('assert works', 1 == 1)

# Okay, that is kind of cheating
assert(
  'assert() should signal an error if a condition does not hold',
  has_error(assert('this should produce an error', 1 == 2))
)

# a meaningless test in terms of R (failure is irrelevant to Frequentist or Bayesian)
try(assert(
  'Frequentists must be correct (http://xkcd.com/1132/)',
  'The sun has exploded!', sample(6, 2) == c(6, 6)
), silent = !interactive())

# fail logical(0)
assert(
  'assert() should stop on logical(0)',
  has_error(assert('1 equals integer(0)', 1 == integer(0)))
)

assert(
  'the infix operator %==% works',
  1 %==% 1, !(1 %==% 1L)
)

assert(
  'has_warning() works',
  has_warning(warning('An intentional warning')),
  has_warning((function() {1:2 + 1:3})())
)

assert(
  'has_error() works',
  has_error(stop('An intentional error')),
  has_error(1 + 'a')
)

assert(
  'has_error() can suppress error message',
  has_error(stop('An intentional error'), silent = TRUE),
  has_error(1 + 'a', silent = FALSE)
)

assert('tests can be written in () in a single {}', {

  (1 == 1L)

  z = 1:10
  (rev(z) %==% 10:1)

  !!TRUE

})
