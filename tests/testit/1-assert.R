library(testit)

assert('assert works', 1==1)

# Okay, that is kind of cheating
assert(
  'assert() should signal an error if a condition does not hold',
  has_error(assert('this should produce an error', 1==2))
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
