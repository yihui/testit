assert('assert works', 1==1)

assert(
  'Okay, that is kind of cheating; test assert() itself now',
  has_error(assert('this should produce an error', 1==2))
)

# a meaningless test in terms of R (failure is irrelevant to Frequentist or Bayesian)
try(assert(
  'Are Frequentists correct? (http://xkcd.com/1132/)',
  'The sun has exploded!', sample(6, 2) == c(6, 6)
), silent = !interactive())
