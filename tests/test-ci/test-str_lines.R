# these depend on str() output, which R-devel has changed before, so only run on CI

assert('str_lines() removes the common leading spaces of str() output', {
  (str_lines(1) %==% 'num 1')
  (str_lines('a') %==% 'chr "a"')
  # nested output keeps the relative indentation of its sub-lines
  (str_lines(list(a = 1)) %==% c('List of 1', ' $ a: num 1'))
})

assert('str_lines() puts exactly one space after ":" and "=" in components', {
  (str_lines(list(a = 1, b = list(c = 'x'))) %==% c(
    'List of 2', ' $ a: num 1', ' $ b: List of 1',
    '  ..$ c: chr "x"'
  ))
  x = structure(1L, foo = 'bar')
  # old R (e.g. 3.2.0) prints 'atomic [1:1]' for integers with attributes
  (has_match(str_lines(x)[1], '^(int|atomic \\[1:1\\]) 1$'))
  (str_lines(x)[2] %==% '- attr(*, "foo")= chr "bar"')
  # old R prints 'length 3' instead of 'language' for formulas
  (has_match(str_lines(list(f = y ~ x))[2], "^ [$] f: Class 'formula' +(language|length 3) y ~ x$"))
})
