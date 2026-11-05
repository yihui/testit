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
  (str_lines(x) %==% c('int 1', '- attr(*, "foo")= chr "bar"'))
  (str_lines(list(f = y ~ x))[2] %==% ' $ f: Class \'formula\'  language y ~ x')
})
