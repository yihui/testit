library(testit)

# no need to use testit:::available_dir()
assert(
  'available_dir() should find an existing directory',
  file.exists(
    available_dir(c('foobar', 'whatever', '~', system.file('man', package = 'testit')))
  )
)

exprs = parse(text = 'if (TRUE) {T&F}\n1+1')
assert(
  'deparse_key() fetches the n-1 element if code is in {}',
  deparse_key(exprs[[1]]) == 'if (TRUE) { .... T & F'
)
assert(
  'deparse_key() returns the parsed code if length == 1',
  deparse_key(exprs[[2]]) == '1 + 1'
)
