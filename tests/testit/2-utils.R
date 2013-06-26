# no need to use testit:::available_dir()
assert(
  'available_dir() should find an existing directory',
  file.exists(
    available_dir(c('foobar', 'whatever', '~', system.file('man', package = 'testit')))
  )
)
