library(testit)

# no need to use testit:::available_dir()
assert('available_dir() should find an existing directory', {
  (file.exists(
    available_dir(c('foobar', 'whatever', '~', system.file('man', package = 'testit')))
  ))
  (has_error(available_dir('asdfasdf')))
})

exprs = parse(text = 'if (TRUE) {T&F}\n1+1')
assert('deparse_key() fetches the n-1 element if code is in {}', {
  (deparse_key(exprs[[1]]) %==% 'if (TRUE) { .... T & F')
})
assert('deparse_key() returns the parsed code if length == 1', {
  (deparse_key(exprs[[2]]) %==% '1 + 1')
})

assert('insert_identical() should not work in a non-interactive R session', {
  (interactive() || has_error(insert_identical()))
})

assert('sys.source2() works on empty files', {
  f = tempfile()
  writeLines('  ', f)
  (sys.source2(f, environment()) %==% NULL)
})

assert('parse_snapshot() accepts both ```r and ```{r} blocks', {
  blocks = parse_snapshot(c(
    '```r', '1 + 1', '```',
    '',
    '```', '[1] 2', '```',
    '',
    '```{r}', '2 + 2', '```'
  ), 'x.md')
  types = vapply(blocks, `[[`, '', 'type')
  (all(c('r', '{r}', '') %in% types))
})

assert('snapshot updates preserve the original R fence style', {
  env = new.env(parent = baseenv())

  f1 = tempfile(fileext = '.md')
  writeLines(c('```r', '1 + 1', '```'), f1)
  test_snaps(f1, env, update = TRUE)
  l1 = readLines(f1, warn = FALSE)
  (grepl('^```r$', l1[1]))
  (!any(grepl('^```\\{r\\}$', l1)))

  f2 = tempfile(fileext = '.md')
  writeLines(c('```{r}', '1 + 1', '```'), f2)
  test_snaps(f2, env, update = TRUE)
  l2 = readLines(f2, warn = FALSE)
  (grepl('^```\\{r\\}$', l2[1]))
})

assert('mini_diff() reports insertions, deletions, replacements, and gaps', {
  o1 = capture.output(mini_diff(c('a', 'b', 'c'), c('a', 'c')))
  (any(grepl('^- b$', o1)))

  o2 = capture.output(mini_diff(c('a', 'c'), c('a', 'b', 'c')))
  (any(grepl('^\\+ b$', o2)))

  o3 = capture.output(mini_diff(c('a'), c('b')))
  (all(c('- a', '+ b') %in% o3))

  x1 = paste0('L', 1:12)
  x2 = x1
  x2[c(3, 10)] = c('X', 'Y')
  o4 = capture.output(mini_diff(x1, x2))
  (any(grepl('^  \\.\\.\\.$', o4)))
})
