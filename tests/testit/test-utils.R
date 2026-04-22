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
  suppressMessages(test_snaps(f1, env, update = TRUE))
  l1 = readLines(f1, warn = FALSE)
  (grepl('^```r$', l1[1]))
  (!any(grepl('^```\\{r\\}$', l1)))

  f2 = tempfile(fileext = '.md')
  writeLines(c('```{r}', '1 + 1', '```'), f2)
  suppressMessages(test_snaps(f2, env, update = TRUE))
  l2 = readLines(f2, warn = FALSE)
  (grepl('^```\\{r\\}$', l2[1]))
})

assert('test_snaps() inserts missing output block before a later code block with output', {
  env = new.env(parent = baseenv())

  # first block has no output; second block has output
  f = tempfile(fileext = '.md')
  writeLines(c(
    '```r', '1 + 1', '```',
    '```r', '2 + 2', '```',
    '```', '[1] 4', '```'
  ), f)
  suppressMessages(test_snaps(f, env, update = TRUE))
  lines = readLines(f, warn = FALSE)
  # output for first block must have been inserted
  (any(grepl('^\\[1\\] 2$', lines)))
  # output for second block must still be present
  (any(grepl('^\\[1\\] 4$', lines)))
})
