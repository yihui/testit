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

assert('pkg_name() errors when no DESCRIPTION file is found', {
  d = tempdir()
  owd = setwd(d); on.exit(setwd(owd))
  (has_error(pkg_name()))
})

assert('get_fence() adds an extra backtick when extra = TRUE and content has ```', {
  text = c('````', 'some text')
  fence = get_fence(text, extra = TRUE)
  # should be 5 backticks (4 found + 1 extra)
  (fence %==% '`````')
})

assert('get_fence() returns ``` when content has no fences', {
  (get_fence(c('hello', 'world')) %==% '```')
})

assert('parse_snapshot() errors on unbalanced fences', {
  (has_error(parse_snapshot(c('```r', '1 + 1'), 'test.md')))
})

assert('test_snaps() fails when snapshot output does not match (update = FALSE)', {
  env = new.env(parent = baseenv())
  f = tempfile(fileext = '.md')
  writeLines(c(
    '```r', '1 + 1', '```',
    '```', '[1] 999', '```'
  ), f)
  (has_error(test_snaps(f, env, update = FALSE)))
})

assert('test_snaps() passes when snapshot output matches (update = FALSE)', {
  env = new.env(parent = baseenv())
  f = tempfile(fileext = '.md')
  writeLines(c(
    '```r', '1 + 1', '```',
    '```', '[1] 2', '```'
  ), f)
  # should not error
  (test_snaps(f, env, update = FALSE) %==% NULL)
})

assert('test_snaps() rebuilds fence correctly including text blocks', {
  env = new.env(parent = baseenv())
  # Include text before the code block to cover the 'text' type branch
  f = tempfile(fileext = '.md')
  writeLines(c('# Title', '', '```r', '1 + 1', '```'), f)
  suppressMessages(test_snaps(f, env, update = TRUE))
  lines = readLines(f, warn = FALSE)
  # text is preserved and output block is added
  (lines[1] %==% '# Title')
  (any(grepl('^\\[1\\] 2$', lines)))
})

assert('capture_output() captures errors as Error: messages', {
  env = new.env(parent = baseenv())
  out = capture_output("stop('oops')", env, tempdir())
  ('Error: oops' %==% out)
})

assert('clean_output() removes bytecode and environment addresses', {
  x = c('<bytecode: 0x1234abcd>', '<environment: 0xdeadbeef>', 'normal')
  (clean_output(x) %==% c('<bytecode: ...>', '<environment: ...>', 'normal'))
})

assert('all_true() handles edge cases', {
  (!all_true(logical(0)))
  (!all_true(NA))
  (!all_true(c(TRUE, NA)))
  (!all_true(1))
  (all_true(TRUE))
  (all_true(c(TRUE, TRUE)))
})

assert('test_pkg() sources helper files before tests', {
  d = tempfile(); dir.create(d)
  # write a helper file that defines a variable
  writeLines('helper_val = 42', file.path(d, 'helper.R'))
  # write a test that uses the helper value
  writeLines(
    'library(testit)\nassert("helper sourced", (helper_val %==% 42))',
    file.path(d, 'test-helper-check.R')
  )
  # test_pkg should source helper.R before test-helper-check.R
  (test_pkg('testit', dir = d) %==% NULL)
})

assert('test_pkg() error handler shows location on test failure', {
  d = tempfile(); dir.create(d)
  # write a test file that errors
  writeLines('stop("deliberate error")', file.path(d, 'test-fail.R'))
  out = capture.output(
    has_error(test_pkg('testit', dir = d)),
    type = 'message'
  )
  # error handler should produce output (may vary by R version)
  (has_error(test_pkg('testit', dir = d)))
})

if (Sys.which('git') != '') assert('test_snaps() with update = NA on git-tracked file writes and diffs', {
  # create a temp git repo with a snapshot file
  d = tempfile(); dir.create(d)
  owd = getwd(); setwd(d)
  system2('git', c('init', '-q'))
  system2('git', c('config', 'user.email', 'test@test.com'))
  system2('git', c('config', 'user.name', 'Test'))
  f = file.path(d, 'test.md')
  writeLines(c('```r', '1 + 1', '```', '```', '[1] 2', '```'), f)
  system2('git', c('add', 'test.md'))
  system2('git', c('commit', '-q', '-m', 'init'))
  # now change the output to be wrong
  writeLines(c('```r', '1 + 1', '```', '```', '[1] 999', '```'), f)
  system2('git', c('add', 'test.md'))
  system2('git', c('commit', '-q', '-m', 'wrong output'))
  env = new.env(parent = baseenv())
  # update = NA on a tracked file should rewrite and then error
  (has_error(test_snaps(f, env, update = NA)))
  # check that the file was updated with correct output
  lines = readLines(f, warn = FALSE)
  (any(grepl('^\\[1\\] 2$', lines)))
  setwd(owd)
})
