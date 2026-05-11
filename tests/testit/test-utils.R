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

assert('deparse_one() collapses multi-line deparse to a single string', {
  (deparse_one(1:3) %==% '1:3')
  (nchar(deparse_one(seq_len(100))) > 0)
  (!grepl('\n', deparse_one(seq_len(100))))
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
  suppressMessages(test_snap(f1, env, update = TRUE))
  l1 = readLines(f1, warn = FALSE)
  (grepl('^```r$', l1[1]))
  (!any(grepl('^```\\{r\\}$', l1)))

  f2 = tempfile(fileext = '.md')
  writeLines(c('```{r}', '1 + 1', '```'), f2)
  suppressMessages(test_snap(f2, env, update = TRUE))
  l2 = readLines(f2, warn = FALSE)
  (grepl('^```\\{r\\}$', l2[1]))
})

assert('test_snap() inserts missing output block before a later code block with output', {
  env = new.env(parent = baseenv())

  # first block has no output; second block has output
  f = tempfile(fileext = '.md')
  writeLines(c(
    '```r', '1 + 1', '```',
    '```r', '2 + 2', '```',
    '```', '[1] 4', '```'
  ), f)
  suppressMessages(test_snap(f, env, update = TRUE))
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

assert('test_snap() fails when snapshot output does not match (update = FALSE)', {
  env = new.env(parent = baseenv())
  f = tempfile(fileext = '.md')
  writeLines(c(
    '```r', '1 + 1', '```',
    '```', '[1] 999', '```'
  ), f)
  (length(test_snap(f, env, update = FALSE)) > 0)
})

assert('test_snap() passes when snapshot output matches (update = FALSE)', {
  env = new.env(parent = baseenv())
  f = tempfile(fileext = '.md')
  writeLines(c(
    '```r', '1 + 1', '```',
    '```', '[1] 2', '```'
  ), f)
  (test_snap(f, env, update = FALSE) %==% NULL)
})

assert('test_snap() rebuilds fence correctly including text blocks', {
  env = new.env(parent = baseenv())
  # Include text before the code block to cover the 'text' type branch
  f = tempfile(fileext = '.md')
  writeLines(c('# Title', '', '```r', '1 + 1', '```'), f)
  suppressMessages(test_snap(f, env, update = TRUE))
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

assert('test_pkg() collects all errors across and within files', {
  d = tempfile(); dir.create(d)
  writeLines(c('stop("error one")', 'stop("error one and a half")'), file.path(d, 'test-aaa.R'))
  writeLines('stop("error two")', file.path(d, 'test-bbb.R'))
  out = NULL
  msg = tryCatch(withCallingHandlers(
    test_pkg('testit', dir = d),
    message = function(m) { out <<- c(out, conditionMessage(m)); invokeRestart('muffleMessage') }
  ), error = conditionMessage)
  (grepl('error one', msg))
  (grepl('error one and a half', msg))
  (grepl('error two', msg))
  (!exists('.traceback', baseenv(), inherits = FALSE) ||
    grepl('^error one at .+\nerror one and a half at .+\nerror two at ', msg))
})

assert('test_pkg() prints details via message() when errors exceed warning.length', {
  d = tempfile(); dir.create(d)
  op = options(warning.length = 100L)
  writeLines(c(
    'stop("error one with extra padding to ensure length exceeds the limit")',
    'stop("error one and a half with additional padding for length")'
  ), file.path(d, 'test-aaa.R'))
  writeLines(
    'stop("error two with more padding to guarantee overflow")', file.path(d, 'test-bbb.R')
  )
  out = NULL
  msg = tryCatch(withCallingHandlers(
    test_pkg('testit', dir = d),
    message = function(m) { out <<- c(out, conditionMessage(m)); invokeRestart('muffleMessage') }
  ), error = conditionMessage)
  options(op)
  (msg %==% '3 tests failed (see details above)')
  printed = paste(out, collapse = '')
  (grepl('error one', printed))
  (grepl('error two', printed))
  (grepl('error one and a half', printed))
})

assert('test_pkg() filter selects a subset of test files', {
  d = tempfile(); dir.create(d)
  writeLines(
    'library(testit)\nassert("a passes", (TRUE))',
    file.path(d, 'test-aaa.R')
  )
  writeLines('stop("should not run")', file.path(d, 'test-bbb.R'))
  # filter matches only "aaa", so "bbb" should not run
  (test_pkg('testit', dir = d, filter = 'aaa') %==% NULL)
  # filter matches "bbb", which errors
  (has_error(test_pkg('testit', dir = d, filter = 'bbb')))
})

if (Sys.which('git') != '') assert('test_snap() with update = NA on git-tracked file writes and diffs', {
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
  system2('git', c('commit', '-q', '-m', shQuote('wrong output')))
  env = new.env(parent = baseenv())
  # update = NA on a tracked file should rewrite and return an error message
  (length(test_snap(f, env, update = NA)) > 0)
  # check that the file was updated with correct output
  lines = readLines(f, warn = FALSE)
  (any(grepl('^\\[1\\] 2$', lines)))
  setwd(owd)
})
