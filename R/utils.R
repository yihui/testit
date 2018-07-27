# find an available dir
available_dir = function(dirs) {
  for (i in dirs) {
    if (utils::file_test('-d', i)) return(i)
  }
  stop('none of the directories exists:\n', paste(utils::formatUL(dirs), collapse = '\n'))
}

# tailored for assert(): extract the expression that is likely to be useful for
# diagnostics if possible
deparse_key = function(expr) {
  x = deparse(expr, width.cutoff = 100L)
  if ((n <- length(x)) <= 1) return(x)
  # if expression is in {}, fetch the line n-1, otherwise use the first line
  paste(x[1], '....', if (x[n] == '}') sub('^\\s*', '', x[n - 1L]))
}

# whether every element of x is strictly TRUE
all_true = function(x) {
  is.logical(x) && length(x) && !any(is.na(x)) && all(x)
}

insert_identical = function() {
  rstudioapi::insertText(text = ' %==% ')
}

# This function is a modification of base::sys.source.  It allows to specify
# the top-level environment, which is by default "envir" (the same as in
# base::sys.source), but for package testing it is desirable to use the
# package namespace to mimick the environment structure used when packages
# are running. This function assumes that chdir = FALSE and keep.source = TRUE.
sys.source2 = function(file, envir, top.env = as.environment(envir)) {
  oop = options(keep.source = TRUE, topLevelEnvironment = top.env)
  on.exit(options(oop))

  lines = readLines(file, warn = FALSE, encoding = 'UTF-8')
  srcfile = srcfilecopy(file, lines, file.mtime(file), isFile = TRUE)
  exprs = parse(text = lines, srcfile = srcfile, encoding = 'UTF-8')

  if (length(exprs) == 0L) return()
  owd = setwd(dirname(file)); on.exit(setwd(owd), add = TRUE)
  for (i in seq_along(exprs)) eval(exprs[i], envir)
}
