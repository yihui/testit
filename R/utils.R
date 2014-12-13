# find an available dir
available_dir = function(dirs) {
  for (i in dirs) {
    if (file_test('-d', i)) return(i)
  }
  stop('none of the directories exists:\n', paste(formatUL(dirs), collapse = '\n'))
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
