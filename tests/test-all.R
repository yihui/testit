library(testit)
test_pkg('testit')

tryCatch(
  test_pkg('testit', 'test-error'),
  error = function(e) invisible(NULL)
)
