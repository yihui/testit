library(testit)
test_pkg('testit')

op = options(testit.hide.error = TRUE)
tryCatch(
  test_pkg('testit', 'test-error'),
  error = function(e) invisible(NULL), finally = function() options(op)
)
