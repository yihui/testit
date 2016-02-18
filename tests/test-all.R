library(testit)
test_pkg('testit')
try(test_pkg('testit', 'test-error'))
