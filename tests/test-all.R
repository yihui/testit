library(testit)
test_pkg('testit')
test_pkg('testit', 'test-error')
if (getRversion() >= '3.5.0') test_pkg('testit', 'test-r35')
