library(testit)
test_pkg('testit')
test_pkg('testit', 'test-error')
if (getRversion() >= '3.5.0') test_pkg('testit', 'test-r35')
# str() output may change in R-devel, so only test it on CI
if (tolower(Sys.getenv('CI')) == 'true') test_pkg('testit', 'test-ci')
