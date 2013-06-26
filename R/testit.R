#' Assertions with a message
#'
#' This function is simply a wrapper to \code{\link{stopifnot}}, with a message
#' emitted in case of errors, which can be a helpful hint for diagnosing the
#' errors (by default, \code{stopifnot()} only prints the possibly truncated
#' source code of the expressions).
#' @param fact a message for the assertions when any of them fails; ignored if
#'   it is not a character string
#' @param ... any number of logical R expressions to be passed to
#'   \code{\link{stopifnot}} to check if their values are all \code{TRUE}
#' @return Invisible \code{NULL} if all expressions returned \code{TRUE},
#'   otherwise an error is signalled and the user-provided message is emitted.
#' @export
#' @examples assert('one equals one', 1==1)
#' assert('seq and : produce equal sequences', seq(1L, 10L) == 1L:10L)
#' assert('seq and : produce identical sequences', identical(seq(1L, 10L), 1L:10L))
#'
#' # multile tests
#' T=FALSE; F=TRUE
#' assert('T is bad for TRUE, and so is F for FALSE', T!=TRUE, F!=FALSE)
#'
#' # a mixture of tests
#' assert("Let's pray all of them will pass", 1==1, 1!=2, letters[4]=='d', rev(rev(letters))==letters)
assert = function(fact, ...) {
  # pass ... to stopifnot (need to protect them from being evaluated for now)
  mc = match.call(expand.dots = FALSE)
  # print a message before quit
  withCallingHandlers(
    do.call('stopifnot', as.list(mc[['...']]), envir = parent.frame()),
    error = function(e) if (is.character(fact)) message(fact)
  )
}


#' Run the tests of a package in its namespace
#'
#' The main purpose of this function is to expose the namespace of a package
#' when running tests, which allows one to use non-exported objects in the
#' package without having to resort to the triple colon \code{\link{:::}} trick.
#'
#' The tests are assumed to be under the \file{testit/} directory by default,
#' and this function also looks for the \file{tests/testit/} directory under the
#' package installation directory when the user-provided \code{dir} does not
#' exist.
#'
#' For \command{R CMD check}, this means the test R scripts (\file{*.R} or
#' \file{*.r}) are under \file{pkg_root/tests/testit/}. The R scripts are
#' executed with \code{\link{sys.source}} in the namespace of the package to be
#' tested; when an R script is executed, the working directory is the same as
#' the directory containing this script.
#' @param package the package name
#' @param dir the directory of the test files; by default, it is the directory
#'   \file{testit/} under the current working directory
#' @return \code{NULL}. All test files are executed, unless an error occurs.
#' @seealso The \pkg{testthat} package (much more sophisticated).
#' @export
#' @examples \dontrun{test_pkg('testit')}
test_pkg = function(package, dir = 'testit/') {
  library(package, character.only = TRUE)
  path = available_dir(c(dir, system.file('tests', 'testit/', package = package)))
  rs = list.files(path, '[.][rR]$', full.names = TRUE)
  # make all objects in the package visible to tests
  env = new.env(parent = getNamespace(package))
  for (r in rs) sys.source(r, envir = env, chdir = TRUE, keep.source = TRUE)
}

#' Check if an R expression produces warnings or errors
#'
#' The two function \code{has_warning()} and \code{has_error()} check if an
#' expression produces warnings and errors, respectively.
#' @param expr an R expression
#' @return A logical value.
#' @export
#' @rdname has_message
#' @examples has_warning(1+1); has_warning(1:2+1:3)
#'
#' has_error(2-3); has_error(1+'a')
has_warning = function(expr) {
  warn = FALSE
  withCallingHandlers(expr, warning = function(w) warn <<- TRUE)
  warn
}
#' @export
#' @rdname has_message
has_error = function(expr) {
  inherits(try(force(expr)), 'try-error')
}
