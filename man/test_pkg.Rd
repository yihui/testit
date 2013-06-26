\name{test_pkg}
\alias{testit}
\alias{test_pkg}
\title{Run the tests of a package in its namespace}
\usage{
test_pkg(package, path = "testit/")
}
\arguments{
  \item{package}{the package name}

  \item{path}{the path to the test files; by default, it is
  the directory \file{testit/} under the current working
  directory}
}
\value{
  \code{NULL}. All test files are executed, unless an error
  occurs.
}
\description{
  The main purpose of this function is to expose the
  namespace of a package when running tests, which allows
  one to use non-exported objects in the package without
  having to resort to the triple colon \code{\link{:::}}
  trick.
}
\details{
  The tests are assumed to be under the \file{testit/}
  directory by default, and this function also looks for
  the \file{tests/testit/} directory under the package
  installation directory when the user-provided \code{path}
  does not exist.

  For \command{R CMD check}, this means the test R scripts
  (\file{*.R} or \file{*.r}) are under
  \file{pkg_root/tests/testit/}. The R scripts are executed
  with \code{\link{sys.source}} in the namespace of the
  package to be tested; when an R script is executed, the
  working directory is the same as the directory containing
  this script.
}
\examples{
\dontrun{
test_pkg("testit")
}
}
\seealso{
  The \pkg{testthat} package (much more sophisticated).
}
