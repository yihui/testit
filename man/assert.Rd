\name{assert}
\alias{assert}
\title{Assertions with a message}
\usage{
assert(msg, ...)
}
\arguments{
  \item{msg}{a message for the assertions when any of them
  fails; ignored if it is not a character string}

  \item{...}{any number of logical R expressions to be
  passed to \code{\link{stopifnot}} to check if their
  values are all \code{TRUE}}
}
\value{
  Invisible \code{NULL} if all expressions returned
  \code{TRUE}, otherwise an error is signalled and the
  user-provided message is emitted.
}
\description{
  This function is simply a wrapper to
  \code{\link{stopifnot}}, with a message emitted in case
  of errors, which can be a helpful hint for diagnosing the
  errors (by default, \code{stopifnot()} only prints the
  possibly truncated source code of the expressions).
}
\examples{
assert("one equals one", 1 == 1)
assert("seq and : produce equal sequences", seq(1L, 10L) == 1L:10L)
assert("seq and : produce identical sequences", identical(seq(1L, 10L), 1L:10L))
# multile tests
T = FALSE
F = TRUE
assert("T is bad for TRUE, and so is F", T != TRUE, F != FALSE)
# a mixture of tests
assert("Let's pray all of them will pass", 1 == 1, 1 != 2, letters[4] == "d", 
    rev(rev(letters)) == letters)
}
