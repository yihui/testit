% Please edit documentation in R/testit.R
\name{assert}
\alias{assert}
\title{Assertions with a message}
\usage{
assert(fact, ...)
}
\arguments{
\item{fact}{a message for the assertions when any of them fails; treated the
same way as expressions in \code{...} if it is not a character string,
which means you do not have to provide a message to this function}

\item{...}{any number of R expressions, presumably to return vectors of
\code{TRUE}'s (if \code{FALSE} is returned anywhere, an error will show up)}
}
\value{
Invisible \code{NULL} if all expressions returned \code{TRUE},
  otherwise an error is signalled and the user-provided message is emitted.
}
\description{
This function was built from \code{\link{stopifnot}()}. It emits a message in
case of errors, which can be a helpful hint for diagnosing the errors
(\code{stopifnot()} only prints the possibly truncated source code of the
expressions).
}
\note{
The internal implementation of \code{stopifnot()} is different with the
  function in R \pkg{base}: (1) the custom message \code{fact} is emitted if
  an error occurs (2) \code{assert()} requires the logical values to be
  non-empty (3) if \code{...} contains a compound expression in \code{{}}
  which returns \code{FALSE} (e.g., \code{if (TRUE) {1+1; FALSE}}), the first
  and the last but one line of the source code from \code{\link{deparse}()}
  are printed in the error message, otherwise the first line is printed
}
\examples{
assert("one equals one", 1 == 1)
assert("seq and : produce equal sequences", seq(1L, 10L) == 1L:10L)
assert("seq and : produce identical sequences", identical(seq(1L, 10L), 1L:10L))

# multile tests
T = FALSE
F = TRUE
assert("T is bad for TRUE, and so is F for FALSE", T != TRUE, F != FALSE)

# a mixture of tests
assert("Let's pray all of them will pass", 1 == 1, 1 != 2, letters[4] == "d", 
    rev(rev(letters)) == letters)

# logical(0) cannot pass assert(), although stopifnot() does not care
try(assert("logical(0) cannot pass", 1 == integer(0)))
stopifnot(1 == integer(0))  # it's OK!

# a compound expression
try(assert("this if statement returns TRUE", if (TRUE) {
    x = 1
    x == 2
}))

# no message
assert(!FALSE, TRUE, is.na(NA))
}
