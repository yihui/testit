# Repository Instructions

## Build and Test

``` bash
R CMD build .
R CMD INSTALL *_*.tar.gz
Rscript tests/test-all.R
```

Tests in `tests/testit/test-*.R` (one per `R/foo.R`); also `tests/test-error/`
and `tests/test-r35/`. Runner: `tests/test-all.R`.

Test form:

``` r
assert('message', {
  (actual %==% expected)
})
```

- Use `has_error()` for error testing, not `tryCatch()`
- Internal functions are auto-exposed by testit; call directly (no `:::`)

## R Code Style

- Assignment `=`, strings single-quoted, indent 2 spaces, wrap at 80 chars
- No `{}` for single-expression `if`
- No `else NULL` (implicit); no `return(NULL)` (use bare `return()`)
- Roxygen: no `@title`/`@description`/`@details`; write description directly
- Examples: avoid `\dontrun{}`
- US spelling ("color" not "colour")

## PR Checklist

1. Every change needs tests
2. Re-roxygenize: `roxygen2::roxygenize('.')` after roxygen changes
3. Merge latest `master` before pushing; run tests
4. Bump patch version in `DESCRIPTION` once per PR
5. Update `NEWS.md`: first heading form `# PKG x.y` (next CRAN release)
6. Clean up before committing: `rm -rf *.tar.gz *.Rcheck/ tests/R-lib-*`; add
   only files you changed (man files from roxygen are fine)
7. Never create a branch/PR without confirming first
