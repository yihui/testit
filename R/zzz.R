# clean up the temp library created in test_pkg() at the end of the R session
.onLoad = function(libname, pkgname) {
  reg.finalizer(.env, function(e) {
    unlink(e$lib_new, recursive = TRUE)
    if (!is.null(e$lib_old)) .libPaths(e$lib_old)
  }, onexit = TRUE)
}
