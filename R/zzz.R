# clean up the temp library created in test_pkg() at the end of the R session
.onLoad = function(libname, pkgname) {
  reg.finalizer(.env, function(e) {
    # unload DLLs loaded from lib_new, otherwise its libs/ dir can't be removed
    if (!is.null(e$lib_new)) for (d in getLoadedDLLs()) {
      if (substring(d[['path']], 1, nchar(e$lib_new) + 1) == paste0(e$lib_new, '/'))
        dyn.unload(d[['path']])
    }
    unlink(e$lib_new, recursive = TRUE)
    if (!is.null(e$lib_old)) .libPaths(e$lib_old)
  }, onexit = TRUE)
}
