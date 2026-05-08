# clean up the temp library created in test_pkg() at the end of the R session
.onLoad = function(libname, pkgname) {
  reg.finalizer(.env, function(e) {
    if (is.null(e$lib_new)) return()
    # unload DLLs loaded from lib_new, otherwise its libs/ dir can't be removed
    prefix = paste0(e$lib_new, .Platform$file.sep)
    for (d in getLoadedDLLs()) {
      p = norm_path(d[['path']], mustWork = FALSE)
      if (starts_with(p, prefix))
        tryCatch(dyn.unload(d[['path']]), error = identity)
    }
    unlink(e$lib_new, recursive = TRUE, force = TRUE)
    cleanup_msg(e$lib_new)
    if (!is.null(e$lib_old)) .libPaths(e$lib_old)
  }, onexit = TRUE)
}
