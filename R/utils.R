# find an available dir
available_dir = function(dirs) {
  for (i in dirs) {
    if (file_test('-d', i)) return(i)
  }
  stop('none of the directories exists:\n', paste(formatUL(dirs), collapse = '\n'))
}
