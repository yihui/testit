# find an available dir
available_dir = function(dirs) {
  for (i in dirs) {
    if (file.exists(i)) return(i)
  }
  stop('none of the directories exists:\n', paste(formatUL(dirs), collapse = '\n'))
}
