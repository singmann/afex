
escape_vars <- function(names) {
  if (length(names) == 0)
    return(names)
  names <- sapply(names, function(name) {
    if (make.names(name) != name) {
      name <- gsub('\\', '\\\\', name, fixed=TRUE)
      name <- gsub('`',  '\\`',  name, fixed=TRUE)
      name <- paste0('`', name, '`')
    }
    name
  }, USE.NAMES=FALSE)
  names
}

