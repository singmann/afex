
escape_vars <- function(names) {
  if (length(names) == 0)
    return(names)
  paste0('`', gsub('\`', '\\\\`', names), '`')
}

