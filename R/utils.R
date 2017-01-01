
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

# decompose functions from jmvcore

decomposeTerm <- function(term) {
    
    chars <- strsplit(term, '')[[1]]
    components <- character()
    componentChars <- character()
    inQuote <- FALSE
    
    i <- 1
    n <- length(chars)
    
    while (i <= n) {
        char <- chars[i]
        if (char == '`') {
            inQuote <- ! inQuote
        }
        else if (char == '\\') {
            i <- i + 1
            char <- chars[i]
            componentChars <- c(componentChars, char)
        }
        else if (char == ':' && inQuote == FALSE) {
            component <- paste0(componentChars, collapse='')
            components <- c(components, component)
            componentChars <- character()
        }
        else {
            componentChars <- c(componentChars, char)
        }
        i <- i + 1
    }

    component <- paste0(componentChars, collapse='')
    components <- c(components, component)
    
    components
}

