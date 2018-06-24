
## paste function that can replace stringr::str_c and differs from the way 
# paste handles NULL arguments as last arguments. 
# It checks whether the first or last char of the string is equal to sep and 
# removes it in this case.
mypaste <- function(..., sep) {
  tmp <- paste(..., sep = sep)
  if (substr(tmp, nchar(tmp), nchar(tmp)) == sep) {
    tmp <- substr(tmp, 1, nchar(tmp)-1)
  }
  if (substr(tmp, 1, 1) == sep) {
    tmp <- substr(tmp, 2, nchar(tmp))
  }
  tmp
}

escape_vars <- function(names) {
  if (length(names) == 0)
    return(names)
  names <- vapply(names, function(name) {
    if (make.names(name) != name) {
      name <- gsub('\\', '\\\\', name, fixed=TRUE)
      name <- gsub('`',  '\\`',  name, fixed=TRUE)
      name <- paste0('`', name, '`')
    }
    name
  }, FUN.VALUE='', USE.NAMES=FALSE)
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


print_legend <- function(x) {
  sig_symbols <- as.character(attr(x, "sig_symbols"))
  if(length(sig_symbols) > 0 & !all(sig_symbols == rep("", 4))) {
    sleg <- attr(stats::symnum(0, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                               symbols = rev(c(" " , trimws(sig_symbols)))), "legend")
    width <- getOption("width")
    
    if(width < nchar(sleg)) {
      sleg <- strwrap(sleg, width = width - 2, prefix = "  ")
    } 
    
    cat("---\nSignif. codes:  ", sleg, sep = "", fill = getOption("width") + 4 + max(nchar(sleg, "bytes") - nchar(sleg)))
  }
}
