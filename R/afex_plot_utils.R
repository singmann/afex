
se <- function(x, na.rm = FALSE) sd(x, na.rm = na.rm)/sqrt(length(x))

get_emms <- function(object, 
                     x,
                     trace,
                     panel,
                     emmeans_arg, 
                     new_levels, 
                     level) {
  if (!requireNamespace("emmeans", quietly = TRUE)) {
    stop("package emmeans is required.", call. = FALSE)
  }
  
  all_vars <- c(x, trace, panel)
  emmeans_arg$options$level <- level
  emms <- as.data.frame(do.call(emmeans::emmeans, 
                                args = c(object = list(object), 
                                         specs = list(all_vars), 
                                         emmeans_arg)))
  for (i in seq_along(new_levels)) {
    levels(emms[[names(new_levels)[i]]]) <- new_levels[[i]]
  }
  emms$x <- interaction(emms[x], sep = "\n")
  colnames(emms)[colnames(emms) == "emmean"] <- "y"
  attr(emms, "dv") <- attr(object, "dv")
  attr(emms, "x") <- paste(x, sep = "\n")
  if (length(panel) > 0) {
    emms$panel <- interaction(emms[panel], sep = "\n")
  } else {
    emms$panel <- "1"
  }
  emms$all_vars <- interaction(emms[all_vars], sep = ".")
  
  return(emms)
}

get_plot_var <- function(x) {
  if (missing(x)) return()
  if (inherits(x, "formula")) {
    return(all.vars(x[[2]]))
  } else {
    return(x)
  }
}

