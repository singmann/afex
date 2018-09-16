
#### @export
afex_plot <- function(object, ...) UseMethod("afex_plot", object)

# attr(object$anova_table, "es")
# 

get_plot_var <- function(x) {
  if (missing(x)) return()
  if (inherits(x, "formula")) {
    return(all.vars(x[[2]]))
  } else {
    return(x)
  }
}

get_emmeans <- function(object, x, trace) {

  return(emmeans::emmeans(object, spec = c(get_plot_var(x),
                                           get_plot_var(trace))))
}

afex_interaction_plot <- function(means, 
                                  data, 
                                  plot_error = TRUE,
                                  error_exp = 1, 
                                  error_width = 0,
                                  plot_data = TRUE,
                                  alpha_data = 1,
                                  jitter_x = 0,
                                  jitter_y = 0,
                                  dodge = 0.2) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("package ggplot2 is required.")
  }
  
  
  plot_out <- ggplot2::ggplot(means, 
                              ggplot2::aes(y = y, x = x,
                                           group = trace, shape = trace, 
                                           linetype = trace)) 
  if (plot_data) {
    plot_out <- plot_out + 
      ggplot2::geom_point(data = data, 
                          alpha = alpha_data,
                          color = "grey",
                          position = 
                            ggplot2::position_jitterdodge(
                              jitter.width = jitter_x, 
                              jitter.height = jitter_y, 
                              dodge.width = dodge))
  }
  
  plot_out <- plot_out + 
    ggplot2::geom_point(position = ggplot2::position_dodge(width = dodge)) +
    ggplot2::geom_line(position = ggplot2::position_dodge(width = dodge))
  
  if (plot_error) {
    plot_out <- plot_out +
      ggplot2::geom_errorbar(ggplot2::aes(
        ymin = y - error_exp*error,
        ymax = y + error_exp*error), 
        position = ggplot2::position_dodge(width = dodge), 
        width = error_width)
  }
  
  return(plot_out)
  
}

#' @rdname afex_plot
#' @method afex_plot afex_aov
#' @export
afex_plot.afex_aov <- function(object, 
                               x,
                               trace,
                               error = "model",
                               error_exp = 1,
                               error_width = 0,
                               dodge = 0.2,
                               plot_data = TRUE,
                               alpha_data = 1,
                               jitter_x = 0,
                               jitter_y = 0,
                               args_emmeans = list(),
                               ...) {
  if (!requireNamespace("emmeans", quietly = TRUE)) {
    stop("package emmeans is required.")
  }
  all_vars <- c(get_plot_var(x), get_plot_var(trace))
  
  emms <- as.data.frame(do.call(emmeans::emmeans, 
                                args = c(object = list(object), 
                                         specs = list(all_vars), 
                                         args_emmeans)))
  
  x <- get_plot_var(x)
  emms$x <- interaction(emms[x], sep = "\n")
  colnames(emms)[colnames(emms) == "emmean"] <- "y"
  emms$all_vars <- interaction(emms[all_vars], sep = ".")
  
  data <- object$data$long
  colnames(data)[colnames(data) == attr(object, "dv")] <- "y"
  data <- aggregate(data$y, by = data[c(all_vars,attr(object, "id"))], 
                    FUN = mean, drop = FALSE)
  data$y <- data$x
  data$x <- interaction(data[x], sep = "\n")
  data$all_vars <- interaction(data[all_vars], sep = ".")
  
  plot_error <- TRUE
  if (error[1] == "model") {
    emms$error <- emms$SE
  } else if (error[1] == "SE") {
    tmp <- tapply(data$y, INDEX = list(data$all_vars), 
                  FUN = function(x) sd(x)/sqrt(length(x)))
    stopifnot(emms$all_vars == names(tmp))
    emms$error <- tmp
  } else if (error[1] %in% c("CMO")) {
    indiv_means <- tapply(data$y, INDEX = data[attr(object, "id")], FUN = mean)
    within_vars <- all_vars[all_vars %in% names(attr(object, "within"))]
    within_fac <- interaction(data[within_vars], sep = ".")
    J <- length(levels(within_fac))
    ## Cosineau & O'Brien (2014), Equation 2:
    new_y <- data$y - 
      indiv_means[as.character(data[,attr(object, "id")])] +
      mean(data$y)
    ## Cosineau & O'Brien (2014), Equation 4:
    y_bar <- tapply(new_y, INDEX = within_fac, FUN = mean)
    new_z <- sqrt(J / (J-1)) * (new_y - y_bar[within_fac]) + y_bar[within_fac]
    tmp <- tapply(new_z, INDEX = list(data$all_vars), 
                  FUN = function(x) sd(x)/sqrt(length(x)))
    stopifnot(emms$all_vars == names(tmp))
    emms$error <- tmp
  } else if (is.null(error[1])) {
    plot_error <- FALSE  
  }
  #str(object$data, 1)
  
  
  if (!missing(trace)) {
    trace <- get_plot_var(trace)
    emms$trace <- interaction(emms[trace], sep = "\n")
    data$trace <- interaction(data[trace], sep = "\n")
    plot_out <- afex_interaction_plot(means = emms, 
                                      error_exp = error_exp, 
                                      error_width = error_width, 
                                      plot_error = plot_error,
                                      dodge = dodge, 
                                      data = data,
                                      plot_data = plot_data, 
                                      alpha_data = alpha_data, 
                                      jitter_x = jitter_x, 
                                      jitter_y = jitter_y)
  } else {
    trace <- NULL
  }
  
  # plot_out <- make_basic_plot(object = object,
  #                             x = x,
  #                             trace = trace, 
  #                             error = error,
  #                             error_exp = error_exp,
  #                             error_width = error_width,
  #                             dodge = dodge)
  return(plot_out)
}

# trace <- NULL
# plot_out <- ggplot2::ggplot(emms, 
#                             ggplot2::aes(y = emmean, x = x, 
#                                          shape = x)) +
#   ggplot2::geom_point() +
#   ggplot2::geom_line()