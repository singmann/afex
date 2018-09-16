
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
  if (!requireNamespace("emmeans", quietly = TRUE)) {
    stop("package emmeans is required.")
  }
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
                               ...) {
  
  emms <- as.data.frame(get_emmeans(object = object,
                                    x = x,
                                    trace = trace))
  x <- get_plot_var(x)
  emms$x <- interaction(emms[x], sep = "\n")
  colnames(emms)[colnames(emms) == "emmean"] <- "y"
  
  data <- object$data$long
  data$x <- interaction(data[x], sep = "\n")
  colnames(data)[colnames(data) == attr(object, "dv")] <- "y"
  
  #str(object, 1)
  
  plot_error <- TRUE
  if (error[1] == "model") {
    emms$error <- emms$SE
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