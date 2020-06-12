###if(getRversion() >= "2.15.1")  utils::globalVariables(c("error", "y", "x"))
#' @rdname afex_plot
#' @export
interaction_plot <- function(means, 
                             data, 
                             mapping = c("shape", "lineytpe"), 
                             error_plot = TRUE,
                             error_arg = list(width = 0),
                             data_plot = TRUE,
                             data_geom = ggplot2::geom_point,
                             data_alpha = 0.5,
                             data_arg = list(color = "darkgrey"),
                             point_arg = list(),
                             line_arg = list(),
                             dodge = 0.5, 
                             legend_title,
                             col_x = "x",
                             col_y = "y",
                             col_trace = "trace",
                             col_panel = "panel",
                             col_lower = "lower",
                             col_upper = "upper") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("package ggplot2 is required.", call. = FALSE)
  }
  if (missing(mapping)) {
    mapping <- c('shape', 'linetype')
  } else if (length(mapping) == 0) {
    stop("mapping cannot be empty. Possible values: 'shape', 'color', 'linetype'.", 
         call. = FALSE)
  }
  tmp_list <- as.list(rep(col_trace, length(mapping)))
  names(tmp_list) <- mapping
  
  error_mapping <- mapping[!(mapping %in% c("linetype", "shape", "fill"))]
  tmp_list_error <- as.list(rep(col_trace, length(error_mapping)))
  names(tmp_list_error) <- error_mapping
  
  
  plot_out <- ggplot2::ggplot(data = means, 
                              mapping = do.call(
                                what = ggplot2::aes_string, 
                                args = c(list(
                                  y = col_y, 
                                  x = col_x, 
                                  group = col_trace),
                                  tmp_list)))
  
  if (data_plot) {
    if (missing(data_geom)) {
      data_geom <- ggplot2::geom_point
    }
    data_arg$alpha <- data_alpha
    if (!("position" %in% names(data_arg)) & 
        ("position" %in% names(formals(data_geom)))) {
      data_arg$position = ggplot2::position_dodge(width = dodge)
    }
    plot_out <- plot_out +
      do.call(what = data_geom,
              args = c(
                #mapping = list(ggplot2::aes(group = interaction(x, trace))),
                mapping = 
                  list(
                    ggplot2::aes_string(
                      group = 
                        paste0("interaction(", 
                               paste0(c(col_x, col_trace), collapse =  ", "), 
                               ")")
                    )),
                data = list(data),
                data_arg
              )
      )
  }
  for (i in levels(data$trace)) {
    tmp_means <- means
    tmp_means[means$trace != i, c(col_y, col_lower, col_upper)] <- NA
    #tmp_means <- tmp_means[means$trace == i,]
    plot_out <- plot_out + 
      do.call(what = ggplot2::geom_point, 
              args = c(
                data = list(tmp_means),
                position = list(
                  ggplot2::position_dodge(width = dodge)
                ),
                point_arg, 
                na.rm = list(TRUE)
              )) +
      do.call(what = ggplot2::geom_line, 
              args = c(
                data = list(tmp_means),
                position = list(
                  ggplot2::position_dodge(width = dodge)
                ),
                line_arg, 
                na.rm = list(TRUE)
              ))
    
    if (error_plot) {
      plot_out <- plot_out + 
        do.call(what = ggplot2::geom_errorbar, 
                args = c(
                  data = list(tmp_means),
                  mapping = list(do.call(
                    what = ggplot2::aes_string, 
                    args = c(list(
                      x = col_x, 
                      ymin = col_lower,
                      ymax = col_upper,
                      group = col_trace),
                      tmp_list_error))),
                  position = list(ggplot2::position_dodge(width = dodge)),
                  error_arg, 
                  na.rm = list(TRUE), 
                  inherit.aes = list(FALSE)
                ))
    }
    
  }
  
  if (length(unique(means$panel)) > 1) {
    plot_out <- plot_out + 
      ggplot2::facet_wrap(facets = "panel")
  }
  
  ## add labels
  if (!is.null(attr(means, "dv"))) {
    plot_out <- plot_out + 
      ggplot2::ylab(attr(means, "dv"))
  }
  if (!is.null(attr(means, "x"))) {
    plot_out <- plot_out + 
      ggplot2::xlab(attr(means, "x"))
  }
  if (!missing(legend_title)) {
    legend_title <- paste(legend_title, collapse = "\n")
    tmp_list <- rep(list(ggplot2::guide_legend(title = legend_title)), 
                    length(mapping))
    names(tmp_list) <- mapping
    plot_out <- plot_out + 
      do.call(what = ggplot2::guides, 
              args = tmp_list)
  }
  
  return(plot_out)
  
}


#' @rdname afex_plot
#' @export
oneway_plot <- function(means, 
                        data, 
                        mapping = "",
                        error_plot = TRUE,
                        error_arg = list(width = 0),
                        data_plot = TRUE,
                        data_geom = ggbeeswarm::geom_beeswarm,
                        data_alpha = 0.5,
                        data_arg = list(color = "darkgrey"),
                        point_arg = list(),
                        legend_title,
                        col_x = "x",
                        col_y = "y",
                        col_panel = "panel",
                        col_lower = "lower",
                        col_upper = "upper") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("package ggplot2 is required.", call. = FALSE)
  }
  
  if (missing(mapping)) {
    mapping <- ""
  }
  
  if (length(mapping) > 1 || mapping[1] != "") {
    tmp_list <- as.list(rep(col_x, length(mapping)))
    names(tmp_list) <- mapping

    error_mapping <- mapping[!(mapping %in% c("linetype", "shape", "fill"))]
    tmp_list_error <- as.list(rep(col_x, length(error_mapping)))
    names(tmp_list_error) <- error_mapping
  } else {
    tmp_list <- list()
    tmp_list_error <- list()
  }
  
  plot_out <- ggplot2::ggplot(data = means, 
                              mapping = do.call(
                                what = ggplot2::aes_string, 
                                args = c(list(
                                  y = col_y, 
                                  x = col_x, 
                                  group = col_x),
                                  tmp_list)))
  
  
 if (data_plot) {
    if (missing(data_geom)) {
      if (!requireNamespace("ggbeeswarm", quietly = TRUE)) {
        stop("package ggbeeswarm is required.", call. = FALSE)
      }
      data_geom <- ggbeeswarm::geom_beeswarm
    }
    data_arg$alpha <- data_alpha
    plot_out <- plot_out +
      do.call(what = data_geom,
              args = c(
                data = list(data),
                data_arg
              )
      )
  }
  
  plot_out <- plot_out + 
    do.call(what = ggplot2::geom_point, 
            args = point_arg) 

  if (error_plot) {
    plot_out <- plot_out + 
      do.call(what = ggplot2::geom_errorbar, 
              args = c(
                mapping = list(do.call(
                    what = ggplot2::aes_string, 
                    args = c(list(
                      x = col_x, 
                      ymin = col_lower,
                      ymax = col_upper),
                      tmp_list_error))),
                error_arg,
                inherit.aes = list(FALSE)
              ))
  }
  
  if (length(unique(means$panel)) > 1) {
    plot_out <- plot_out + 
      ggplot2::facet_wrap(facets = "panel")
  }
  
  ## add labels
  if (!is.null(attr(means, "dv"))) {
    plot_out <- plot_out + 
      ggplot2::ylab(attr(means, "dv"))
  }
  if (!is.null(attr(means, "x"))) {
    plot_out <- plot_out + 
      ggplot2::xlab(attr(means, "x"))
  }
  if (!missing(legend_title)) {
    legend_title <- paste(legend_title, collapse = "\n")
    tmp_list <- rep(list(ggplot2::guide_legend(title = legend_title)), 
                    length(mapping))
    names(tmp_list) <- mapping
    plot_out <- plot_out + 
      do.call(what = ggplot2::guides, 
              args = tmp_list)
  }
  
  return(plot_out)
}
