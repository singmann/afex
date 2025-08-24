###if(getRversion() >= "2.15.1")  utils::globalVariables(c("error", "y", "x"))
#' @rdname afex_plot
#' @export
interaction_plot <- function(
  means,
  data,
  mapping = c("shape", "lineytpe"),
  error_plot = TRUE,
  error_arg = list(width = 0),
  data_plot = TRUE,
  data_geom = ggplot2::geom_point,
  data_alpha = 0.5,
  data_color = "darkgrey",
  data_arg = list(),
  point_arg = list(),
  line_arg = list(),
  dodge = 0.5,
  plot_first = NULL,
  legend_title,
  col_x = "x",
  col_y = "y",
  col_trace = "trace",
  col_panel = "panel",
  col_lower = "lower",
  col_upper = "upper"
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("package ggplot2 is required.", call. = FALSE)
  }
  if (missing(mapping)) {
    mapping <- c('shape', 'linetype')
  } else if (length(mapping) == 0) {
    stop(
      "mapping cannot be empty. Possible values: 'shape', 'color', 'linetype'.",
      call. = FALSE
    )
  }
  tmp_list <- vector("list", length(mapping))
  names(tmp_list) <- mapping
  for (i in seq_along(tmp_list)) {
    tmp_list[[i]] <- rlang::data_sym(col_trace)
  }

  error_mapping <- mapping[!(mapping %in% c("linetype", "shape", "fill"))]
  tmp_list_error <- vector("list", length(error_mapping))
  names(tmp_list_error) <- error_mapping
  for (i in seq_along(tmp_list_error)) {
    tmp_list_error[[i]] <- rlang::data_sym(col_trace)
  }

  plot_out <- ggplot2::ggplot(
    data = means,
    mapping = ggplot2::aes(
      y = .data[[col_y]],
      x = .data[[col_x]],
      group = .data[[col_trace]],
      !!!tmp_list
    )
  )
  if (!is.null(plot_first)) {
    if (is.list(plot_first)) {
      for (i in seq_along(plot_first)) {
        plot_out <- plot_out + plot_first[[i]]
      }
    } else {
      plot_out <- plot_out + plot_first
    }
  }

  if (data_plot) {
    if (missing(data_geom)) {
      data_geom <- ggplot2::geom_point
    }
    if (is.function(data_geom)) {
      if (!is.null(data_alpha)) {
        data_arg$alpha <- data_alpha
      }
      if (!is.null(data_color) & !any(c("color", "colour") %in% mapping)) {
        data_arg$color <- data_color
      }
      if (
        !("position" %in% names(data_arg)) &
          ("position" %in% names(formals(data_geom)))
      ) {
        data_arg$position = ggplot2::position_dodge(width = dodge)
      }
      plot_out <- plot_out +
        do.call(
          what = data_geom,
          args = c(
            mapping = list(
              ggplot2::aes(
                group = interaction(.data[[col_x]], .data[[col_trace]])
              )
            ),
            data = list(data),
            data_arg
          )
        )
    } else if (is.list(data_geom)) {
      ## https://stackoverflow.com/a/13433689/289572
      depth <- function(this) {
        ifelse(
          is.list(this),
          1L +
            as.numeric(all(as.logical(sapply(this, depth)))),
          0L
        )
      }
      if ((length(data_arg) == 0) | (depth(data_arg) == 1)) {
        if (!is.null(data_alpha)) {
          data_arg$alpha <- data_alpha
        }
        if (!is.null(data_color) & !any(c("color", "colour") %in% mapping)) {
          data_arg$color <- data_color
        }
        if (
          !("position" %in% names(data_arg)) &
            ("position" %in%
              unlist(sapply(data_geom, function(x) names(formals(x)))))
        ) {
          data_arg$position = ggplot2::position_dodge(width = dodge)
        }
        for (i in seq_along(data_geom)) {
          plot_out <- plot_out +
            do.call(
              what = data_geom[[i]],
              args = c(
                mapping = list(
                  ggplot2::aes(
                    group = interaction(.data[[col_x]], .data[[col_trace]])
                  )
                ),
                data = list(data),
                data_arg
              )
            )
        }
      } else {
        for (i in seq_along(data_arg)) {
          if (!is.null(data_alpha)) {
            data_arg[[i]]$alpha <- data_alpha
          }
          if (!is.null(data_color) & !any(c("color", "colour") %in% mapping)) {
            data_arg[[i]]$color <- data_color
          }
          if (
            !("position" %in% names(data_arg[[i]])) &
              ("position" %in% names(formals(data_geom[[i]])))
          ) {
            data_arg[[i]]$position = ggplot2::position_dodge(width = dodge)
          }
        }
        for (i in seq_along(data_geom)) {
          plot_out <- plot_out +
            do.call(
              what = data_geom[[i]],
              args = c(
                mapping = list(
                  ggplot2::aes(
                    group = interaction(.data[[col_x]], .data[[col_trace]])
                  )
                ),
                data = list(data),
                data_arg[[i]]
              )
            )
        }
      }
    }
  }
  for (i in levels(data$trace)) {
    tmp_means <- means
    tmp_means[means$trace != i, c(col_y, col_lower, col_upper)] <- NA
    #tmp_means <- tmp_means[means$trace == i,]
    plot_out <- plot_out +
      do.call(
        what = ggplot2::geom_point,
        args = c(
          data = list(tmp_means),
          position = list(
            ggplot2::position_dodge(width = dodge)
          ),
          point_arg,
          na.rm = list(TRUE)
        )
      ) +
      do.call(
        what = ggplot2::geom_line,
        args = c(
          data = list(tmp_means),
          position = list(
            ggplot2::position_dodge(width = dodge)
          ),
          line_arg,
          na.rm = list(TRUE)
        )
      )

    if (error_plot) {
      plot_out <- plot_out +
        do.call(
          what = ggplot2::geom_errorbar,
          args = c(
            data = list(tmp_means),
            mapping = list(ggplot2::aes(
              x = .data[[col_x]],
              ymin = .data[[col_lower]],
              ymax = .data[[col_upper]],
              group = .data[[col_trace]],
              !!!tmp_list_error
            )),
            position = list(ggplot2::position_dodge(width = dodge)),
            error_arg,
            na.rm = list(TRUE),
            inherit.aes = list(FALSE)
          )
        )
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
  mapping <- mapping[nzchar(mapping)]
  if (!missing(legend_title) && length(mapping) > 0) {
    legend_title <- paste(legend_title, collapse = "\n")
    tmp_list <- rep(
      list(ggplot2::guide_legend(title = legend_title)),
      length(mapping)
    )
    names(tmp_list) <- mapping
    plot_out <- plot_out +
      do.call(what = ggplot2::guides, args = tmp_list)
  }

  return(plot_out)
}


#' @rdname afex_plot
#' @export
oneway_plot <- function(
  means,
  data,
  mapping = "",
  error_plot = TRUE,
  error_arg = list(width = 0),
  data_plot = TRUE,
  data_geom = ggbeeswarm::geom_beeswarm,
  data_alpha = 0.5,
  data_color = "darkgrey",
  data_arg = list(),
  point_arg = list(),
  plot_first = NULL,
  legend_title,
  col_x = "x",
  col_y = "y",
  col_panel = "panel",
  col_lower = "lower",
  col_upper = "upper"
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("package ggplot2 is required.", call. = FALSE)
  }

  if (missing(mapping)) {
    mapping <- ""
  }

  if (length(mapping) > 1 || mapping[1] != "") {
    tmp_list <- vector("list", length(mapping))
    names(tmp_list) <- mapping
    for (i in seq_along(tmp_list)) {
      tmp_list[[i]] <- rlang::data_sym(col_x)
    }

    error_mapping <- mapping[!(mapping %in% c("linetype", "shape", "fill"))]
    tmp_list_error <- vector("list", length(error_mapping))
    names(tmp_list_error) <- error_mapping
    for (i in seq_along(tmp_list_error)) {
      tmp_list_error[[i]] <- rlang::data_sym(col_x)
    }
  } else {
    tmp_list <- list()
    tmp_list_error <- list()
  }

  plot_out <- ggplot2::ggplot(
    data = means,
    mapping = ggplot2::aes(
      y = .data[[col_y]],
      x = .data[[col_x]],
      group = .data[[col_x]],
      !!!tmp_list
    )
  )

  if (!is.null(plot_first)) {
    if (is.list(plot_first)) {
      for (i in seq_along(plot_first)) {
        plot_out <- plot_out + plot_first[[i]]
      }
    } else {
      plot_out <- plot_out + plot_first
    }
  }

  if (data_plot) {
    if (missing(data_geom)) {
      if (!requireNamespace("ggbeeswarm", quietly = TRUE)) {
        stop("package ggbeeswarm is required.", call. = FALSE)
      }
      data_geom <- ggbeeswarm::geom_beeswarm
    }
    if (is.function(data_geom)) {
      if (!is.null(data_alpha)) {
        data_arg$alpha <- data_alpha
      }
      if (!is.null(data_color) & !any(c("color", "colour") %in% mapping)) {
        data_arg$color <- data_color
      }
      plot_out <- plot_out +
        do.call(
          what = data_geom,
          args = c(
            data = list(data),
            data_arg
          )
        )
    } else if (is.list(data_geom)) {
      ## https://stackoverflow.com/a/13433689/289572
      depth <- function(this) {
        ifelse(
          is.list(this),
          1L +
            as.numeric(all(as.logical(sapply(this, depth)))),
          0L
        )
      }
      if (depth(data_arg) == 1) {
        if (!is.null(data_alpha)) {
          data_arg$alpha <- data_alpha
        }
        if (!is.null(data_color) & !any(c("color", "colour") %in% mapping)) {
          data_arg$color <- data_color
        }
        for (i in seq_along(data_geom)) {
          plot_out <- plot_out +
            do.call(
              what = data_geom[[i]],
              args = c(
                data = list(data),
                data_arg
              )
            )
        }
      } else {
        for (i in seq_along(data_arg)) {
          if (!is.null(data_alpha)) {
            data_arg[[i]]$alpha <- data_alpha
          }
          if (!is.null(data_color) & !any(c("color", "colour") %in% mapping)) {
            data_arg[[i]]$color <- data_color
          }
        }
        data_arg$alpha <- data_alpha
        for (i in seq_along(data_geom)) {
          plot_out <- plot_out +
            do.call(
              what = data_geom[[i]],
              args = c(
                data = list(data),
                data_arg[[i]]
              )
            )
        }
      }
    }
  }

  plot_out <- plot_out +
    do.call(what = ggplot2::geom_point, args = point_arg)

  if (error_plot) {
    plot_out <- plot_out +
      do.call(
        what = ggplot2::geom_errorbar,
        args = c(
          mapping = list(ggplot2::aes(
            x = .data[[col_x]],
            ymin = .data[[col_lower]],
            ymax = .data[[col_upper]],
            !!!tmp_list_error
          )),
          error_arg,
          na.rm = list(TRUE),
          inherit.aes = list(FALSE)
        )
      )
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
  mapping <- mapping[nzchar(mapping)]
  if (!missing(legend_title) && length(mapping) > 0) {
    legend_title <- paste(legend_title, collapse = "\n")
    tmp_list <- rep(
      list(ggplot2::guide_legend(title = legend_title)),
      length(mapping)
    )
    names(tmp_list) <- mapping
    plot_out <- plot_out +
      do.call(what = ggplot2::guides, args = tmp_list)
  }

  return(plot_out)
}
