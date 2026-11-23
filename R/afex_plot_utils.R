afex_plot_internal <- function(
  x,
  trace,
  panel,
  means,
  data,
  error_plot,
  error_arg,
  dodge,
  data_plot,
  data_geom,
  data_alpha,
  data_color,
  data_arg,
  point_arg,
  line_arg,
  mapping,
  legend_title,
  plot_first,
  return
) {
  if (length(trace) > 0) {
    means$trace <- interaction(means[trace], sep = "\n")
    data$trace <- interaction(data[trace], sep = "\n")

    if (return == "data") {
      return(list(means = means, data = data))
    } else if (return == "plot") {
      return(interaction_plot(
        means = means,
        data = data,
        error_plot = error_plot,
        error_arg = error_arg,
        dodge = dodge,
        data_plot = data_plot,
        data_geom = data_geom,
        data_alpha = data_alpha,
        data_arg = data_arg,
        data_color = data_color,
        point_arg = point_arg,
        line_arg = line_arg,
        mapping = mapping,
        plot_first = plot_first,
        legend_title = if (missing(legend_title)) {
          paste(trace, sep = "\n")
        } else {
          legend_title
        }
      ))
    }
  } else {
    if (return == "data") {
      return(list(means = means, data = data))
    } else if (return == "plot") {
      return(oneway_plot(
        means = means,
        data = data,
        error_plot = error_plot,
        error_arg = error_arg,
        data_plot = data_plot,
        data_geom = data_geom,
        data_alpha = data_alpha,
        data_color = data_color,
        data_arg = data_arg,
        point_arg = point_arg,
        mapping = mapping,
        plot_first = plot_first,
        legend_title = if (missing(legend_title)) {
          paste(x, sep = "\n")
        } else {
          legend_title
        }
      ))
    }
  }
}

se <- function(x, na.rm = FALSE) sd(x, na.rm = na.rm) / sqrt(length(x))

rename_factor_levels <- function(data, factor_levels, status_message = TRUE) {
  if (length(factor_levels) > 0) {
    if (is.null(names(factor_levels))) {
      stop("factor_levels needs to be a named list.", call. = FALSE)
    }
    if (any(!(names(factor_levels) %in% colnames(data)))) {
      if (status_message) {
        warning(
          "factor_levels: No factor named ",
          paste(
            paste0(
              "'",
              names(factor_levels)[!(names(factor_levels) %in% colnames(data))],
              "'"
            ),
            collapse = ", "
          ),
          " in data.",
          call. = FALSE
        )
      }
      factor_levels <- factor_levels[names(factor_levels) %in% colnames(data)]
    }
    for (i in seq_along(factor_levels)) {
      if (is.null(names(factor_levels[[i]]))) {
        if (
          length(factor_levels[[i]]) !=
            length(levels(data[[names(factor_levels)[i]]]))
        ) {
          stop(
            "length of new factor_levels for '",
            names(factor_levels)[i],
            "' != length of factor levels.",
            call. = FALSE
          )
        }
        names(factor_levels[[i]]) <- levels(data[[names(factor_levels)[i]]])
      }
      if (!is.factor(data[[names(factor_levels)[i]]])) {
        data[[names(factor_levels)[i]]] <- factor(data[[names(factor_levels)[
          i
        ]]])
      }
      factor_levels[[i]] <- factor_levels[[i]][
        names(factor_levels[[i]]) %in% levels(data[[names(factor_levels)[i]]])
      ]
      if (status_message) {
        message(
          "Renaming/reordering factor levels of '",
          names(factor_levels)[i],
          "':\n  ",
          paste(
            paste(
              levels(data[[names(factor_levels)[i]]])[
                match(
                  names(factor_levels[[i]]),
                  levels(data[[names(factor_levels)[i]]])
                )
              ],
              factor_levels[[i]],
              sep = " -> "
            ),
            collapse = "\n  "
          )
        )
      }
      if (
        length(factor_levels[[i]]) ==
          length(levels(data[[names(factor_levels)[i]]]))
      ) {
        data[[names(factor_levels)[i]]] <- factor(
          x = data[[names(factor_levels)[i]]],
          levels = names(factor_levels[[i]]),
          labels = factor_levels[[i]]
        )
      } else {
        levels(data[[names(factor_levels)[i]]])[
          match(
            names(factor_levels[[i]]),
            levels(data[[names(factor_levels)[i]]])
          )
        ] <- factor_levels[[i]]
      }
    }
  }
  data
}

get_emms <- function(
  object,
  x,
  trace,
  panel,
  emmeans_arg,
  factor_levels,
  level
) {
  if (!requireNamespace("emmeans", quietly = TRUE)) {
    stop("package emmeans is required.", call. = FALSE)
  }

  all_vars <- c(x, trace, panel)
  emmeans_arg$options$level <- level
  if (!inherits(object, "afex_aov") || !"type" %in% names(emmeans_arg)) {
    emmeans_arg <- c(list(type = "response"), emmeans_arg)
  }
  grid <- do.call(
    emmeans::emmeans,
    args = c(
      object = list(object),
      specs = list(all_vars),
      emmeans_arg
    )
  )
  emms <- as.data.frame(grid)
  if (inherits(object, "afex_aov") && !is.null(attr(object, "transf"))) {
    regrid <- if (is.null(emmeans_arg$regrid)) {
      emmeans_arg$transform
    } else {
      emmeans_arg$regrid
    }
    attr(emms, "response_transformation") <- plot_response_transformation(
      object, grid, emms, regrid
    )
  }
  emms <- rename_factor_levels(
    data = emms,
    factor_levels = factor_levels,
    status_message = TRUE
  )

  emms$x <- interaction(emms[x], sep = "\n")
  #col_y <- colnames(emms)[which(colnames(emms) == "SE")-1]
  if (any(colnames(emms) == "SE")) {
    colnames(emms)[which(colnames(emms) == "SE") - 1] <- "y"
  } else {
    colnames(emms)[grep("CL|HPD", colnames(emms))[1] - 1] <- "y"
  }
  attr(emms, "dv") <- attr(object, "dv")
  attr(emms, "x") <- paste(x, collapse = " - ")
  if (length(panel) > 0) {
    emms$panel <- interaction(emms[panel], sep = "\n")
  } else {
    emms$panel <- "1"
  }
  emms$all_vars <- interaction(emms[all_vars], sep = ".")

  return(emms)
}

plot_response_transformation <- function(object, grid, emms, regrid) {
  if (!inherits(grid, "emmGrid")) return(NULL)
  misc <- grid@misc
  response <- isTRUE(attr(emms, "type") %in% c("response", "mu", "unlink"))
  original <- attr(object, "transf")
  requested_regrid <- isTRUE(regrid) || is.list(regrid) ||
    (is.character(regrid) && is.na(pmatch(regrid, c("none", "pass"))))
  regridded <- requested_regrid && (isTRUE(misc$regrid.flag) || !identical(misc$tran, original))
  dv <- attr(object, "dv")
  response_dv <- substring(dv, nchar(original[[1]]) + 2L)

  # Regridding can replace the grid's transformation. First invert the one
  # used to fit the model, then apply the grid's link for fitted-scale summaries.
  if (regridded) {
    original_link <- afex_plot_link(original)
    inverse <- if (is.null(original_link)) identity else original_link$linkinv
    if (!is.null(original_link)) dv <- response_dv
    link <- afex_plot_link(misc$tran)
    if (!response && !is.null(link)) {
      return(list(
        apply = function(y) link$linkfun(inverse(y)),
        dv = paste0(link$name, "(", dv, ")")
      ))
    }
    return(list(apply = inverse, dv = dv))
  }
  if (!response) {
    return(NULL)
  }
  link <- afex_plot_link(misc$tran)
  if (is.null(link)) return(NULL)
  multiplier <- if (is.null(misc$tran.mult)) 1 else misc$tran.mult
  offset <- if (is.null(misc$tran.offset)) 0 else misc$tran.offset
  list(
    apply = function(y) link$linkinv(y / multiplier) - offset,
    dv = response_dv
  )
}

afex_plot_link <- function(transform) {
  if (is.list(transform)) return(transform)
  if (length(transform) != 1L) return(NULL)
  # Parameterized links need the link object, not just the constructor name.
  if (transform %in% c("genlog", "power", "boxcox", "sympower", "bcnPower", "scale")) {
    return(NULL)
  }
  link <- tryCatch(emmeans::make.tran(transform), error = function(e) NULL)
  if (is.null(link)) {
    # Older emmeans releases leave standard links to stats::make.link().
    link <- switch(
      transform,
      log2 = list(name = "log2", linkfun = log2, linkinv = function(y) 2^y),
      log10 = list(name = "log10", linkfun = log10, linkinv = function(y) 10^y),
      exp = list(name = "exp", linkfun = exp, linkinv = log),
      "/" = ,
      reciprocal = stats::make.link("inverse"),
      asinh.sqrt = list(
        name = "asinh(sqrt(mu))", linkfun = function(y) asinh(sqrt(y)),
        linkinv = function(y) sinh(y)^2
      ),
      tryCatch(stats::make.link(transform), error = function(e) NULL)
    )
  }
  if (isTRUE(link$unknown)) return(NULL)
  link
}

prep_data <- function(data, x, trace, panel, factor_levels, dv_col, id) {
  all_vars <- c(x, trace, panel)

  data <- rename_factor_levels(
    data = data,
    factor_levels = factor_levels,
    status_message = FALSE
  )

  ## we have to make sure that if data already contains a column y, the correct
  ## one is used for aggregation here.
  ## the old code below led to problems in this case
  # colnames(data)[colnames(data) == dv_col] <- "y"
  # if (!is.numeric(data$y)) {
  #   message("transforming dv to numerical scale")
  #   data$y <- as.numeric(data$y)
  # }
  # data <- aggregate(data$y, by = data[c(all_vars,id)],
  #                   FUN = mean, drop = TRUE)
  #colnames(data)[colnames(data) == dv_col] <- "y"

  if (!is.numeric(data[[dv_col]])) {
    message("transforming dv to numerical scale")
    if (is.factor(data[[dv_col]])) {
      data[[dv_col]] <- as.numeric(data[[dv_col]]) - 1
    }
    data[[dv_col]] <- as.numeric(data[[dv_col]])
  }
  data <- aggregate(
    data[[dv_col]],
    by = data[c(all_vars, id)],
    FUN = mean,
    drop = TRUE
  )

  data$y <- data$x
  data$x <- interaction(data[x], sep = "\n")
  if (length(panel) > 0) {
    data$panel <- interaction(data[panel], sep = "\n")
  } else {
    data$panel <- "1"
  }
  data$all_vars <- interaction(data[all_vars], sep = ".")
  return(data)
}

get_plot_var <- function(x) {
  if (missing(x)) {
    return()
  }
  if (inherits(x, "formula")) {
    return(all.vars(x[[2]]))
  } else {
    return(x)
  }
}

get_data_based_cis <- function(
  emms,
  data,
  error,
  id, ## colname holding the id/grouping variable
  all_vars,
  within_vars,
  between_vars,
  error_level,
  error_ci
) {
  error_plot <- TRUE
  ## SE/CI calculation:
  if (error == "model") {
    emms$error <- emms$SE
    # emms$lower <- emms$lower.CL
    # emms$upper <- emms$upper.CL
    col_cis <- grep("CL|HPD", colnames(emms), value = TRUE)
    col_cis <- col_cis[!(col_cis %in% all_vars)]
    emms$lower <- emms[, col_cis[1]]
    emms$upper <- emms[, col_cis[2]]
  } else if (error == "mean") {
    ses <- tapply(data$y, INDEX = list(data$all_vars), FUN = se)
    sizes <- tapply(data$y, INDEX = list(data$all_vars), FUN = length)
    stopifnot(emms$all_vars %in% names(ses))
    emms$error <- ses[emms$all_vars]
    emms$lower <- emms$y - qt(1 - (1 - error_level) / 2, sizes - 1) * emms$error
    emms$upper <- emms$y + qt(1 - (1 - error_level) / 2, sizes - 1) * emms$error
  } else if (error %in% c("CMO", "within")) {
    if (length(within_vars) == 0) {
      stop(
        "within-subject SE only possible if within-subject factors present.",
        call. = FALSE
      )
    }
    within_fac <- interaction(data[within_vars], sep = ".")
    indiv_means <- tapply(data$y, INDEX = data[id], FUN = mean)
    J <- length(levels(within_fac))
    ## Cosineau & O'Brien (2014), Equation 2:
    new_y <- data$y -
      indiv_means[as.character(data[, id])] +
      mean(data$y)
    ## Cosineau & O'Brien (2014), Equation 4:
    y_bar <- tapply(new_y, INDEX = within_fac, FUN = mean)
    new_z <- sqrt(J / (J - 1)) * (new_y - y_bar[within_fac]) + y_bar[within_fac]
    ses <- tapply(new_z, INDEX = list(data$all_vars), FUN = se)
    sizes <- tapply(new_z, INDEX = list(data$all_vars), FUN = length)
    stopifnot(emms$all_vars %in% names(ses))
    emms$error <- ses[emms$all_vars]
    emms$lower <- emms$y - qt(1 - (1 - error_level) / 2, sizes - 1) * emms$error
    emms$upper <- emms$y + qt(1 - (1 - error_level) / 2, sizes - 1) * emms$error
  } else if (error == "between") {
    if (length(between_vars) > 0) {
      between_fac <- interaction(data[between_vars], sep = ".")
    } else {
      between_fac <- factor(rep("1", nrow(data)))
    }
    indiv_means <- aggregate(
      data$y,
      by = list(
        data[, id],
        between_fac
      ),
      FUN = mean
    )
    ses <- tapply(indiv_means$x, INDEX = indiv_means[["Group.2"]], FUN = se)
    sizes <- tapply(
      indiv_means$x,
      INDEX = indiv_means[["Group.2"]],
      FUN = length
    )

    if (length(between_vars) == 0) {
      emms$error <- ses
      emms$lower <- emms$y -
        qt(1 - (1 - error_level) / 2, sizes - 1) * emms$error
      emms$upper <- emms$y +
        qt(1 - (1 - error_level) / 2, sizes - 1) * emms$error
    } else {
      emm_between <- interaction(emms[between_vars], sep = ".")
      emms$error <- ses[emm_between]
      emms$lower <- emms$y -
        qt(1 - (1 - error_level) / 2, sizes[emm_between] - 1) * emms$error
      emms$upper <- emms$y +
        qt(1 - (1 - error_level) / 2, sizes[emm_between] - 1) * emms$error
    }
  } else if (error == "none") {
    emms$error <- NA_real_
    emms$lower <- NA_real_
    emms$upper <- NA_real_
    error_plot <- FALSE
  }

  if (!error_ci) {
    emms$lower <- emms$y - emms$error
    emms$upper <- emms$y + emms$error
  }

  return(list(emms = emms, error_plot = error_plot))
}
