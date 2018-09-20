#' m-way Interaction Plot with Error Bars and Raw Data
#' 
#' @description Plots results from factorial experiments. Estimated marginal 
#'   means and error bars are plotted in the foreground, raw data is plotted in 
#'   the background. Error bars can be based on different standard errors (e.g.,
#'   model-based, within-subjects, between-subjects). Functions described here 
#'   are extremely flexible and allow customization of almost any characteristic
#'   of the plot.
#'   
#'   \code{afex_plot} is the user friendly function that does data preparation
#'   and plotting. It also allows to only return the prepared data (\code{return
#'   = "data"}).
#'   
#'   \code{interaction_plot} does the plotting when a \code{trace} factor is
#'   present. \code{oneway_plot} does the plotting when a \code{trace} factor is
#'   absent.
#'   
#' @param object object of class \code{afex_aov} as returned from 
#'   \code{\link{aov_car}} and related functions.
#' @param x A \code{character} vector or one-sided \code{formula} specifying the
#'   factor names of the predictors displayed on the x-axis.
#' @param trace A \code{character} vector or one-sided \code{formula} specifying
#'   the factor names of the predictors connected by the same line. Argument 
#'   \code{mapping} specifies further mappings for these factors. Optional.
#' @param panel A \code{character} vector or one-sided \code{formula} 
#'   specifying the factor names of the predictors shown in different panels. 
#'   Optional.
#' @param mapping A \code{character} vector specifying which aesthetic mappings 
#'   should be applied to either the \code{trace} factors (if \code{trace} is 
#'   specified) or the \code{x} factors. Useful options are any combination of 
#'   \code{"shape"}, \code{"color"}, and \code{"linetype"} or also \code{"fill"}
#'   (see examples). The default (i.e., missing) uses \code{c("shape",
#'   "lineytpe")} if \code{trace} is specified, otherwise \code{""} (i.e., no
#'   additional aesthetic).
#' @param error A scalar \code{character} vector specifying which type of
#'   standard error should be plotted. Default is \code{"model"}, which plots
#'   model-based standard errors. Further options are: \code{"none"} (or
#'   \code{NULL}), \code{"mean"}, \code{"within"} (or \code{"CMO"}), and 
#'   \code{"between"}. See details.
#' @param error_ci Logical. Should error bars plot confidence intervals
#'   (=\code{TRUE}, the default) or standard errors (=\code{FALSE})?
#' @param error_level Numeric value between 0 and 1 determing the width of the
#'   confidence interval. Default is .95 corresponding to a 95\% confidence
#'   interval.
#' @param error_arg A \code{list} of further arguments passed to 
#'   \code{\link[ggplot2]{geom_errorbar}}, which draws the errorsbars. Default 
#'   is \code{list(width = 0)} which suppresses the vertical bars at the end of 
#'   the error bar.
#' @param data_plot \code{logical}. Should raw data be plotted in the 
#'   background? Default is \code{TRUE}.
#' @param data_geom Geom \code{function} used for plotting data in background. 
#'   The default (missing) uses \code{\link[ggplot2]{geom_point}} if \code{trace}
#'   is specified, otherwise \code{\link[ggbeeswarm]{geom_beeswarm}}. See
#'   examples.
#' @param data_alpha \code{alpha} value passed to \code{data_geom}.
#' @param data_arg A \code{list} of further arguments passed to 
#'   \code{data_geom}. Default is \code{list(color = "darkgrey")}.
#' @param point_arg,line_arg A \code{list} of further arguments passed to 
#'   \code{\link[ggplot2]{geom_point}} or \code{\link[ggplot2]{geom_line}} which
#'   draw the points and lines in the foreground. Default is \code{list()}.
#'   \code{line_arg} is only used if \code{trace} is specified.
#' @param emmeans_arg A \code{list} of further arguments passed to 
#'   \code{\link[emmeans]{emmeans}}. Of particular importance for ANOVAs is 
#'   \code{model}, see \code{\link{afex_aov-methods}}.
#' @param dodge Numerical amount of dodging of factor-levels on x-axis. Default 
#'   is \code{0.2}.
#' @param return A scalar \code{character} specifying what should be returned. 
#'   The default \code{"plot"} returns the \pkg{ggplot2} plot. The other option 
#'   \code{"data"} returns a list with two \code{data.frame}s containing the 
#'   data used for plotting: \code{means} contains the means and standard errors
#'   for the foreground, \code{data} contains the raw data in the background.
#' @param new_levels A \code{list} of new factor levels that should be used in 
#'   the plot. The name of each list entry needs to correspond to one of the 
#'   factors in the plot.
#' @param means,data \code{data.frame}s used for plotting. 
#' @param col_y,col_x,col_trace,col_panel A scalar \code{character} string 
#'   specifying the name of the corresponding column containing the information
#'   used for plotting. Each column needs to exist in both the \code{means} and
#'   the \code{data} \code{data.frame}.
#'@param col_lower,col_upper A scalar \code{character} string specifying the 
#'  name of the columns containing lower and upper bounds for the error bars. 
#'  These columns need to exist in \code{means}.
#' @param error_plot \code{logical}. Should error bars be plotted? Only used in 
#'   plotting functions. To suppress plotting of error bars use \code{error =
#'   "none"} in \code{afex_plot}.
#' @param ... currently ignored.
#' 
#' @details \code{afex_plot} obtains the estimated marginal means via 
#'   \code{\link[emmeans]{emmeans}} and aggregates the raw data to the same 
#'   level. It then calculates the desired confidence interval or standard error
#'   (see below) and passes the prepared data to one of the two plotting
#'   functions: \code{interaction_plot} when \code{trace} is specified and 
#'   \code{oneway_plot} otherwise.
#' 
#'   \subsection{Error Bars}{Error bars provide a grahical representation of the
#'   variability of the estimated means and should be routinely added to results
#'   figures. However, there exist several possibilities which particular 
#'   measure of variability to use. Because of this, any figure depicting error 
#'   bars should be accompanied by a note detailing which measure the error bars
#'   shows. The present functions allow plotting of different types of
#'   confidence intervals (if \code{error_ci = TRUE}, the default) or standard 
#'   errors (if \code{error_ci = FALSE}).
#'   
#'   A further complication is that readers routinely misinterpret confidence 
#'   intervals. The most common error is to assume that non-overlapping error 
#'   bars indicate a significant difference (e.g., Belia et al., 2005). This is 
#'   rarely the case (see e.g., Cumming & Finch, 2005; Knol et al., 2011; 
#'   Schenker & Gentleman, 2005). For example, in a fully between-subject design
#'   in which the error bars depict 95% confidence intervals and groups are of 
#'   approximately equal size and have equal variance, even error bars that 
#'   overlap by as much as 50\% still correspond to \emph{p} < .05. Error bars 
#'   that are just touching roughly correspond to \emph{p} = .01.
#'   
#'   In the case of repeated-measures designs the usual confidence intervals or
#'   standard errors (i.e., model-based confidence intervals or intervals based
#'   on the standard error of the mean) cannot be used to gauge significant
#'   differences as this requires knowledge about the correlation between
#'   measures. One popular alternative in the psychological literature are
#'   intervals based on within-subject standard errors/confidence intervals
#'   (e.g., Cousineau & O'Brien, 2014). These attempt to control for the
#'   correlation across individuals and thereby allow judging differences
#'   between repeated-measures condition. As a downside, when using
#'   within-subject intervals no comparisons across between-subject conditions
#'   or with respect to a fixed-value are possible anymore.
#'   
#'   In the case of a mixed-design, no single type of error bar is possible that
#'   allows comparison across all conditions. Therefore, special care is
#'   necessary in such cases.
#'   
#'   The following "rules of eye" (Cumming and Finch, 2005) hold, when permitted
#'   by design (i.e., within-subject bars for within-subject comparisons; other 
#'   variants for between-subject comparisons), and groups are approximately 
#'   equal in size and variance:
#'   \itemize{
#'     \item  \emph{p} < .05 when the overlap of the 95\% confidence intervals
#'     (CIs) is no more than about half the average margin of error, that is,
#'     when proportion overlap is about .50 or less.
#'     \item \emph{p} < .01 when the two CIs do not overlap, that is, when
#'     proportion overlap is about 0 or there is a positive gap.
#'     \item  \emph{p} < .05 when the gap between standard error (SE) bars is at
#'     least about the size of the average SE, that is, when the proportion gap
#'     is about 1 or greater.
#'     \item \emph{p} < .01 when the proportion gap between SE bars is about 2
#'     or more.
#'   }   
#'   }
#'   \subsection{Implemented Standard Errors}{The following lists the 
#'   implemented approaches to calculate confidence intervals (CIs) and standard
#'   errors (SEs). CIs are based on the SEs using the \emph{t}-distribution with
#'   degrees of freedom based on the cell or group size.
#'   \describe{
#'     \item{\code{"model"}}{Uses model-based CIs and SEs. For ANOVAs, the
#'     variant based on the \code{lm} or \code{mlm} model (i.e.,
#'     \code{emmeans_arg = list(model = "multivariate")}) seems generally
#'     preferrable.}
#'     \item{\code{"mean"}}{Calculates the standard error of the mean for
#'     each cell ignoring any repeated-measures factors.}
#'     \item{\code{"within-SE"} or \code{"CMO"}}{Calculates within-subject SEs
#'     using the Cosineau-Morey-O'Brien (Cousineau & O'Brien, 2014) method. This
#'     method is based on a double normalization of the data. SEs and CIs are
#'     then calculated independently for each cell (i.e., if the desired output
#'     contains between-subject factors, SEs are calculated for each cell
#'     including the between-subject factors).}
#'     \item{\code{"between-SE"}}{First aggregates the data per participant and 
#'     then calculates the SEs for each between-subject condition. Results in 
#'     one SE and \emph{t}-qutnile for all conditions in purely within-subjects
#'     designs.}
#'     \item{\code{"none"} or \code{NULL}}{Suppresses calculation of SEs and
#'     plots no error bars.}
#'   }
#'   }
#'   
#' @return Returns a \pkg{ggplot2} plot (i.e., object of class \code{c("gg",
#'   "ggplot")}) unless \code{return = "data"}.
#' 
#' @references Belia, S., Fidler, F., Williams, J., & Cumming, G. (2005).
#'   Researchers Misunderstand Confidence Intervals and Standard Error Bars.
#'   \emph{Psychological Methods}, 10(4), 389-396.
#'   https://doi.org/10.1037/1082-989X.10.4.389
#'   
#'   Cousineau, D., & O'Brien, F. (2014). Error bars in within-subject designs:
#'   a comment on Baguley (2012). \emph{Behavior Research Methods}, 46(4),
#'   1149-1151. https://doi.org/10.3758/s13428-013-0441-z
#'   
#'   Cumming, G., & Finch, S. (2005). Inference by Eye: Confidence Intervals and
#'   How to Read Pictures of Data. \emph{American Psychologist}, 60(2), 170-180.
#'   https://doi.org/10.1037/0003-066X.60.2.170
#'   
#'   Knol, M. J., Pestman, W. R., & Grobbee, D. E. (2011). The (mis)use of
#'   overlap of confidence intervals to assess effect modification.
#'   \emph{European Journal of Epidemiology}, 26(4), 253-254.
#'   https://doi.org/10.1007/s10654-011-9563-8
#'   
#'   Schenker, N., & Gentleman, J. F. (2001). On Judging the Significance of
#'   Differences by Examining the Overlap Between Confidence Intervals.
#'   \emph{The American Statistician}, 55(3), 182-186.
#'   https://doi.org/10.1198/000313001317097960
#'   
#'   
#' @importFrom stats aggregate sd qt
#' 
#' @example examples/examples.afex_plot.R
#'   
#' @export
afex_plot <- function(object, ...) UseMethod("afex_plot", object)


# @method afex_plot afex_aov
#' @rdname afex_plot
#' @export
afex_plot.afex_aov <- function(object, 
                               x,
                               trace,
                               panel,
                               mapping,
                               error = "model",
                               error_ci = TRUE,
                               error_level = 0.95, 
                               error_arg = list(width = 0),
                               data_plot = TRUE,
                               data_geom,
                               data_alpha = 0.5,
                               data_arg = list(color = "darkgrey"),
                               point_arg = list(),
                               line_arg = list(),
                               emmeans_arg = list(),
                               dodge = 0.2,
                               return = "plot",
                               new_levels = list(),
                               ...) {
  
  return <- match.arg(return, c("plot", "data"))
  error <- match.arg(error, c("none", 
                              "model", 
                              "mean", 
                              "within", "CMO",
                              "between"))
  
  x <- get_plot_var(x)
  trace <- get_plot_var(trace)
  panel <- get_plot_var(panel)
  all_vars <- c(x, trace, panel)
  
  emms <- get_emms(object = object, 
                   x = x,
                   trace = trace,
                   panel = panel,
                   emmeans_arg = emmeans_arg, 
                   new_levels = new_levels,
                   level = error_level)
  
  ## prepare raw (i.e., participant by cell) data
  data <- object$data$long
  for (i in seq_along(new_levels)) {
    levels(data[[names(new_levels)[i]]]) <- new_levels[[i]]
  }
  colnames(data)[colnames(data) == attr(object, "dv")] <- "y"
  data <- aggregate(data$y, by = data[c(all_vars,attr(object, "id"))], 
                    FUN = mean, drop = TRUE)
  data$y <- data$x
  data$x <- interaction(data[x], sep = "\n")
  data$all_vars <- interaction(data[all_vars], sep = ".")
  
  ### prepare variables for SE calculation
  plot_error <- TRUE
  within_vars <- all_vars[all_vars %in% names(attr(object, "within"))]
  if (length(within_vars) > 0) {
    within_fac <- interaction(data[within_vars], sep = ".")
  } else {
    within_fac <- factor(rep("1", nrow(data)))
  }
  
  between_vars <- all_vars[all_vars %in% names(attr(object, "between"))]
  if (length(between_vars) > 0) {
    between_fac <- interaction(data[between_vars], sep = ".")
  } else {
    between_fac <- factor(rep("1", nrow(data)))
  }
  
  if (error %in% c("model", "mean", "between") && 
      all(c(x, trace) %in% within_vars)) {
    warning("Panel(s) show within-subjects factors, but no within-subjects error bars.", call. = FALSE)
  } else if (error %in% c("within", "CMO") && 
      all(c(x, trace) %in% between_vars)) {
    warning("Panel(s) show between-subjects factors, but within-subjects error bars.", call. = FALSE)
  } else if (any(between_vars %in% c(x, trace)) && 
             any(within_vars %in% c(x, trace)) && 
             error != "none") {
    warning("Panel(s) show a mixed within-between-design.\nError bars do not allow comparisons across all means.", call. = FALSE)
  }
  
  ## SE/CI calculation:
  if (error == "model") {
    emms$error <- emms$SE
    emms$lower <- emms$lower.CL
    emms$upper <- emms$upper.CL
  } else if (error == "mean") {
    ses <- tapply(data$y, INDEX = list(data$all_vars), FUN = se)
    sizes <- tapply(data$y, INDEX = list(data$all_vars), FUN = length)
    stopifnot(emms$all_vars == names(ses))
    emms$error <- ses
    emms$lower <- emms$y - qt(1-(1-error_level)/2, sizes - 1) * emms$error
    emms$upper <- emms$y + qt(1-(1-error_level)/2, sizes - 1) * emms$error
  } else if (error %in% c("CMO", "within")) {
    if (length(within_vars) == 0) {
      stop("within-subject SE only possible if within-subject factors present.", 
           call. = FALSE)
    }
    indiv_means <- tapply(data$y, INDEX = data[attr(object, "id")], FUN = mean)
    J <- length(levels(within_fac))
    ## Cosineau & O'Brien (2014), Equation 2:
    new_y <- data$y - 
      indiv_means[as.character(data[,attr(object, "id")])] +
      mean(data$y)
    ## Cosineau & O'Brien (2014), Equation 4:
    y_bar <- tapply(new_y, INDEX = within_fac, FUN = mean)
    new_z <- sqrt(J / (J-1)) * (new_y - y_bar[within_fac]) + y_bar[within_fac]
    ses <- tapply(new_z, INDEX = list(data$all_vars), FUN = se)
    sizes <- tapply(new_z, INDEX = list(data$all_vars), FUN = length)
    stopifnot(emms$all_vars == names(ses))
    emms$error <- ses
    emms$lower <- emms$y - qt(1-(1-error_level)/2, sizes - 1) * emms$error
    emms$upper <- emms$y + qt(1-(1-error_level)/2, sizes - 1) * emms$error
  } else if (error == "between") {
    indiv_means <- aggregate(data$y, 
                             by = list(
                               data[,attr(object, "id")],
                               between_fac), 
                             FUN = mean)
    ses <- tapply(indiv_means$x, INDEX = indiv_means[["Group.2"]], FUN = se)
    sizes <- tapply(indiv_means$x, INDEX = indiv_means[["Group.2"]], FUN = length)
    
    if (length(between_vars) == 0) {
      emms$error <- ses  
      emms$lower <- emms$y - qt(1-(1-error_level)/2, sizes - 1) * emms$error
      emms$upper <- emms$y + qt(1-(1-error_level)/2, sizes - 1) * emms$error
    } else {
      emm_between <- interaction(emms[between_vars], sep = ".")
      emms$error <- ses[emm_between]
      emms$lower <- emms$y - qt(1-(1-error_level)/2, sizes[emm_between] - 1) * emms$error
      emms$upper <- emms$y + qt(1-(1-error_level)/2, sizes[emm_between] - 1) * emms$error
    }
  } else if (error == "none") {
    emms$error <- NA_real_
    emms$lower <- NA_real_
    emms$upper <- NA_real_
    plot_error <- FALSE  
  }
  
  if (!error_ci) {
    emms$lower <- emms$y - emms$error
    emms$upper <- emms$y + emms$error
  }
  
  if (length(trace) > 0) {
    attr(emms, "trace") <- paste(trace, sep = "\n")
    emms$trace <- interaction(emms[trace], sep = "\n")
    data$trace <- interaction(data[trace], sep = "\n")
    
    if (return == "data") {
      return(list(means = emms, data = data))
    } else if (return == "plot") {
      return(interaction_plot(means = emms, 
                              data = data,
                              error_plot = plot_error,
                              error_arg = error_arg, 
                              dodge = dodge, 
                              data_plot = data_plot,
                              data_geom = data_geom,
                              data_alpha = data_alpha,
                              data_arg = data_arg,
                              point_arg = point_arg,
                              line_arg = line_arg,
                              mapping = mapping
      ))
    }
  } else {
    if (return == "data") {
      return(list(means = emms, data = data))
    } else if (return == "plot") {
      return(oneway_plot(means = emms, 
                         data = data,
                         error_plot = plot_error,
                         error_arg = error_arg, 
                         data_plot = data_plot,
                         data_geom = data_geom,
                         data_alpha = data_alpha,
                         data_arg = data_arg,
                         point_arg = point_arg,
                         mapping = mapping
      ))
    }
  }
}





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
                             dodge = 0.2, 
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
  
  plot_out <- plot_out + 
    do.call(what = ggplot2::geom_point, 
            args = c(
              position = list(
                ggplot2::position_dodge(width = dodge)
              ),
              point_arg
            )) +
    do.call(what = ggplot2::geom_line, 
            args = c(
              position = list(
                ggplot2::position_dodge(width = dodge)
              ),
              line_arg
            ))

  if (error_plot) {
    plot_out <- plot_out + 
      do.call(what = ggplot2::geom_errorbar, 
              args = c(
                mapping = list(ggplot2::aes_string(
                  ymin = col_lower,
                  ymax = col_upper)),
                position = list(ggplot2::position_dodge(width = dodge)),
                error_arg
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
  if (!is.null(attr(means, "trace"))) {
    tmp_list <- rep(list(ggplot2::guide_legend(title = attr(means, "trace"))), 
                    length(mapping))
    names(tmp_list) <- mapping
    plot_out <- plot_out + 
      do.call(what = ggplot2::guides, 
              args = tmp_list)
  }
  
  return(plot_out)
  
}


##if(getRversion() >= "2.15.1")  utils::globalVariables(c("error", "y"))
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
  
  tmp_list <- as.list(rep(col_x, length(mapping)))
  names(tmp_list) <- mapping
  
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
                mapping = list(ggplot2::aes_string(
                  ymin = col_lower,
                  ymax = col_upper)),
                error_arg
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
  
  return(plot_out)
}
