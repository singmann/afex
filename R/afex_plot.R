#' m-way Plot with Error Bars and Raw Data
#' 
#' @description Plots results from factorial experiments. Estimated marginal 
#'   means and error bars are plotted in the foreground, raw data is plotted in 
#'   the background. Error bars can be based on different standard errors (e.g.,
#'   model-based, within-subjects, between-subjects). Functions described here
#'   return a \pkg{ggplot2} plot object, thus allowing further customization of
#'   the plot.
#'   
#'   \code{afex_plot} is the user friendly function that does data preparation
#'   and plotting. It also allows to only return the prepared data (\code{return
#'   = "data"}).
#'   
#'   \code{interaction_plot} does the plotting when a \code{trace} factor is
#'   present. \code{oneway_plot} does the plotting when a \code{trace} factor is
#'   absent.
#'   
#' @param object \code{afex_aov}, \code{mixed}, or \code{merMod} object.
#' @param x A \code{character} vector or one-sided \code{formula} specifying the
#'   factor names of the predictors displayed on the x-axis. \code{mapping}
#'   specifies further mappings for these factors if \code{trace} is missing.
#' @param trace An optional \code{character} vector or one-sided \code{formula}
#'   specifying the factor names of the predictors connected by the same line.
#'   \code{mapping} specifies further mappings for these factors.
#' @param panel An optional \code{character} vector or one-sided \code{formula} 
#'   specifying the factor names of the predictors shown in different panels.
#' @param mapping A \code{character} vector specifying which aesthetic mappings 
#'   should be applied to either the \code{trace} factors (if \code{trace} is 
#'   specified) or the \code{x} factors. Useful options are any combination of 
#'   \code{"shape"}, \code{"color"}, \code{"linetype"}, or also \code{"fill"} 
#'   (see examples). The default (i.e., missing) uses \code{c("shape", 
#'   "linetype")} if \code{trace} is specified and \code{""} otherwise (i.e., no
#'   additional aesthetic).
#' @param error A scalar \code{character} vector specifying on which standard 
#'   error the error bars should be based. Default is \code{"model"}, which
#'   plots model-based standard errors. Further options are: \code{"none"} (or 
#'   \code{NULL}), \code{"mean"}, \code{"within"} (or \code{"CMO"}), and 
#'   \code{"between"}. See details.
#' @param id An optional \code{character} vector specifying over which variables
#'   the raw data should be aggregated. Only relevant for \code{mixed}, 
#'   \code{merMod}, and \code{default} method. The default (missing) uses all 
#'   random effects grouping factors (for \code{mixed} and \code{merMod} method)
#'   or assumes all data points are independent. This can lead to many data
#'   points. \code{error = "within"} or \code{error = "between"} require that
#'   \code{id} is of length 1. See examples.
#' @param dv An optional scalar \code{character} vector giving the name of the
#'   column containing the dependent variable for the \code{afex_plot.default}
#'   method. If missing, the function attempts to take it from the \code{call}
#'   slot of \code{object}. This is also used as y-axis label.
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
#'   The default (missing) uses \code{\link[ggplot2]{geom_point}} if 
#'   \code{trace} is specified, otherwise 
#'   \code{\link[ggbeeswarm]{geom_beeswarm}}. See examples fo further options.
#' @param data_alpha numeric \code{alpha} value between 0 and 1 passed to 
#'   \code{data_geom}. Default is \code{0.5} which correspond to semitransparent
#'   data points in the background such that overlapping data points are plotted
#'   darker.
#' @param data_arg A \code{list} of further arguments passed to 
#'   \code{data_geom}. Default is \code{list(color = "darkgrey")}, which plots
#'   points in the background in grey.
#' @param point_arg,line_arg A \code{list} of further arguments passed to 
#'   \code{\link[ggplot2]{geom_point}} or \code{\link[ggplot2]{geom_line}} which
#'   draw the points and lines in the foreground. Default is \code{list()}.
#'   \code{line_arg} is only used if \code{trace} is specified.
#' @param emmeans_arg A \code{list} of further arguments passed to 
#'   \code{\link[emmeans]{emmeans}}. Of particular importance for ANOVAs is 
#'   \code{model}, see \code{\link{afex_aov-methods}}.
#' @param dodge Numerical amount of dodging of factor-levels on x-axis. Default 
#'   is \code{0.5}.
#' @param return A scalar \code{character} specifying what should be returned. 
#'   The default \code{"plot"} returns the \pkg{ggplot2} plot. The other option 
#'   \code{"data"} returns a list with two \code{data.frame}s containing the 
#'   data used for plotting: \code{means} contains the means and standard errors
#'   for the foreground, \code{data} contains the raw data in the background.
#' @param factor_levels A \code{list} of new factor levels that should be used in 
#'   the plot. The name of each list entry needs to correspond to one of the 
#'   factors in the plot. 
#' @param legend_title A scalar \code{character} vector with a new title for the
#'   legend.
#' @param data For the \code{afex_plot.default} method, an optional
#'   \code{data.frame} containing the raw data used for fitting the model and
#'   which will be used as basis for the data points in the background. If
#'   missing, it will be attempted to obtain it from the model via
#'   \code{\link[emmeans]{recover_data}}. For the plotting functions, a
#'   \code{data.frame} with the data that has to be passed and contains the
#'   background data points.
#' @param within_vars,between_vars For the \code{afex_plot.default} method, an
#'   optional \code{character} vector specifying which variables should be
#'   treated as within-subjects (or repeated-measures) factors and which as
#'   between-subjects (or independen-sampels) factors. If one of the two
#'   arguments is given, all other factors are assumed to fall into the other
#'   category.
#' @param means \code{data.frame}s used for plotting of the plotting
#'   functions.
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
#'   Schenker & Gentleman, 2005). For example, in a fully between-subjects design
#'   in which the error bars depict 95\% confidence intervals and groups are of 
#'   approximately equal size and have equal variance, even error bars that 
#'   overlap by as much as 50\% still correspond to \emph{p} < .05. Error bars 
#'   that are just touching roughly correspond to \emph{p} = .01.
#'   
#'   In the case of designs involving repeated-measures factors the usual
#'   confidence intervals or standard errors (i.e., model-based confidence
#'   intervals or intervals based on the standard error of the mean) cannot be
#'   used to gauge significant differences as this requires knowledge about the
#'   correlation between measures. One popular alternative in the psychological
#'   literature are intervals based on within-subjects standard
#'   errors/confidence intervals (e.g., Cousineau & O'Brien, 2014). These
#'   attempt to control for the correlation across individuals and thereby allow
#'   judging differences between repeated-measures condition. As a downside,
#'   when using within-subjects intervals no comparisons across between-subjects
#'   conditions or with respect to a fixed-value are possible anymore.
#'   
#'   In the case of a mixed-design, no single type of error bar is possible that
#'   allows comparison across all conditions. Likewise, for mixed models
#'   involving multiple \emph{crossed} random effects, no single set of error
#'   bars (or even data aggregation) adequately represent the true varibility in
#'   the data and adequately allows for "inference by eye". Therefore, special
#'   care is necessary in such cases. One possiblity is to avoid error bars
#'   altogether and plot only the raw data in the background (with \code{error =
#'   "none"}). The raw data in the background still provides a visual impression
#'   of the variability in the data and the precision of the mean estimate, but
#'   does not as easily suggest an incorrect inferences. Another possibility is
#'   to use the model-based standard error and note in the figure caption that
#'   it does not permit comparisons across repeated-measures factors.
#'   
#'   The following "rules of eye" (Cumming and Finch, 2005) hold, when permitted
#'   by design (i.e., within-subjects bars for within-subjects comparisons;
#'   other variants for between-subjects comparisons), and groups are
#'   approximately equal in size and variance. Note that for more complex
#'   designs ususally analyzed with mixed models, such as designs involving
#'   complicated dependencies across data points, these rules of thumbs may be
#'   highly misleading.
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
#'   degrees of freedom based on the cell or group size. For ANOVA models,
#'   \code{afex_plot} attempts to warn in case the chosen approach is misleading
#'   given the design (e.g., model-based error bars for purely
#'   within-subjects plots). For \code{mixed} models, no such warnings are
#'   produced, but users should be aware that all options beside \code{"model"}
#'   are not actually appropriate and have only heuristic value. But then again,
#'   \code{"model"} based error bars do not permit comparisons for factors
#'   varying within one of the random-effects grouping factors (i.e., factors
#'   for which random-slopes should be estimated).
#'   \describe{
#'     \item{\code{"model"}}{Uses model-based CIs and SEs. For ANOVAs, the
#'     variant based on the \code{lm} or \code{mlm} model (i.e.,
#'     \code{emmeans_arg = list(model = "multivariate")}) seems generally
#'     preferrable.}
#'     \item{\code{"mean"}}{Calculates the standard error of the mean for
#'     each cell ignoring any repeated-measures factors.}
#'     \item{\code{"within"} or \code{"CMO"}}{Calculates within-subjects SEs
#'     using the Cosineau-Morey-O'Brien (Cousineau & O'Brien, 2014) method. This
#'     method is based on a double normalization of the data. SEs and CIs are
#'     then calculated independently for each cell (i.e., if the desired output
#'     contains between-subjects factors, SEs are calculated for each cell
#'     including the between-subjects factors).}
#'     \item{\code{"between"}}{First aggregates the data per participant and 
#'     then calculates the SEs for each between-subjects condition. Results in 
#'     one SE and \emph{t}-quantile for all conditions in purely within-subjects
#'     designs.}
#'     \item{\code{"none"} or \code{NULL}}{Suppresses calculation of SEs and
#'     plots no error bars.}
#'   }
#'   For \code{mixed} models, the within-subjects/repeated-measures factors are
#'   relative to the chosen \code{id} effects grouping factor. They are
#'   automatically detected based on the random-slopes of the random-effects
#'   grouping factor in \code{id}. All other factors are treated as
#'   independent-samples or between-subjects factors.
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
                               dodge = 0.5,
                               return = "plot",
                               factor_levels = list(),
                               legend_title,
                               ...) {
  
  return <- match.arg(return, c("plot", "data"))
  error <- match.arg(error, c("none", 
                              "model", 
                              "mean", 
                              "within", "CMO",
                              "between"))
  dots <- list(...)
  if (length(dots) > 0) {
    warning("Additional arguments ignored: ", 
            paste(names(dots), collapse = ", "), call. = FALSE)
  }
  
  x <- get_plot_var(x)
  trace <- get_plot_var(trace)
  panel <- get_plot_var(panel)
  all_vars <- c(x, trace, panel)
  
  emms <- get_emms(object = object, 
                   x = x,
                   trace = trace,
                   panel = panel,
                   emmeans_arg = emmeans_arg, 
                   factor_levels = factor_levels,
                   level = error_level)
  
  ## prepare raw (i.e., participant by cell) data
  data <- prep_data(object$data$long, 
                    x = x,
                    trace = trace,
                    panel = panel,
                    factor_levels = factor_levels,
                    dv_col = attr(object, "dv"),
                    id = attr(object, "id"))
  
  ### prepare variables for SE/CI calculation
  within_vars <- all_vars[all_vars %in% names(attr(object, "within"))]
  between_vars <- all_vars[all_vars %in% names(attr(object, "between"))]
  
  ### check if error bars are consistent with panel(s) and warn otherwise
  if (error %in% c("model", "mean", "between") && 
      all(c(x, trace) %in% within_vars)) {
    warning("Panel(s) show within-subjects factors, ", 
            "but not within-subjects error bars.\n", 
            'For within-subjects error bars use: error = "within"', 
            call. = FALSE)
  } else if (error %in% c("within", "CMO") && 
             all(c(x, trace) %in% between_vars)) {
    warning("Panel(s) show between-subjects factors, ", 
            "but within-subjects error bars.\n", 
            'For between-subjects error bars use e.g.,: ',
            'error = "model" or error = "mean"',
            call. = FALSE)
  } else if (any(between_vars %in% c(x, trace)) && 
             any(within_vars %in% c(x, trace)) && 
             error != "none") {
    warning("Panel(s) show a mixed within-between-design.\n",
            "Error bars do not allow comparisons across all means.\n", 
            'Suppress error bars with: error = "none"',
            call. = FALSE)
  }
  
  tmp <- get_data_based_cis(emms = emms, 
                            data = data, 
                            error = error, 
                            id = attr(object, "id"), ## colname holding the id/grouping variable 
                            all_vars = all_vars,
                            within_vars = within_vars, 
                            between_vars = between_vars, 
                            error_level = error_level, 
                            error_ci = error_ci)
  emms <- tmp$emms
  error_plot <- tmp$error_plot
  
  return(afex_plot_internal(x = x,
                            trace = trace,
                            panel = panel,
                            means = emms, 
                            data = data,
                            error_plot = error_plot,
                            error_arg = error_arg, 
                            dodge = dodge, 
                            data_plot = data_plot,
                            data_geom = data_geom,
                            data_alpha = data_alpha,
                            data_arg = data_arg,
                            point_arg = point_arg,
                            line_arg = line_arg,
                            mapping = mapping,
                            legend_title =  legend_title,
                            return = return
  ))
}



# @method afex_plot afex_aov
#' @rdname afex_plot
#' @export
afex_plot.mixed <- function(object, 
                            x,
                            trace,
                            panel,
                            mapping,
                            id,
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
                            dodge = 0.5,
                            return = "plot",
                            factor_levels = list(),
                            legend_title,
                            ...) {
  
  return <- match.arg(return, c("plot", "data"))
  error <- match.arg(error, c("none", 
                              "model", 
                              "mean", 
                              "within", "CMO",
                              "between"))
  dots <- list(...)
  if (length(dots) > 0) {
    warning("Additional arguments ignored: ", 
            paste(names(dots), collapse = ", "), call. = FALSE)
  }
  
  x <- get_plot_var(x)
  trace <- get_plot_var(trace)
  panel <- get_plot_var(panel)
  all_vars <- c(x, trace, panel)
  
  if (missing(id)) {
    id <- unique(names(lme4::ranef(object$full_model)))
    message("Aggregating data over: ", paste(id, collapse = ", "))
  }
  ## prepare raw (i.e., participant by cell) data
  data <- prep_data(object$data, 
                    x = x,
                    trace = trace,
                    panel = panel,
                    factor_levels = factor_levels,
                    dv_col = deparse(object$full_model@call[["formula"]][[2]]),
                    id = id)
  data$afex_id <- interaction(data[id], sep = ".")
  
  if (!(error %in% c("none" ,"model", "mean")) & 
      (length(id) > 1)) {
    stop("When aggregating over multiple random effects,\n",
         '       error has to be in: c("model", "mean", "none")',
         call. = FALSE)
  } 
  
  emms <- get_emms(object = object, 
                   x = x,
                   trace = trace,
                   panel = panel,
                   emmeans_arg = emmeans_arg, 
                   factor_levels = factor_levels,
                   level = error_level)
  
  if (length(id) == 1) {
    all_within <- lapply(lme4::findbars(object$call), all.vars)
    all_within <- 
      unique(unlist(
        all_within[vapply(all_within, function(x) id %in% x, NA)]
      ))
    all_within <- all_within[all_within != id]
    within_vars <- all_vars[all_vars %in% all_within]
    between_vars <- all_vars[!(all_vars %in% within_vars)]
  }
  
  
  ### prepare variables for SE/CI calculation
  tmp <- get_data_based_cis(emms = emms, 
                            data = data, 
                            error = error, 
                            id = "afex_id", ## colname holding the id/grouping variable 
                            all_vars = all_vars,
                            within_vars = within_vars, 
                            between_vars = between_vars, 
                            error_level = error_level, 
                            error_ci = error_ci)
  emms <- tmp$emms
  error_plot <- tmp$error_plot
  
  return(afex_plot_internal(x = x,
                            trace = trace,
                            panel = panel,
                            means = emms, 
                            data = data,
                            error_plot = error_plot,
                            error_arg = error_arg, 
                            dodge = dodge, 
                            data_plot = data_plot,
                            data_geom = data_geom,
                            data_alpha = data_alpha,
                            data_arg = data_arg,
                            point_arg = point_arg,
                            line_arg = line_arg,
                            mapping = mapping,
                            legend_title =  legend_title,
                            return = return
  ))
}



#' @rdname afex_plot
#' @export
afex_plot.merMod <- function(object, 
                            x,
                            trace,
                            panel,
                            mapping,
                            id,
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
                            dodge = 0.5,
                            return = "plot",
                            factor_levels = list(),
                            legend_title,
                            ...) {
  
  return <- match.arg(return, c("plot", "data"))
  error <- match.arg(error, c("none", 
                              "model", 
                              "mean", 
                              "within", "CMO",
                              "between"))
  dots <- list(...)
  if (length(dots) > 0) {
    warning("Additional arguments ignored: ", 
            paste(names(dots), collapse = ", "), call. = FALSE)
  }
  
  x <- get_plot_var(x)
  trace <- get_plot_var(trace)
  panel <- get_plot_var(panel)
  all_vars <- c(x, trace, panel)
  
  
  if (missing(id)) {
    id <- unique(names(lme4::ranef(object)))
    message("Aggregating data over: ", paste(id, collapse = ", "))
  }
  ## prepare raw (i.e., participant by cell) data
  data <- prep_data(
    data = emmeans::recover_data(
      object = object, 
      trms = terms(object, fixed.only = FALSE)
      ), 
    x = x,
    trace = trace,
    panel = panel,
    factor_levels = factor_levels,
    dv_col = deparse(object@call[["formula"]][[2]]),
    id = id)
  data$afex_id <- interaction(data[id], sep = ".")
  
  if (!(error %in% c("none" ,"model", "mean")) & 
      (length(id) > 1)) {
    stop("When aggregating over multiple random effects,\n",
         '       error has to be in: c("model", "mean", "none")',
         call. = FALSE)
  } 
  
  emms <- get_emms(object = object, 
                   x = x,
                   trace = trace,
                   panel = panel,
                   emmeans_arg = emmeans_arg, 
                   factor_levels = factor_levels,
                   level = error_level)
  
  if (length(id) == 1) {
    all_within <- lapply(lme4::findbars(object@call), all.vars)
    all_within <- 
      unique(unlist(
        all_within[vapply(all_within, function(x) id %in% x, NA)]
      ))
    all_within <- all_within[all_within != id]
    within_vars <- all_vars[all_vars %in% all_within]
    between_vars <- all_vars[!(all_vars %in% within_vars)]
  }
  
  
  ### prepare variables for SE/CI calculation
  tmp <- get_data_based_cis(emms = emms, 
                            data = data, 
                            error = error, 
                            id = "afex_id", ## colname holding the id/grouping variable 
                            all_vars = all_vars,
                            within_vars = within_vars, 
                            between_vars = between_vars, 
                            error_level = error_level, 
                            error_ci = error_ci)
  emms <- tmp$emms
  error_plot <- tmp$error_plot
  
  return(afex_plot_internal(x = x,
                            trace = trace,
                            panel = panel,
                            means = emms, 
                            data = data,
                            error_plot = error_plot,
                            error_arg = error_arg, 
                            dodge = dodge, 
                            data_plot = data_plot,
                            data_geom = data_geom,
                            data_alpha = data_alpha,
                            data_arg = data_arg,
                            point_arg = point_arg,
                            line_arg = line_arg,
                            mapping = mapping,
                            legend_title =  legend_title,
                            return = return
  ))
}

#' @rdname afex_plot
#' @export
afex_plot.default <- function(object, 
                              x,
                              trace,
                              panel,
                              mapping,
                              id,
                              dv,
                              data,
                              within_vars,
                              between_vars,
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
                              dodge = 0.5,
                              return = "plot",
                              factor_levels = list(),
                              legend_title,
                              ...) {
  
  return <- match.arg(return, c("plot", "data"))
  error <- match.arg(error, c("none", 
                              "model", 
                              "mean", 
                              "within", "CMO",
                              "between"))
  dots <- list(...)
  if (length(dots) > 0) {
    warning("Additional arguments ignored: ", 
            paste(names(dots), collapse = ", "), call. = FALSE)
  }
  
  x <- get_plot_var(x)
  trace <- get_plot_var(trace)
  panel <- get_plot_var(panel)
  all_vars <- c(x, trace, panel)
  
  if (missing(dv)) {
    formula_name <- names(object$call)[2]
    message("dv column detected: ", deparse(object$call[[formula_name]][[2]]))
    dv <- deparse(object$call[[formula_name]][[2]])
  }
  ## prepare raw (i.e., participant by cell) data if missing
  if (missing(data)) {
    data <- emmeans::recover_data(
      object = object, 
      trms = terms(object)
    )
  }
  if (missing(id)) {
    message("No id column passed. ", 
            "Assuming all rows are independent samples.")
    data$id <- factor(seq_len(nrow(data)))
    id <- "id"
  }
  data <- prep_data(
    data = data, 
    x = x,
    trace = trace,
    panel = panel,
    factor_levels = factor_levels,
    dv_col = dv,
    id = id)
  data$afex_id <- interaction(data[id], sep = ".")
  
  if (!(error %in% c("none" ,"model", "mean")) & 
      (length(id) > 1)) {
    stop("When aggregating over multiple ids,\n",
         '       error has to be in: c("model", "mean", "none")',
         call. = FALSE)
  } 
  
  emms <- get_emms(object = object, 
                   x = x,
                   trace = trace,
                   panel = panel,
                   emmeans_arg = emmeans_arg, 
                   factor_levels = factor_levels,
                   level = error_level)
  attr(emms, "dv") <- dv
  
  if (missing(within_vars) & !missing(between_vars)) {
    within_vars <- all_vars[!(all_vars %in% within_vars)]
  }
  if (!missing(within_vars) & missing(between_vars)) {
    between_vars <- all_vars[!(all_vars %in% between_vars)]
  }
  
  ### prepare variables for SE/CI calculation
  tmp <- get_data_based_cis(emms = emms, 
                            data = data, 
                            error = error, 
                            id = "afex_id", ## colname holding the id/grouping variable 
                            all_vars = all_vars,
                            within_vars = within_vars, 
                            between_vars = between_vars, 
                            error_level = error_level, 
                            error_ci = error_ci)
  emms <- tmp$emms
  error_plot <- tmp$error_plot
  
  return(afex_plot_internal(x = x,
                            trace = trace,
                            panel = panel,
                            means = emms, 
                            data = data,
                            error_plot = error_plot,
                            error_arg = error_arg, 
                            dodge = dodge, 
                            data_plot = data_plot,
                            data_geom = data_geom,
                            data_alpha = data_alpha,
                            data_arg = data_arg,
                            point_arg = point_arg,
                            line_arg = line_arg,
                            mapping = mapping,
                            legend_title =  legend_title,
                            return = return
  ))
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
  if (!missing(legend_title)) {
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
  if (!missing(legend_title)) {
    tmp_list <- rep(list(ggplot2::guide_legend(title = legend_title)), 
                    length(mapping))
    names(tmp_list) <- mapping
    plot_out <- plot_out + 
      do.call(what = ggplot2::guides, 
              args = tmp_list)
  }
  
  return(plot_out)
}
