#' Set/get global afex options
#' 
#' Global afex options are used, for example, by \code{\link{aov_car}} (et al.) and \code{\link{mixed}}. But can be changed in each functions directly using an argument (which has precedence over the global options).
#' 
#' @param ... One of four: (1) nothing, then returns all options as a list; (2) a name of an option element, then returns its' value; (3) a name-value pair which sets the corresponding option to the new value (and returns nothing), (4) a list with option-value pairs which sets all the corresponding arguments. The example show all possible cases.
#' 
#' @details The following arguments are currently set:
#' \itemize{
#' \item \code{check_contrasts} should contrasts be checked and changed to sum-to-zero contrasts? Default is \code{TRUE}.
#' \item \code{type} type of sums-of-squares to be used for testing effects, default is 3 which reports Type 3 tests.
#' \item \code{method_mixed}: Method used to obtain p-values in \code{\link{mixed}}, default is \code{"KR"} (which will change to \code{"LRT"} soon). (\code{mixed()} only)
#' \item \code{return_aov}: Return value of the ANOVA functions (see \code{\link{aov_car}}), default is \code{"nice"}. 
#' \item \code{es_aov}: Effect size reported for ANOVAs (see \code{\link{aov_car}}), default is \code{"ges"} (generalized eta-squared).
#' \item \code{correction_aov}: Correction used for within-subjects factors with more than two levels for ANOVAs  (see \code{\link{aov_car}} or \code{\link{nice}}), default is \code{"GG"} (Greenhouse-Geisser correction). (ANOVA functions only)
#' \item \code{factorize}: Should between subject factors be factorized (with note) before running the analysis? Default is \code{TRUE}. (ANOVA functions only)
#' \item \code{sig_symbols}: Default significant symbols used for ANOVA and \code{mixed} printing. Default is\code{c(" +", " *", " **", " ***")}.
#' \item \code{lmer_function}: Which \code{lmer} function should \code{mixed} or \code{lmer_alt} use. The default is \code{"lmerTest"} which uses \code{\link[lmerTest]{lmer}}, \code{"lme4"} is also possible which uses \code{\link[lme4]{lmer}}. There should be no difference between the two. The latter could be minimally faster, but does not allow to use \code{lmerTest::anova()}.
#' }
#' 
#' @note All options are saved in the global R \code{\link{options}} with prefix \code{afex.}
#' 
#' @return depends on input, see above.
#'
#' @example examples/examples.helpers.R
#' 
#' @export

# afex_options <- function(...)
# {
#   dots <- list(...)
#   if (length(dots) == 0) return(ls.str(envir = .afexEnv))
#   else {
#     if (!is.null(names(dots))) {
#       if (length(dots) > 1) stop("afex_options can only return a single element.")
#       for (i in seq_along(dots)) {
#         assign(names(dots)[i], dots[[i]], envir = .afexEnv)
#       }
#     } else return(get(dots[[1]], envir = .afexEnv))
#   }
# }

afex_options <- function(...) {
  dots <- list(...)
  #browser()
  if (length(dots) == 0) {  # branch to get all afex options
    op <- options()
    afex_op <- op[str_detect(names(op), "^afex.")]
    names(afex_op) <- str_replace(names(afex_op), "^afex.", "")
    return(afex_op)
  } else if (is.list(dots[[1]])) {  # set several afex options as a list:
    newop <- dots[[1]]
    names(newop) <- str_c("afex.", names(newop))
    options(newop)
  } else if (!is.null(names(dots))) {
    newop <- dots
    names(newop) <- str_c("afex.", names(newop))
    options(newop)
  } else if (is.null(names(dots))) {  # get a single afex options
    if (length(dots) > 1) stop("afex_options() can only return the value of a single option.", call. = FALSE)
    return(getOption(str_c("afex.", unlist(dots))))
  } else {
    warning("Unsopported command to afex_options(), nothing done.", call. = FALSE)
  }
}

