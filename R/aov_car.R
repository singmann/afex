#' Convenient ANOVA estimation for factorial designs
#'
#' These functions allow convenient specification of any type of ANOVAs (i.e., purely within-subjects ANOVAs, purely between-subjects ANOVAs, and mixed between-within or split-plot ANOVAs) for data in the \strong{long} format (i.e., one observation per row). If the data has more than one observation per individual and cell of the design (e.g., multiple responses per condition), the data will by automatically aggregated. The default settings reproduce results from commercial statistical packages such as SPSS or SAS. \code{aov_ez} is called specifying the factors as character vectors, \code{aov_car} is called using a formula similar to \code{\link{aov}} specifying an error strata for the within-subject factor(s), and \code{aov_4} is called with a \pkg{lme4}-like formula (all ANOVA functions return identical results). The returned object contains the ANOVA also fitted via base R's \code{\link{aov}} which can be passed to e.g., \pkg{lsmeans} for further analysis (e.g., follow-up tests, contrasts, plotting, etc.). These functions employ \code{\link[car]{Anova}} (from the \pkg{car} package) to provide test of effects avoiding the somewhat unhandy format of \code{car::Anova}. 
#'
#' @usage 
#' aov_ez(id, dv, data, between = NULL, within = NULL, covariate = NULL, 
#'      observed = NULL, fun.aggregate = NULL, type = afex_options("type"), 
#'      factorize = afex_options("factorize"), 
#'      check.contrasts = afex_options("check.contrasts"), 
#'      return = afex_options("return_aov"), 
#'      anova_table = list(), ..., print.formula = FALSE)
#'      
#' aov_car(formula, data, fun.aggregate = NULL, type = afex_options("type"), 
#'      factorize = afex_options("factorize"), 
#'      check.contrasts = afex_options("check.contrasts"), 
#'      return = afex_options("return_aov"), observed = NULL, 
#'      anova_table = list(), ...)
#'      
#' aov_4(formula, data, observed = NULL, fun.aggregate = NULL, type = afex_options("type"),
#'      factorize = afex_options("factorize"), 
#'      check.contrasts = afex_options("check.contrasts"),
#'      return = afex_options("return_aov"), 
#'      anova_table = list(), ..., print.formula = FALSE)
#' 
#'
#' @param id \code{character} vector (of length 1) indicating the subject identifier column in \code{data}.
#' @param dv \code{character} vector (of length 1) indicating the column containing the \strong{dependent variable} in \code{data}.
#' @param between \code{character} vector indicating the \strong{between}-subject(s) factor(s)/column(s) in \code{data}. Default is \code{NULL} indicating no between-subjects factors.
#' @param within \code{character} vector indicating the \strong{within}-subject(s)(or repeated-measures) factor(s)/column(s) in \code{data}.  Default is \code{NULL} indicating no within-subjects factors.
#' @param covariate \code{character} vector indicating the between-subject(s) covariate(s) (i.e., column(s)) in \code{data}. Default is \code{NULL} indicating no covariates.
#' @param observed \code{character} vector indicating which of the variables are observed (i.e, measured) as compared to experimentally manipulated. The default effect size reported (generalized eta-squared) requires correct specification of the obsered (in contrast to manipulated) variables. 
#' @param formula A formula specifying the ANOVA model similar to \code{\link{aov}} (for \code{aov_car} or similar to \code{lme4:lmer} for \code{aov_4}). Should include an error term (i.e., \code{Error(id/...)} for \code{aov_car} or \code{(...|id)} for \code{aov_4}). Note that the within-subject factors do not need to be outside the Error term (this contrasts with \code{aov}). See Details.
#' @param data A \code{data.frame} containing the data. Mandatory.
#' @param fun.aggregate The function for aggregating the data before running the ANOVA if there is more than one observation per individual and cell of the design. The default \code{NULL} issues a warning if aggregation is necessary and uses \code{\link{mean}}. Pass \code{mean} directly to avoid the warning.
#' @param type The type of sums of squares for the ANOVA. The default is given by \code{afex_options("type")}, which is \strong{initially set to 3}. Passed to \code{\link[car]{Anova}}. Possible values are \code{"II"}, \code{"III"}, \code{2}, or \code{3}.
#' @param factorize logical. Should between subject factors be factorized (with note) before running the analysis. he default is given by \code{afex_options("factorize")}, which is initially \code{TRUE}. If one wants to run an ANCOVA, needs to be set to \code{FALSE} (in which case centering on 0 is checked on numeric variables).
#' @param check.contrasts \code{logical}. Should contrasts for between-subject factors be checked and (if necessary) changed to be \code{"contr.sum"}. See details. The default is given by \code{afex_options("check.contrasts")}, which is initially \code{TRUE}.
#' @param print.formula \code{aov_ez} and \code{aov_4} are wrapper for \code{aov_car}. This boolean argument indicates whether the formula in the call to \code{car.aov} should be printed. 
#' @param return What should be returned? The default is given by \code{afex_options("return_aov")}, which is initially \code{"afex_aov"}, returning an S3 object of class \code{afex_aov} for which various \link[=afex_aov-methods]{methods} exist (see there and below for more details). To avoid the (potentially costly) computation via \code{aov} set \code{return} to \code{"nice"} in which case only the nice ANOVA table is returned (produced by \code{\link{nice}}, this was the previous default return value). Other values are currently still supported for backward compatibility.
# Possible values are \code{c("Anova", "lm", "data", "nice", "full", "all", "univariate", "marginal", "aov")} (possibly abbreviated). 
#' @param anova_table \code{list} of further arguments passed to function producing the ANOVA table.  Arguments such as \code{es} (effect size) or \code{correction}  are passed to either \code{anova.afex_aov} or \code{nice}. Note that those settings can also be changed once an object of class \code{afex_aov} is created by invoking the \code{anova} method directly.
#' @param ... Further arguments passed to \code{fun.aggregate}.
#'
#' @return \code{aov_car}, \code{aov_4}, and \code{aov_ez} are wrappers for \code{\link[car]{Anova}} and \code{\link{aov}}, the return value is dependent on the \code{return} argument. Per default, an S3 object of class \code{"afex_aov"} is returned containing the following slots: 
#' 
#' \describe{
#'   \item{\code{"anova_table"}}{An ANOVA table of class \code{c("anova", "data.frame")}.}
#'   \item{\code{"aov"}}{\code{aov} object returned from \code{\link{aov}} (should not be used to evaluate significance of effects, but can be passed to \code{lsmeans} for post-hoc tests).}
#'   \item{\code{"Anova"}}{object returned from \code{\link[car]{Anova}}, an object of class \code{"Anova.mlm"} (if within-subjects factors are present) or of class \code{c("anova", "data.frame")}.}
#'   \item{\code{"lm"}}{the object fitted with \code{lm} and passed to \code{Anova} (i.e., an object of class \code{"lm"} or \code{"mlm"}). Also returned if \code{return = "lm"}.}
#'   \item{\code{"data"}}{a list containing: (1) \code{long} (the possibly aggregated data in long format used for \code{aov}), \code{wide} (the data used to fit the \code{lm} object), and \code{idata} (if within-subject factors are present, the \code{idata} argument passed to \code{car::Anova}). Also returned if \code{return = "data"}.}
#' }
#' In addition, the object has the following attributes: \code{"dv"}, \code{"id"}, \code{"within"}, \code{"between"}, and \code{"type"}.
#' 
#' The \link[=afex_aov-methods]{print} method for \code{afex_aov} objects (invisibly) returns (and prints) the same as if \code{return} is \code{"nice"}: a nice ANOVA table (produced by \code{\link{nice}}) with the following columns: \code{Effect}, \code{df}, \code{MSE} (mean-squared errors), \code{F} (potentially with significant symbols), \code{ges} (generalized eta-squared), \code{p}.
#' 
#' @details 
#' 
#' \subsection{Details of ANOVA Specification}{ 
#' \code{aov_ez} will concatenate all between-subject factors using \code{*} (i.e., producing all main effects and interactions) and all covariates by \code{+} (i.e., adding only the main effects to the existing between-subject factors). The within-subject factors do fully interact with all between-subject factors and covariates. This is essentially identical to the behavior of SPSS's \code{glm} function.
#' 
#' The \code{formula}s for \code{aov_car} or \code{aov_4} must contain a single \code{Error} term specifying the \code{ID} column and potential within-subject factors (you can use \code{\link{mixed}} for running mixed-effects models with multiple error terms). Factors outside the \code{Error} term are treated as between-subject factors (the within-subject factors specified in the \code{Error} term are ignored outside the \code{Error} term; in other words, it is not necessary to specify them outside the \code{Error} term, see Examples).\cr
#' Suppressing the intercept (i.e, via \code{0 +} or \code{- 1}) is ignored. Specific specifications of effects (e.g., excluding terms with \code{-} or using \code{^}) could be okay but is not tested. Using the \code{\link{I}} or \code{\link{poly}} function within the formula is not tested and not supported!
#'
#' To run an ANCOVA you need to set \code{factorize = FALSE} and make sure that all variables have the correct type (i.e., factors are factors and numeric variables are numeric and centered). 
#'
#' Note that the default behavior is to include calculation of the effect size generalized eta-squared for which \strong{all non-manipluated (i.e., observed)} variables need to be specified via the \code{observed} argument to obtain correct results. When changing the effect size to \code{"pes"} (partial eta-squared) or \code{"none"} via \code{anova_table} this becomes unnecessary.
#' 
#' If \code{check.contrasts = TRUE}, contrasts will be set to \code{"contr.sum"} for all between-subject factors if default contrasts are not equal to \code{"contr.sum"} or \code{attrib(factor, "contrasts") != "contr.sum"}. (within-subject factors are hard-coded \code{"contr.sum"}.)
#' }
#' 
#' \subsection{Statistical Issues}{
#' \strong{Type 3 sums of squares are default in \pkg{afex}.} While some authors argue that so-called type 3 sums of squares are dangerous and/or problematic (most notably Venables, 2000), they are the default in many commercial statistical application such as SPSS or SAS. Furthermore, statisticians with an applied perspective recommend type 3 tests (e.g., Maxwell and Delaney, 2004). Consequently, they are the default for the ANOVA functions described here. For some more discussion on this issue see \href{http://stats.stackexchange.com/q/6208/442}{here}. 
#'
#' Note that lower order effects (e.g., main effects) in type 3 ANOVAs are only meaningful with \href{http://www.ats.ucla.edu/stat/mult_pkg/faq/general/effect.htm}{effects coding}. That is, contrasts should be set to \code{\link{contr.sum}} to obtain meaningful results. This is imposed automatically for the functions discussed here as long as \code{check.contrasts} is \code{TRUE} (the default). I nevertheless recommend to set the contrasts globally to \code{contr.sum} via running \code{\link{set_sum_contrasts}}. For a discussion of the other (non-recommended) coding schemes see \href{http://www.ats.ucla.edu/stat/r/library/contrast_coding.htm}{here}. 
#' }
#' 
#' \subsection{Follow-Up Contrasts and Post-Hoc Tests}{ 
#' The S3 object returned per default can be directly passed to \code{lsmeans::lsmeans} for further analysis. This allows to test any type of contrasts that might be of interest independent of whether or not this contrast involves between-subject variables, within-subject variables, or a combination thereof. The general procedure to run those contrasts is the following (see Examples for a full example):
#' 
#'  \enumerate{
#'    \item Estimate an \code{afex_aov} object with the function returned here. For example: \code{x <- aov_car(dv ~ a*b + (id/c), d)}
#'    \item Obtain a \code{\link[lsmeans]{ref.grid}} object by running \code{\link[lsmeans]{lsmeans}} on the \code{afex_aov} object from step 1 using the factors involved in the contrast. For example: \code{r <- lsmeans(x, ~a:c)}
#'    \item Create a list containing the desired contrasts on the reference grid object from step 2. For example: \code{con1 <- list(a_x = c(-1, 1, 0, 0, 0, 0), b_x = c(0, 0, -0.5, -0.5, 0, 1))}
#'    \item Test the contrast on the reference grid using \code{\link[lsmeans]{contrast}}. For example: \code{contrast(r, con1)}
#'    \item To control for multiple testing p-value adjustments can be specified. For example the Bonferroni-Holm correction: \code{contrast(r, con1, adjust = "holm")}
#'  }
#'  
#'  Note that \pkg{lsmeans} allows for a variety of advanced settings and simplifiations, for example: all pairwise comparison of a single factor using one command (e.g., \code{lsmeans(x, "a", contr = "pairwise")}) or advanced control for multiple testing by passing objects to \pkg{multcomp}. A comprehensive overview of the functionality is provided in the accompanying vignettes (see \href{http://cran.r-project.org/package=lsmeans}{here}).
#'  
#'  A caveat regarding the use of \pkg{lsmeans} concerns the assumption of sphericity for ANOVAs including within-subjects/repeated-measures factors (with more than two levels). While the ANOVA tables per default report results using the Greenhousse-Geisser correction, no such correction is available when using \pkg{lsmeans}. This may result in anti-conservative tests.
#'  
#'  \pkg{lsmeans} is loaded/attached automatically when loading \pkg{afex} via \code{library} or \code{require}.
#' }  
#' 
#' \subsection{Methods for \code{afex_aov} Objects}{
#' A full overview over the methods provided for \code{afex_aov} objects is provided in the corresponding help page: \code{\link{afex_aov-methods}}. The probably most important ones for end-users are \code{summary} and \code{anova}.
#' 
#' The \code{summary} method returns, for ANOVAs containing within-subject (repeated-measures) factors with more than two levels, the complete univariate analysis: Results without df-correction, the Greenhouse-Geisser corrected results, the Hyunh-Feldt corrected results, and the results of the Mauchly test for sphericity.
#' 
#' The \code{anova} method returns a \code{data.frame} of class \code{"anova"} containing the ANOVA table in numeric form (i.e., the one in slot \code{anova_table} of a \code{afex_aov}). This method has arguments such as \code{correction} and \code{es} and can be used to obtain an ANOVA table with different correction than the one initially specified.
#' }
#' 
#' @author Henrik Singmann
#'
#' The design of these functions was influenced by \code{\link[ez]{ezANOVA}} from package \pkg{ez}.
#'
#' @note Calculation of ANOVA models via \code{aov} (which is done per default) can be comparatively slow and produce comparatively large objects for ANOVAs with many within-subjects factors or levels. To avoid this calculation set the return argument to \code{"nice"}. This can also be done globally via \code{afex_options(return_aov = "nice")}. \code{return = "nice"} also produces the default output of previous versions of afex (versions 0.13 and earlier).
#' 
#' The id variable and variables entered as within-subjects (i.e., repeated-measures) factors are silently converted to factors. Levels of within-subject factors are converted to valid variable names using \code{\link{make.names}(...,unique=TRUE)}. Unused factor levels are silently dropped on all variables.
#'
#' Contrasts attached to a factor as an attribute are probably not preserved and not supported.
#' 
#' The workhorse is \code{aov_car}. \code{aov_4} and \code{aov_ez} only construe and pass an appropriate formula to \code{aov_car}. Use \code{print.formula = TRUE} to view this formula.
#' 
#' In contrast to \code{\link{aov}} \code{aov_car} assumes that all factors to the right of \code{/} in the \code{Error} term are belonging together. Consequently, \code{Error(id/(a*b))} and \code{Error(id/a*b)} are identical (which is not true for \code{\link{aov}}).
#'
#' @seealso Various methods for objects of class \code{afex_aov} are available: \code{\link{afex_aov-methods}}
#' 
#' \code{\link{nice}} creates the nice ANOVA tables which is by default printed. See also there for a slightly longer discussion of the available effect sizes.
#'
#' \code{\link{mixed}} provides a (formula) interface for obtaining p-values for mixed-models via \pkg{lme4}.
#'
#' @references Maxwell, S. E., & Delaney, H. D. (2004). \emph{Designing Experiments and Analyzing Data: A Model-Comparisons Perspective}. Mahwah, N.J.: Lawrence Erlbaum Associates.
#'
#' Venables, W.N. (2000). \emph{Exegeses on linear models}. Paper presented to the S-Plus User's Conference, Washington DC, 8-9 October 1998, Washington, DC. Available from: \url{http://www.stats.ox.ac.uk/pub/MASS3/Exegeses.pdf}
#'
#' @name aov_car
#' @aliases aov_ez aov_car aov_4
#' @export aov_ez aov_car  aov_4
#' @importFrom car Anova
#' @importFrom stringr str_c str_detect str_replace_all str_extract
#' @importFrom reshape2 dcast
#' @importFrom lme4 findbars nobars 
#' @importFrom stats terms as.formula xtabs contrasts<- coef
#' 
#' @example examples/examples.aov_car.R
#' 
#' 
#' @encoding UTF-8
#'




aov_car <- function(formula, data, fun.aggregate = NULL, type = afex_options("type"), factorize = afex_options("factorize"), check.contrasts = afex_options("check.contrasts"), return = afex_options("return_aov"), observed = NULL, anova_table = list(), ...) {
  return <- match.arg(return, c("Anova", "lm", "data", "nice", "afex_aov", "univariate", "marginal", "aov"))
  # stuff copied from aov:
  Terms <- terms(formula, "Error", data = data)
  indError <- attr(Terms, "specials")$Error
  if (length(indError) > 1L) 
    stop(sprintf(ngettext(length(indError), "there are %d Error terms: only 1 is allowed", 
                          "there are %d Error terms: only 1 is allowed"), length(indError)), 
         domain = NA)
  # from here, code by Henrik Singmann:
  vars <- all.vars(formula)
  dv <- vars[1]
  #chec if dv is numeric:
  if (!is.numeric(data[,dv])) stop("dv needs to be numeric.")
  vars <- vars[-1]
  parts <- attr(terms(formula, "Error", data = data), "term.labels")
  error.term <- parts[str_detect(parts, "^Error\\(")]
  id <- all.vars(parse(text = error.term))[1]
  within <- all.vars(parse(text = error.term))[-1]
  between <- vars[!(vars %in% c(id, within))]
  effect.parts <- parts[!str_detect(parts, "^Error\\(")]
  effect.parts.no.within <- effect.parts[!str_detect(effect.parts, str_c("\\<",within,"\\>", collapse = "|"))]
  data <- droplevels(data) #remove empty levels.
  # make id and within variables to factors:
  if (!(is.factor(data[,id]))) data[,id] <- factor(data[,id])
  # factorize if necessary
  if (factorize) {
    if (any(!vapply(data[, between, drop = FALSE], is.factor, TRUE))) {
      to.factor <- between[!vapply(data[,between, drop = FALSE], is.factor, TRUE)]
      message(str_c("Converting to factor: ", str_c(to.factor, collapse = ", ")))
      for (tmp.c in to.factor) {
        data[,tmp.c] <- factor(data[,tmp.c])
      }
    }
  } else {
    # check if numeric variables are centered.
    c.ns <- between[vapply(data[, between, drop = FALSE], is.numeric, TRUE)]
    if (length(c.ns) > 0) {
      non.null <- c.ns[!abs(vapply(data[, c.ns, drop = FALSE], mean, 0)) < .Machine$double.eps ^ 0.5]
      if (length(non.null) > 0) warning(str_c("Numerical variables NOT centered on 0 (i.e., likely bogus results): ", str_c(non.null, collapse = ", ")))
    }
  }
  for (i in c(between, within)) {
    if (is.factor(data[,i]) && length(unique(data[,i])) == 1) stop(paste0("Factor \"", i, "\" consists of one level only. Remove factor from model?"))
  }
  # make formulas
  rh2 <- if (length(between) > 0) str_c(effect.parts.no.within, collapse = "+") else "1"
  lh1 <- str_c(id, if (length(between) > 0) str_c(between, collapse = "+") else NULL, sep = "+")
  rh1 <- str_c(within, collapse = "+")
  rh3 <- str_c(within, collapse = "*")
  # converting all within subject factors to factors and adding a leading charcter (x) if starting with a digit.
  for (within.factor in within) {
    if (is.factor(data[,within.factor])) levels(data[,within.factor]) <- make.names(levels(data[,within.factor]), unique = TRUE)
    else data[,within.factor] <- factor(as.character(data[,within.factor]), levels = unique(as.character(data[,within.factor])), labels = make.names(unique(as.character(data[,within.factor])), unique=TRUE))
  }
  # Check if each id is in only one between subjects cell.
  between.factors <- between[vapply(data[, between, drop = FALSE], is.factor, TRUE)]
  if (length(between.factors) > 0) {
    split.data <- split(data, lapply(between.factors, function(x) data[,x]))
    ids.per.condition <- lapply(split.data, function(x) unique(as.character(x[,id])))
    ids.in.more.condition <- unique(unlist(lapply(seq_along(ids.per.condition), function(x) unique(unlist(lapply(ids.per.condition[-x], function(y, z = ids.per.condition[[x]]) intersect(z, y)))))))
    if (length(ids.in.more.condition) > 0) stop(str_c("Following ids are in more than one between subjects condition:\n", str_c(ids.in.more.condition, collapse = ", ")))
  }
  # Is fun.aggregate NULL and aggregation necessary?
  if (is.null(fun.aggregate)) {
    if (any(xtabs(as.formula(str_c("~", id, if (length(within) > 0) "+", rh1)), data = data) > 1)) {
      warning("More than one observation per cell, aggregating the data using mean (i.e, fun.aggregate = mean)!")
      fun.aggregate <- mean
    }
  } 
  # if return = "lme4" return the (aggregated) data fitted with lmer!
  #   if (return == "lme4") {
  #     warning("lme4 return is experimental!\nAlso: Missing values and contrasts not checked for return = 'lme4'!")
  #     n.dat <- dcast(data, formula = as.formula(str_c(lh1, if (length(within) > 0) paste0("+", rh1) else "", "~ .", sep = "")), fun.aggregate = fun.aggregate, ..., value.var = dv)
  #     colnames(n.dat)[length(colnames(n.dat))] <- "value"
  #     f.within.new <- str_replace_all(rh1, pattern="\\+", replacement="*")
  #     return(lmer(as.formula(str_c("value~", rh2, if (length(within) > 0) paste0("*", f.within.new) else "", "+ (1", if (length(within) > 0) paste0("+", f.within.new) else "", "|", id, ")" , sep = "")), data = n.dat))
  #   }
  # prepare the data:
  tmp.dat <- dcast(data, formula = as.formula(str_c(lh1, if (length(within) > 0) rh1 else ".", sep = "~")), fun.aggregate = fun.aggregate, ..., value.var = dv)
  # check for missing values:
  if (any(is.na(tmp.dat))) {
    missing.values <- apply(tmp.dat, 1, function(x) any(is.na(x)))
    warning(str_c("Missing values for following ID(s):\n", str_c(tmp.dat[missing.values,1], collapse = ", "), "\nRemoving those cases from the analysis."))        
  }
  #   if (length(between) > 0) {
  #     n_data_points <- xtabs(as.formula(paste("~", paste(between, collapse = "+"))), data = tmp.dat)
  #     if (any(n_data_points == 0)) warning("Some cells of the fully crossed between-subjects design are empty. A full model might not be estimable.")
  #   }
  # marginals: (disabled in April 2015)
  dat.ret <- dcast(data, formula = as.formula(str_c(str_c(lh1, if (length(within) > 0) rh1 else NULL, sep = "+"), "~.")), fun.aggregate = fun.aggregate, ..., value.var = dv)
  colnames(dat.ret)[length(colnames(dat.ret))] <- dv
  #   full.formula <- as.formula(str_c(dv, " ~ ", str_c(c(between.factors, within), collapse = "*")))
  #   all.terms <- attr(terms(full.formula), "term.labels")
  #   marginals.out <- lapply(all.terms, function(x) aggregate(as.formula(str_c(dv, " ~ ", x)), dat.ret, mean))
  #   names(marginals.out) <- all.terms
  #   grand.mean <- data.frame(mean(dat.ret[,dv]))
  #   colnames(grand.mean) <- dv
  #   marginals.out <- c(grand_mean = list(grand.mean), marginals.out)
  #   if (return == "marginal") {
  #     return(marginals.out)
  #   }
  if (length(between) > 0) {
    if (check.contrasts) {
      resetted <- NULL
      for (i in between) {
        if (is.factor(tmp.dat[,i])) {
          if (is.null(attr(tmp.dat[,i], "contrasts")) & (options("contrasts")[[1]][1] != "contr.sum")) {
            contrasts(tmp.dat[,i]) <- "contr.sum"
            resetted  <- c(resetted, i)
          }
          else if (!is.null(attr(tmp.dat[,i], "contrasts")) && attr(tmp.dat[,i], "contrasts") != "contr.sum") {
            contrasts(tmp.dat[,i]) <- "contr.sum"
            resetted  <- c(resetted, i)
          }
        }
      }
      if (!is.null(resetted)) message(str_c("Contrasts set to contr.sum for the following variables: ", str_c(resetted, collapse=", ")))
    } else {
      non_sum_contrast <- c()
      for (i in between) {
        if (is.factor(tmp.dat[,i])) {
          if (is.null(attr(tmp.dat[,i], "contrasts")) & (options("contrasts")[[1]][1] != "contr.sum")) {
            non_sum_contrast <- c(non_sum_contrast, between)
          }
          else if (!is.null(attr(tmp.dat[,i], "contrasts")) && attr(tmp.dat[,i], "contrasts") != "contr.sum") {
            non_sum_contrast <- c(non_sum_contrast, between)
          }
        }
      }
      if((type == 3 | type == "III") && (length(non_sum_contrast)>0)) warning(str_c("Calculating Type 3 sums with contrasts != 'contr.sum' for: ", paste0(non_sum_contrast, collapse=", "), "\n  Results likely bogus or not interpretable!\n  You probably want check.contrasts = TRUE or options(contrasts=c('contr.sum','contr.poly'))"))
    }
  }
  if (return %in% c("aov", "afex_aov")) include.aov <- TRUE
  else include.aov <- FALSE
  if(include.aov){
    if (check.contrasts) {
      factor_vars <- vapply(dat.ret[,c(within, between), drop = FALSE], is.factor, NA)
      contrasts <- as.list(rep("contr.sum", sum(factor_vars)))
      names(contrasts) <- c(within, between)[factor_vars]
    }
    #return(aov(formula(paste(dv, "~", paste(c(between, within), collapse = "*"),  if (length(within) > 0) paste0("+Error(", id, "/(",paste(within, collapse="*"), "))") else NULL)), data=dat.ret, contrasts = contrasts))
    aov <- aov(formula(paste(dv, "~", paste(c(between, within), collapse = "*"),  if (length(within) > 0) paste0("+Error(", id, "/(",paste(within, collapse="*"), "))") else NULL)), data=dat.ret, contrasts = contrasts)
  }
  if(return == "aov") return(aov)
  data.l <- list(long = dat.ret, wide = tmp.dat)
  if (return == "data") return(tmp.dat)
  # branching based on type of ANOVA
  if (length(within) > 0) {  # if within-subject factors are present:
    # make idata argument
    if (length(within) > 1) {
      within.levels <- lapply(lapply(data[,within], levels), factor)
      idata <- rev(expand.grid(rev(within.levels)))
    } else {
      idata <- data.frame(levels(data[,within]))
      colnames(idata) <- within
    }
    # print(as.formula(str_c("cbind(",str_c(colnames(tmp.dat[-(seq_along(c(id, between)))]), collapse = ", "), ") ~ ", rh2)))
    # browser()
    tmp.lm <- do.call("lm", list(formula = as.formula(str_c("cbind(",str_c(colnames(tmp.dat[-(seq_along(c(id, between)))]), collapse = ", "), ") ~ ", rh2)), data = tmp.dat))
    # browser()
    if (any(is.na(coef(tmp.lm)))) stop("Some parameters are not estimable, most likely due to empty cells of the design (i.e., structural missings). Check your data.")
    if (return == "lm") return(tmp.lm)
    Anova.out <- Anova(tmp.lm, idata = idata, idesign = as.formula(str_c("~", rh3)), type = type)
    data.l <- c(data.l, idata = list(idata))
  } else { # if NO within-subjetc factors are present (i.e., purley between ANOVA):
    colnames(tmp.dat)[ncol(tmp.dat)] <- "dv"
    tmp.lm <- do.call("lm", list(formula = as.formula(str_c("dv ~ ", rh2)), data = tmp.dat))
    if (return == "lm") return(tmp.lm)
    Anova.out <- Anova(tmp.lm, type = type)
  }
  if (return == "afex_aov") {
    afex_aov <- list(
      anova_table = NULL, 
      aov = aov,
      Anova = Anova.out,
      lm = tmp.lm,
      data = data.l
    )
    class(afex_aov) <- "afex_aov"
    attr(afex_aov, "dv") <- dv
    attr(afex_aov, "id") <- id
    attr(afex_aov, "within") <- within
    attr(afex_aov, "between") <- between
    attr(afex_aov, "type") <- type
    afex_aov$anova_table <- do.call("anova", args = c(object = list(afex_aov), observed = list(observed), anova_table))
    return(afex_aov)
  }
  if (return == "Anova") return(Anova.out)
  else if (return == "univariate") {
    if (class(Anova.out) == "Anova.mlm") return(summary(Anova.out, multivariate = FALSE))
    else return(Anova.out)
  }
  else if (return == "nice") {
     afex_aov <- list(
      anova_table = NULL,
      Anova = Anova.out
    )
    class(afex_aov) <- "afex_aov"
    attr(afex_aov, "dv") <- dv
    attr(afex_aov, "id") <- id
    attr(afex_aov, "within") <- within
    attr(afex_aov, "between") <- between
    attr(afex_aov, "type") <- type
    #afex_aov$anova_table <- do.call("anova", args = c(object = list(afex_aov), observed = list(observed), args.return))
    return(do.call("nice", args = c(object = list(afex_aov), observed = list(observed), anova_table)))
  }
}

aov_4 <- function(formula, data, observed = NULL, fun.aggregate = NULL, type = afex_options("type"), factorize = afex_options("factorize"), check.contrasts = afex_options("check.contrasts"), return = afex_options("return_aov"), anova_table = list(), ..., print.formula = FALSE) {
  #browser()
  barterms <- findbars(formula)
  if (length(barterms) > 1) stop("aov_4 only allows one random effect term")
  within <- all.vars(barterms[[1]][[2]])
  id <- all.vars(barterms[[1]][[3]])
  error <- str_c(" + Error(", id, if (length(within) > 0) "/(" else "", str_c(within, collapse = " * "), if (length(within) > 0) ")" else "", ")")
  lh <- as.character(nobars(formula))
  if (length(lh) == 1) {
   dv <- lh
   rh <- "1"
  } else {
    dv <- lh[2]
    rh <- lh[3]
  }
  formula <- str_c(dv, " ~ ", rh, error)
  if (print.formula) message(str_c("Formula send to aov_car: ", formula))
  aov_car(formula = as.formula(formula), data = data, fun.aggregate = fun.aggregate, type = type, return = return, factorize = factorize, check.contrasts = check.contrasts, observed = observed, anova_table = anova_table, ...)
}



aov_ez <- function(id, dv, data, between = NULL, within = NULL, covariate = NULL, observed = NULL, fun.aggregate = NULL, type = afex_options("type"), factorize = afex_options("factorize"), check.contrasts = afex_options("check.contrasts"), return = afex_options("return_aov"), anova_table = list(), ..., print.formula = FALSE) {
  if (is.null(between) & is.null(within)) stop("Either between or within need to be non-NULL!")
  if (!is.null(covariate)) covariate <- str_c(covariate, collapse = "+")
  #browser()
  rh <- if (!is.null(between) || !is.null(covariate)) str_c(if (!is.null(between)) str_c(between, collapse = " * ") else NULL, covariate, sep = " + ") else "1"
  error <- str_c(" + Error(", id, if (!is.null(within)) "/(" else "", str_c(within, collapse = " * "), if (length(within) > 0) ")" else "", ")")
  formula <- str_c(dv, " ~ ", rh, error)
  if (print.formula) message(str_c("Formula send to aov_car: ", formula))
  aov_car(formula = as.formula(formula), data = data, fun.aggregate = fun.aggregate, type = type, return = return, factorize = factorize, check.contrasts = check.contrasts, observed = observed, anova_table = anova_table, ...)
}


