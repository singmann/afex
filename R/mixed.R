#' p-values for fixed effects of mixed-model via lme4::lmer()
#'
#' Calculates p-values for all fixed effects in a mixed model. This is done by first fitting (with \code{\link[lme4]{lmer}}) the full model and then versions thereof in which a single effect is removed and comparing the reduced model to the full model. The default is to calculate type 3 like p-values using the Kenward-Roger approximation for degrees-of-freedom (using \code{\link[pbkrtest]{KRmodcomp}}; for LMMs only). Other methods for obtaining p-values are parametric bootstrap (\code{method = "PB"}) or likelihood ratio tests (\code{method = "LRT"}), both of which are available for both LMMs and GLMMs. \code{print}, \code{summary}, and \code{anova} methods for the returned object of class \code{"mixed"} are available (the last two return the same data.frame). \code{lmer_alt} is simply a wrapper for mixed that only returns the \code{"merMod"} object and correctly uses the \code{||} notation to remove correlation among factors, but otherwise behaves like \code{g/lmer} (as for \code{mixed}, it calls \code{glmer} as soon as a \code{family} argument is present).
#'
#'
#' @param formula a formula describing the full mixed-model to be fitted. As this formula is passed to \code{lmer}, it needs at least one random term.
#' @param data data.frame containing the data. Should have all the variables present in \code{fixed}, \code{random}, and \code{dv} as columns.
#' @param type type of test on which effects are based. Default is to use type 3 tests, taken from \code{\link{afex_options}}.
#' @param method character vector indicating which methods for obtaining p-values should be used: \code{"KR"} corresponds to the Kenward-Roger approximation for degrees of freedom (only working with linear mixed models), \code{"PB"} calculates p-values based on parametric bootstrap, \code{"LRT"} calculates p-values via the likelihood ratio tests implemented in the \code{anova} method for \code{merMod} objects (only recommended for models with many [i.e., > 50] levels for the random factors). The default (currently \code{"KR"}) is taken from \code{\link{afex_options}}.
#' @param per.parameter \code{character} vector specifying for which variable tests should be run for each parameter (instead for the overall effect). Can be useful e.g., for testing ordered factors. Relatively untested so results should be compared with a second run without setting this argument. Uses \code{\link{grep}} for selecting parameters among the fixed effects so regular expressions (\code{\link{regex}}) are possible. See Examples.
#' @param args.test \code{list} of arguments passed to the function calculating the p-values. See Details.
#' @param test.intercept logical. Whether or not the intercept should also be fitted and tested for significance. Default is \code{FALSE}. Only relevant if \code{type = 3}.
#' @param check.contrasts \code{logical}. Should contrasts be checked and (if necessary) changed to \code{"contr.sum"}? See Details. The default (\code{"TRUE"}) is taken from \code{\link{afex_options}}.
#' @param expand_re logical. Should random effects terms be expanded (i.e., factors transformed into numerical variables) before fitting with \code{(g)lmer}? Allows to use "||" notation with factors.
#' @param set.data.arg \code{logical}. Should the data argument in the slot \code{call} of the \code{merMod} object returned from \code{lmer} be set to the passed data argument? Otherwise the name will be \code{data}. Helpful if fitted objects are used afterwards (e.g., using \pkg{lsmeans}). Default is \code{TRUE}. 
#' @param progress  if \code{TRUE}, shows progress with a text progress bar and other status messages during fitting.
#' @param cl  A vector identifying a cluster; used for distributing the estimation of the different models using several cores. See examples. If \code{ckeck.contrasts}, mixed sets the current contrasts (\code{getOption("contrasts")}) at the nodes. Note this does \emph{not} distribute calculation of p-values (e.g., when using \code{method = "PB"}) across the cluster. Use \code{args.test} for this.
#' @param return the default is to return an object of class \code{"mixed"}. \code{return = "merMod"} will skip the calculation of all submodels and p-values and simply return the full model fitted with lmer. Can be useful in combination with \code{expand_re = TRUE} which allows to use "||" with factors. \code{return = "data"} will not fit any models but just return the data that would have been used for fitting the model. Can be used in combination with \code{expand_re = TRUE} and \code{\link{allFit}} (see examples there).
#' @param ... further arguments (such as \code{weights}/\code{family}) passed to \code{\link{lmer}}/\code{\link{glmer}}.
#'
#'
#' @return An object of class \code{"mixed"} (i.e., a list) with the following elements:
#'
#' \enumerate{
#' \item \code{anova_table} a data.frame containing the statistics returned from \code{\link[pbkrtest]{KRmodcomp}}. The \code{stat} column in this data.frame gives the value of the test statistic, an F-value for \code{method = "KR"} and a chi-square value for the other two methods.
#' \item \code{full.model} the \code{"lmerMod"} object returned from fitting the full mixed model.
#' \item \code{restricted.models} a list of \code{"lmerMod"} objects from fitting the restricted models (i.e., each model lacks the corresponding effect)
#' \item \code{tests} a list of objects returned by the function for obtaining the p-values.
#' }
#' 
#' It also has the following attributes, \code{"type"} and \code{"method"}.
#'
#' Two similar methods exist for objects of class \code{"mixed"}: \code{print} and \code{anova}. They print a nice version of the \code{anova_table} element of the returned object (which is also invisibly returned). This methods omit some columns and nicely round the other columns. The following columns are always printed:
#' \enumerate{
#' \item \code{Effect} name of effect
#' \item \code{p.value} estimated p-value for the effect
#' }
#'
#' For LMMs with \code{method="KR"} the following further columns are returned (note: the Kenward-Roger correction does two separate things: (1) it computes an effective number for the denominator df; (2) it scales the statistic by a calculated amount, see also \url{http://stackoverflow.com/a/25612960/289572}):
#' \enumerate{
#' \item \code{F} computed F statistic
#' \item \code{ndf} numerator degrees of freedom (number of parameters used for the effect)
#' \item \code{ddf} denominator degrees of freedom (effective residual degrees of freedom for testing the effect), computed from the Kenward-Roger correction using \code{pbkrtest::KRmodcomp}
#' \item \code{F.scaling} scaling of F-statistic computing from Kenward-Roger approximation.
#' }
#' 
#' For models with \code{method="LRT"} the following further columns are returned:
#' \enumerate{
#' \item \code{df.large} degrees of freedom (i.e., estimated paramaters) for full model (i.e., model containing the corresponding effect)
#' \item \code{df.small} degrees of freedom (i.e., estimated paramaters) for restricted model (i.e., model without the corresponding effect)
#' \item \code{chisq} 2 times the difference in likelihood (obtained with \code{logLik}) between full and restricted model
#' \item \code{df} difference in degrees of freedom between full and restricted model (p-value is based on these df).
#' }
#' 
#' For models with \code{method="PB"} the following further column is returned:
#' \enumerate{
#' \item \code{stat} 2 times the difference in likelihood (obtained with \code{logLik}) between full and restricted model (i.e., a chi-square value).
#' }
#'
#' Note that  \code{anova} can also be called with additional mixed and/or \code{merMod} objects. In this casethe full models are passed on to \code{anova.merMod} (with \code{refit=FALSE}, which differs from the default of \code{anova.merMod}) which produces the known LRT tables.
#'
#' The \code{summary} method for objects of class \code{mixed} simply calls \code{\link{summary.merMod}} on the full model.
#' 
#' If \code{return = "merMod"}, an object of class \code{"merMod"}, as returned from \code{g/lmer}, is returned.
#' 
#' @details For an introduction to mixed-modeling for experimental designs see Barr, Levy, Scheepers, & Tily (2013; I highly recommend reading this paper if you use this function), arguments for using the Kenward-Roger approximation for obtaining p-values are given by Judd, Westfall, and Kenny (2012). Further introductions to mixed-modeling for experimental designs are given by Baayen and colleagues (Baayen, 2008; Baayen, Davidson & Bates, 2008; Baayen & Milin, 2010). Specific recommendations on which random effects structure to specify for confirmatory tests can be found in Barr and colleagues (2013) and Barr (2013), but also see Bates et al. (2015).
#'
#'\subsection{p-value Calculations}{
#'
#' p-values are per default calculated via methods from \pkg{pbkrtest}. When \code{method = "KR"} (the default), the Kenward-Roger approximation for degrees-of-freedom is calculated using \code{\link[pbkrtest]{KRmodcomp}}, which is only applicable to linear-mixed models. The test statistic in the output is a F-value (\code{F}).
#'
#' \code{method = "PB"} calculates p-values using parametric bootstrap using \code{\link[pbkrtest]{PBmodcomp}}. This can be used for linear and also generalized linear mixed models (GLMM) by specifying a \code{\link[stats]{family}} argument to \code{mixed}. Note that you should specify further arguments to \code{PBmodcomp} via \code{args.test}, especially \code{nsim} (the number of simulations to form the reference distribution) or \code{cl} (for using multiple cores). For other arguments see \code{\link[pbkrtest]{PBmodcomp}}. Note that \code{REML} (argument to \code{[g]lmer}) will be set to \code{FALSE} if method is \code{PB}.
#'
#' \code{method = "LRT"} calculates p-values via likelihood ratio tests implemented in the \code{anova} method for \code{"merMod"} objects. This is recommended by Barr et al. (2013; which did not test the other methods implemented here). Using likelihood ratio tests is only recommended for models with many levels for the random effects (> 50), but can be pretty helpful in case the other methods fail (due to memory and/or time limitations). The \href{http://glmm.wikidot.com/faq}{lme4 faq} also recommends the other methods over likelihood ratio tests.
#' }
#' 
#' \subsection{Implementation Details}{
#' 
#' Type 3 tests are obtained by comparing a model in which only the tested effect is excluded with the full model (containing all effects). This corresponds to the (type 3) Wald tests given by \code{car::Anova} for \code{"lmerMod"} models. The submodels in which the tested effect is excluded are obtained by manually creating a model matrix which is then fitted in \code{"lme4"}. This is done to avoid R's "feature" to not allow this behavior.
#'
#' Type 2 tests are truly sequential. They are obtained by comparing a model in which the tested effect and all higher oder effect (e.g., all three-way interactions for testing a two-way interaction) are excluded with a model in which only effects up to the order of the tested effect are present and all higher order effects absent. In other words, there are multiple full models, one for each order of effects. Consequently, the results for lower order effects are identical of whether or not higher order effects are part of the model or not. This latter feature is not consistent with classical ANOVA type 2 tests but a consequence of the sequential tests (and \href{https://stat.ethz.ch/pipermail/r-sig-mixed-models/2012q3/018992.html}{I didn't find a better way} of implementing the Type 2 tests). This \strong{does not} correspond to the (type 2) Wald test reported by \code{car::Anova}. If you want type 2 Wald tests instead of truly sequential typde 2 tests, use \code{car::Anova} with \code{test = "F"}. Note that the order in which the effects are entered into the formula does not matter (in contrast to type 1 tests).
#' 
#' If \code{check.contrasts = TRUE}, contrasts will be set to \code{"contr.sum"} for all factors in the formula if default contrasts are not equal to \code{"contr.sum"} or \code{attrib(factor, "contrasts") != "contr.sum"}. Furthermore, the current contrasts (obtained via \code{getOption("contrasts")}) will be set at the cluster nodes if \code{cl} is not \code{NULL}.
#' }
#' 
#' \subsection{Expand Random Effects}{
#' The latest addition, motivated by Bates et al. (2015), is the possibility to expand the random effects structure before passing it to \code{lmer} by setting \code{expand_re = TRUE}. This allows to disable estimation of correlation among random effects for random effects term containing factors using the \code{||} notation. This is achieved by first creating a model matrix for each random effects term individually, rename and append the so created columns to the data that will be fitted, replace the actual random effects term with the so created variables (concatenated with +), and then fit the model. The variables are renamed by prepending all variables with rei (where i is the number of the random effects term) and replacing ":" with "_by_".
#' 
#' \code{lmer_alt} is simply a wrapper for \code{mixed} that is intended to behave like \code{lmer} (or \code{glmer} if a \code{family} argument is present), but also allows to use \code{||} with factors correctly (by always using \code{expand_re = TRUE}). This means that \code{lmer_alt} per default does not enforce a specific contrast on factors and only returns the \code{"merMod"} object without calculating any additional models or p-values (this is achieved by setting \code{return = "merMod"}). Note that it most likely differs from \code{g/lmer} in how it handles missing values so it is recommended to only pass data without missing values to it!
#' 
#' One negative consequence of using \code{expand_re = TRUE} is that the data that is fitted will not be the same as the passed data.frame which can lead to problems with e.g., the \code{predict} method. Finally, note that this functionality is relatively new so please proceed with care.
#'  } 
#'
#' @note When \code{method = "KR"}, obtaining p-values is known to crash due too insufficient memory or other computational limitations (especially with complex random effects structures). In these cases, the other methods should be used. The RAM demand is a problem especially on 32 bit Windows which only supports up to 2 or 3GB RAM (see \href{http://cran.r-project.org/bin/windows/base/rw-FAQ.html}{R Windows FAQ}). Then it is probably a good idea to use methods "LRT" or "PB".
#'
#' \code{"mixed"} will throw a message if numerical variables are not centered on 0, as main effects (of other variables then the numeric one) can be hard to interpret if numerical variables appear in interactions. See Dalal & Zickar (2012).
#'
#' Formulas longer than 500 characters will most likely fail due to the use of \code{\link{deparse}}.
#' 
#' Please report bugs or unexpected behavior by opening a guthub issue: \url{https://github.com/singmann/afex/issues}
#'
#' @author Henrik Singmann with contributions from \href{http://stackoverflow.com/q/11335923/289572}{Ben Bolker and Joshua Wiley}.
#'
#' @seealso \code{\link{aov_ez}} and \code{\link{aov_car}} for convenience functions to analyze experimental deisgns with classical ANOVA or ANCOVA wrapping \code{\link[car]{Anova}}. 
#' 
#' see the following for the data sets from Maxwell and Delaney (2004) used and more examples: \code{\link{md_15.1}}, \code{\link{md_16.1}}, and \code{\link{md_16.4}}.
#'
#' @references Baayen, R. H. (2008). \emph{Analyzing linguistic data: a practical introduction to statistics using R}. Cambridge, UK; New York: Cambridge University Press.
#'
#' Baayen, R. H., Davidson, D. J., & Bates, D. M. (2008). Mixed-effects modeling with crossed random effects for subjects and items. \emph{Journal of Memory and Language}, 59(4), 390-412. doi:10.1016/j.jml.2007.12.005
#' 
#' Baayen, R. H., & Milin, P. (2010). Analyzing Reaction Times. \emph{International Journal of Psychological Research}, 3(2), 12-28.
#' 
#' Barr, D. J. (2013). Random effects structure for testing interactions in linear mixed-effects models. \emph{Frontiers in Quantitative Psychology and Measurement}, 328. doi:10.3389/fpsyg.2013.00328
#' 
#' Barr, D. J., Levy, R., Scheepers, C., & Tily, H. J. (2013). Random effects structure for confirmatory hypothesis testing: Keep it maximal. \emph{Journal of Memory and Language}, 68(3), 255-278. doi:10.1016/j.jml.2012.11.001
#' 
#' Bates, D., Kliegl, R., Vasishth, S., & Baayen, H. (2015). \emph{Parsimonious Mixed Models}. arXiv:1506.04967 [stat]. Retrieved from \url{http://arxiv.org/abs/1506.04967}
#'
#' Dalal, D. K., & Zickar, M. J. (2012). Some Common Myths About Centering Predictor Variables in Moderated Multiple Regression and Polynomial Regression. \emph{Organizational Research Methods}, 15(3), 339-362. doi:10.1177/1094428111430540
#'
#' Judd, C. M., Westfall, J., & Kenny, D. A. (2012). Treating stimuli as a random factor in social psychology: A new and comprehensive solution to a pervasive but largely ignored problem. \emph{Journal of Personality and Social Psychology}, 103(1), 54-69. doi:10.1037/a0028347
#' 
#' Maxwell, S. E., & Delaney, H. D. (2004). \emph{Designing experiments and analyzing data: a model-comparisons perspective.} Mahwah, N.J.: Lawrence Erlbaum Associates.
#'
#'
#' @import pbkrtest
#' @importFrom lme4 lmer glmer nobars getME fixef isREML
#' @importFrom stringr str_replace
#' @importMethodsFrom Matrix t isSymmetric "%*%" solve diag
#' @importClassesFrom Matrix Matrix
#' @importFrom Matrix Matrix sparseMatrix rankMatrix
#' @importFrom parallel clusterCall clusterExport clusterEvalQ clusterApplyLB
#' @importFrom stats logLik terms as.formula contrasts<- model.matrix model.frame anova vcov
#' @importFrom methods is
#' @encoding UTF-8
#' 
#' @example examples/examples.mixed.R
#' 
#' @export
mixed <- function(formula, data, type = afex_options("type"), method = afex_options("method_mixed"), per.parameter = NULL, args.test = list(), test.intercept = FALSE, check.contrasts = afex_options("check.contrasts"), expand_re = FALSE, set.data.arg = TRUE, progress = TRUE, cl = NULL, return = "mixed", ...) {
  if (check.contrasts) {
    #browser()
    vars.to.check <- all.vars(formula)
    resetted <- NULL
    for (i in vars.to.check) {
      if (is.factor(data[,i])) {
        if (is.null(attr(data[,i], "contrasts")) & (options("contrasts")[[1]][1] != "contr.sum")) {
          contrasts(data[,i]) <- "contr.sum"
          resetted  <- c(resetted, i)
        }
        else if (!is.null(attr(data[,i], "contrasts")) && attr(data[,i], "contrasts") != "contr.sum") {
          contrasts(data[,i]) <- "contr.sum"
          resetted  <- c(resetted, i)
        }
      }
    }
    if (!is.null(resetted)) message(str_c("Contrasts set to contr.sum for the following variables: ", str_c(resetted, collapse=", ")))
  }
  #warning(str_c("Calculating Type 3 sums with contrasts = ", options("contrasts")[[1]][1], ".\n  Use options(contrasts=c('contr.sum','contr.poly')) instead"))
  # browser()
  method <- match.arg(method, c("KR", "PB", "LRT", "F"), several.ok=TRUE)
  ####################
  ### Part I: prepare fitting (i.e., obtain model info, check model, ...)
  ####################
  mc <- match.call()
  #browser()
  formula.f <- as.formula(formula)
  #if (class(formula) != "formula") message("Formula (the first argument) converted to formula.")
  if (!inherits(formula, "formula")) message("Formula (the first argument) converted to formula.")
  dv <- as.character(formula.f)[[2]]
  all.terms <- attr(terms(formula.f), "term.labels")
  effect.order <- attr(terms(formula.f), "order")
  effect.order <- effect.order[!grepl("\\|", all.terms)]
  max.effect.order <- max(effect.order)
  random <- str_c(str_c("(", all.terms[grepl("\\|", all.terms)], ")"), collapse = " + ")
  rh2 <- nobars(formula.f)
  rh2[[2]] <- NULL
  m.matrix <- model.matrix(rh2, data = data)
  fixed.effects <- attr(terms(rh2, data = data), "term.labels")
  mapping <- attr(m.matrix, "assign")
  fixed.vars <- all.vars(rh2)
  # check for missing values in variables used:
  if (nrow(m.matrix) != nrow(data)) {
    data <- model.frame(as.formula(str_c(vars.to.check[1], "~", str_c(vars.to.check[-1], collapse = "+"))), data = data)
    m.matrix <- model.matrix(rh2, data = data)
    warning(str_c("Due to missing values, reduced number of observations to ", nrow(data)))
    if(set.data.arg) {
      warning("Due to missing values, set.data.arg set to FALSE.")
      set.data.arg <- FALSE
    }
  }
  
  # check if numerical variables are centered
  c.ns <- fixed.vars[vapply(data[, fixed.vars, drop = FALSE], is.numeric, TRUE)]
  if (length(c.ns) > 0) {
    non.null <- c.ns[!abs(vapply(data[, c.ns, drop = FALSE], mean, 0)) < .Machine$double.eps ^ 0.5]
    if (length(non.null) > 0) message(str_c("Numerical variables NOT centered on 0 (i.e., interpretation of all main effects might be difficult if in interactions): ", str_c(non.null, collapse = ", ")))
  }
  if (expand_re) {
    random_parts <- str_c(all.terms[grepl("\\|", all.terms)])
    which_random_double_bars <- str_detect(random_parts, "\\|\\|")
    random_units <- str_replace(random_parts, "^.+\\|\\s+", "")
    tmp_random <- lapply(str_replace(random_parts, "\\|.+$", ""), function(x) as.formula(str_c("~", x)))
    tmp_model.matrix <- vector("list", length(random_parts))
    re_contains_intercept <- rep(FALSE, length(random_parts))
    new_random <- vector("character", length(random_parts))
    for (i in seq_along(random_parts)) {
      tmp_model.matrix[[i]] <- model.matrix(tmp_random[[i]], data = data)
      if (ncol(tmp_model.matrix[[i]]) == 0) stop("Invalid random effects term, e.g., (0|id)")
      if (colnames(tmp_model.matrix[[i]])[1] == "(Intercept)") {
        tmp_model.matrix[[i]] <- tmp_model.matrix[[i]][,-1, drop = FALSE]
        re_contains_intercept[i] <- TRUE
      }
      if (ncol(tmp_model.matrix[[i]]) > 0) {
        colnames(tmp_model.matrix[[i]]) <- str_c("re", i, ".", str_replace_all(colnames(tmp_model.matrix[[i]]), ":", "_by_"))
        new_random[i] <- str_c("(", as.numeric(re_contains_intercept[i]), "+", str_c(colnames(tmp_model.matrix[[i]]), collapse = "+"), if (which_random_double_bars[i]) "||" else "|", random_units[i], ")")
      } else {
        new_random[i] <- str_c("(", as.numeric(re_contains_intercept[i]), if (which_random_double_bars[i]) "||" else "|", random_units[i], ")")
      }
    }
    data <- cbind(data, as.data.frame(do.call(cbind, tmp_model.matrix)))
    random <- str_c(new_random, collapse = "+")
  }
  if (return == "data") return(data)
  ####################
  ### Part II: obtain the lmer fits
  ####################
  ## Part IIa: prepare formulas
  mf <- mc[!names(mc) %in% c("type", "method", "args.test", "progress", "check.contrasts", "per.parameter", "cl", "test.intercept", "expand_re", "return")]
  mf[["formula"]] <-as.formula(str_c(dv,deparse(rh2, width.cutoff = 500L),"+",random))   #formula.f
  if ("family" %in% names(mf)) mf[[1]] <- as.name("glmer")
  else mf[[1]] <- as.name("lmer")
  mf[["data"]] <- as.name("data")
  if ((method[1] %in% c("PB", "LRT")) & !("family" %in% names(mf))) if ((!"REML" %in% names(mf)) || mf[["REML"]]) {
    message("REML argument to lmer() set to FALSE for method = 'PB' or 'LRT'")
    mf[["REML"]] <- FALSE
  }
  #browser()
  if (return == "merMod") {
    out <- eval(mf)
    if(set.data.arg) out@call[["data"]] <- mc[["data"]]
    return(out)
  }
  ## prepare (g)lmer formulas:
  if (type == 3 | type == "III") {
    if (attr(terms(rh2, data = data), "intercept") == 1) fixed.effects <- c("(Intercept)", fixed.effects)
    #per.parameter <- c("hour", "treatment")
    # The next part alters the mapping of parameters to effects/variables if
    # per.parameter is not NULL (this does the complete magic).
    if (!is.null(per.parameter)) {
      fixed.to.change <- c()
      for (parameter in per.parameter) {
        fixed.to.change <- c(fixed.to.change, grep(parameter, fixed.effects))
      }
      fixed.to.change <- fixed.effects[sort(unique(fixed.to.change))]
      if ("(Intercept)" %in% fixed.to.change) fixed.to.change <- fixed.to.change[-1]
      fixed.all <- dimnames(m.matrix)[[2]]
      #tf2 <- fixed.to.change[2]
      for (tf2 in fixed.to.change) {
        tf <- which(fixed.effects == tf2)
        fixed.lower <- fixed.effects[seq_len(tf-1)]
        fixed.upper <- if (tf < length(fixed.effects)) fixed.effects[(tf+1):length(fixed.effects)] else NULL
        fixed.effects <- c(fixed.lower, fixed.all[which(mapping == (tf-1))], fixed.upper)
        map.to.replace <- which(mapping == (tf-1))
        map.lower <- mapping[seq_len(map.to.replace[1]-1)]
        map.upper <- if (max(map.to.replace) < length(mapping)) mapping[(map.to.replace[length(map.to.replace)]+1):length(mapping)] else NULL
        mapping <- c(map.lower, seq_along(map.to.replace) + map.lower[length(map.lower)], map.upper + length(map.to.replace)-1)
      }
    }
    # make formulas
    formulas <- vector("list", length(fixed.effects) + 1)
    formulas[[1]] <- mf[["formula"]]
    for (i in seq_along(fixed.effects)) {
      tmp.columns <- str_c(deparse(-which(mapping == (i-1))), collapse = "")
      formulas[[i+1]] <- as.formula(str_c(dv, "~ 0 + m.matrix[,", tmp.columns, "] +", random))
    }
    names(formulas) <- c("full.model", fixed.effects)
    if (!test.intercept && fixed.effects[1] == "(Intercept)") {
      fixed.effects <- fixed.effects[-1]
      formulas[["(Intercept)"]] <- NULL
    }
  } else if (type == 2 | type == "II") {
    #warning("Implementation of Type 2 method not unproblematic.\n  Check documentation or use car::Anova (Wald tests).")
    if (!is.null(per.parameter)) stop("per.parameter argument only implemented for Type 3 tests.")
    full.model.formulas <- vector("list", max.effect.order)
    submodel.formulas <- vector("list", length(fixed.effects))
    full.model.formulas[[length(full.model.formulas)]] <- mf[["formula"]]
    for (c in seq_len(max.effect.order)) {
      if (c == max.effect.order) next 
      tmp.columns <- str_c(deparse(-which(mapping %in% which(effect.order > c))), collapse = "")
      full.model.formulas[[c]] <-  as.formula(str_c(dv, "~ 0 + m.matrix[,", tmp.columns, "] +", random))
    }
    for (c in seq_along(fixed.effects)) {
      order.c <- effect.order[c]
      tmp.columns <- str_c(deparse(-which(mapping == (c) | mapping %in% if (order.c == max.effect.order) -1 else which(effect.order > order.c))), collapse = "")
      submodel.formulas[[c]] <- as.formula(str_c(dv, "~ 0 + m.matrix[,", tmp.columns, "] +", random))
    }
    formulas <- c(full.model.formulas, submodel.formulas)
  } else stop('Only type 3 and type 2 tests implemented.')
  ## Part IIb: fit models
  # single core
  if (is.null(cl)) {
    if (progress) cat(str_c("Fitting ", length(formulas), " (g)lmer() models:\n["))
    fits <- vector("list", length(formulas))
    for (i in seq_along(formulas)) {
      mf[["formula"]] <- formulas[[i]]
      fits[[i]] <- eval(mf)
      if (progress) cat(".")
    }
    if (progress) cat("]\n")
  } else {  # multicore
    eval.cl <- function(formula, m.call, progress) {
      m.call[[2]] <- formula
      res <- eval(m.call)
      if (progress) cat(".")
      return(res)
    }
    if (progress) cat(paste0("Fitting ", length(formulas), " (g)lmer() models.\n"))
    #junk <- clusterEvalQ(cl = cl, library("lme4", character.only = TRUE))
    junk <- clusterCall(cl = cl, "require", package = "lme4", character.only = TRUE)
    #junk <- clusterEvalQ(cl = cl, loadNamespace("lme4"))
    if (check.contrasts)  {
      curr.contrasts <- getOption("contrasts")
      clusterExport(cl = cl, "curr.contrasts", envir = sys.nframe())
      junk <- clusterEvalQ(cl = cl, options(contrasts=curr.contrasts))
    }
    if (progress) junk <- clusterEvalQ(cl = cl, cat("["))
    fits <- clusterApplyLB(cl = cl, x = formulas, eval.cl, m.call = mf, progress = progress)
    if (progress) junk <- clusterEvalQ(cl = cl, cat("]"))
  }

  ####################
  ### Part IIb: likelihood checks and refitting
  ####################
  
  check_likelihood <- function(fits) {
    if (type == 3 | type == "III") {
      logLik_full <- as.numeric(logLik(fits[[1]]))
      logLik_restricted <- as.numeric(vapply(fits[2:length(fits)], logLik, 0))
      if(any(logLik_restricted > logLik_full)) return(fixed.effects[logLik_restricted > logLik_full])
    } else if (type == 2 | type == "II") {
      logLik_full <- as.numeric(vapply(fits[1:max.effect.order],logLik, 0))
      logLik_restricted <- as.numeric(vapply(fits[(max.effect.order+1):length(fits)], logLik, 0))
      warn_logLik <- c()
      for (c in seq_along(fixed.effects)) {
        order.c <- effect.order[c]
        if(logLik_restricted[[c]] > logLik_full[[order.c]]) warn_logLik <- c(warn_logLik, fixed.effects[c])
      }
      if(length(warn_logLik)>0) return(warn_logLik)
    }
    return(TRUE)    
  }
  
  # check for smaller likelihood of nested model and refit if test fails:
  if (FALSE) {
    if(!isTRUE(check_likelihood(fits))) {
      if (progress) cat("refitting...")
      refits <- lapply(fits, allFit, verbose=FALSE, data = data)
      browser()
      str(fits[[1]], 2)
      fits[[1]]@call
      #sapply(allFit(fits[[1]], data=md_16.4b), function(y) try(logLik(y)))
      sapply(refits, function(x) sapply(x, function(y) tryCatch(as.numeric(logLik(y)), error = function(e) as.numeric(NA))))
      
      fits <- lapply(refits, function(x) {
        tmp_llk <- vapply(x, function(y) tryCatch(logLik(y), error = function(e) as.numeric(NA)), 0)
        x[[which.min(tmp_llk)]]
      })
    }
  }
  # check again and warn 
  if(!isREML(fits[[1]]) & !isTRUE(check_likelihood(fits))) {
    warning(paste("Following nested model(s) provide better fit than full model:", paste(check_likelihood(fits), collapse = ", "), "\n  It is highly recommended to try different optimizer via lmerControl or allFit!"))
  }
  
  if(set.data.arg){
    for (i in seq_along(fits)) {
      fits[[i]]@call[["data"]] <- mc[["data"]]
    }
  }
  ## prepare for p-values:
  if (type == 3 | type == "III") {
    full.model <- fits[[1]]
    fits <- fits[-1]
  } else if (type == 2 | type == "II") {
    full.model <- fits[1:max.effect.order]
    fits <- fits[(max.effect.order+1):length(fits)]
  }
  names(fits) <- fixed.effects  
  
  ####################
  ### Part III: obtain p-values
  ####################
  ## obtain p-values:
  #browser()
  if (method[1] == "KR") {
    if (progress) cat(str_c("Obtaining ", length(fixed.effects), " p-values:\n["))
    tests <- vector("list", length(fixed.effects))
    for (c in seq_along(fixed.effects)) {
      if (type == 3 | type == "III") tests[[c]] <- KRmodcomp(full.model, fits[[c]])
      else if (type == 2 | type == "II") {
        order.c <- effect.order[c]
        tests[[c]] <- KRmodcomp(full.model[[order.c]], fits[[c]])
      }
      if (progress) cat(".")
    }
    if (progress) cat("]\n")
    names(tests) <- fixed.effects
    #df.out <- data.frame(Effect = fixed.effects, stringsAsFactors = FALSE)
    anova_table <- data.frame(t(vapply(tests, function(x) unlist(x[["test"]][1,]), unlist(tests[[1]][["test"]][1,]))))
    #FtestU <- vapply(tests, function(x) unlist(x[["test"]][2,]), unlist(tests[[1]][["test"]][2,]))
    #row.names(FtestU) <- str_c(row.names(FtestU), ".U")
    #anova_table <- cbind(anova_table, t(FtestU))
    rownames(anova_table) <- fixed.effects
    colnames(anova_table) <- c("F", "num Df", "den Df", "F.scaling", "Pr(>F)")
    anova_table <- anova_table[, c("num Df", "den Df", "F.scaling", "F", "Pr(>F)")]
    anova_tab_addition <- NULL
  } else if (method[1] == "PB") {
    if (progress) cat(str_c("Obtaining ", length(fixed.effects), " p-values:\n["))
    tests <- vector("list", length(fixed.effects))
    for (c in seq_along(fixed.effects)) {
      if (type == 3 | type == "III") tests[[c]] <- do.call(PBmodcomp, args = c(largeModel = full.model, smallModel = fits[[c]], args.test))
      else if (type == 2 | type == "II") {
        order.c <- effect.order[c]
        tests[[c]] <- do.call(PBmodcomp, args = c(largeModel = full.model[[order.c]], smallModel = fits[[c]], args.test))
      }
      if (progress) cat(".")
    }
    if (progress) cat("]\n")
    names(tests) <- fixed.effects
    #browser()
    #df.out<- data.frame(Effect = fixed.effects, stringsAsFactors = FALSE)
    anova_table <- data.frame(t(vapply(tests, function(x) unlist(x[["test"]][2,]), unlist(tests[[1]][["test"]][2,]))))
    anova_table <- anova_table[,-2]
    LRT <- vapply(tests, function(x) unlist(x[["test"]][1,]), unlist(tests[[1]][["test"]][1,]))
    row.names(LRT) <- str_c(row.names(LRT), ".LRT")
    anova_table <- cbind(anova_table, t(LRT))
    rownames(anova_table) <- fixed.effects
    anova_table <- anova_table[, c("stat", "df.LRT", "p.value.LRT", "p.value")]
    colnames(anova_table) <- c("Chisq", "Chi Df", "Pr(>Chisq)", "Pr(>PB)")
    anova_tab_addition <- NULL
  } else if (method[1] == "LRT") {
    tests <- vector("list", length(fixed.effects))
    for (c in seq_along(fixed.effects)) {
      if (type == 3 | type == "III") tests[[c]] <- anova(full.model, fits[[c]])
      else if (type == 2 | type == "II") {
        order.c <- effect.order[c]
        tmpModel  <- full.model[[order.c]] 
        tests[[c]] <- anova(tmpModel, fits[[c]])
      }
    }
    names(tests) <- fixed.effects
    df.large  <- vapply(tests, function(x) x[["Df"]][2], 0)
    df.small  <- vapply(tests, function(x) x[["Df"]][1], 0)
    chisq  <- vapply(tests, function(x) x[["Chisq"]][2], 0)
    df  <- vapply(tests, function(x) x[["Chi Df"]][2], 0)
    p.value  <- vapply(tests, function(x) x[["Pr(>Chisq)"]][2], 0)
    anova_table <- data.frame(Df = df.small, Chisq = chisq, "Chi Df" = df, "Pr(>Chisq)"=p.value, stringsAsFactors = FALSE, check.names = FALSE)
    rownames(anova_table) <- fixed.effects
    if (type == 3 | type == "III") anova_tab_addition <- paste0("Df full model: ", df.large[1])
    else anova_tab_addition <- paste0("Df full model(s): ", df.large)
    
    # 
    # attr(anova_table, "heading") <- c(paste0("Mixed Model Anova Table (Type ", type , " tests)\n"), paste0("Response: ", dv)
    #title <- "Analysis of Variance Table\n"
    #topnote <- paste("Model ", format(1L:nmodels), ": ", variables, 
    #    sep = "", collapse = "\n")
  } else if (method[1] == "F") {
    #browser()
    tests <- vector("list", length(fixed.effects))
    getFvalue <- function(largeModel, smallModel) {
      #browser()
      L  <- .model2restrictionMatrix(largeModel, smallModel)
      #PhiA  <- vcovAdj(largeModel, details = 0)
      PhiA  <- vcov(largeModel)
      beta <- fixef(largeModel)
      betaH <- 0
      betaDiff <- cbind( beta - betaH )
      Wald  <- as.numeric(t(betaDiff) %*% t(L) %*% solve(L%*%PhiA%*%t(L), L%*%betaDiff))
      q <- rankMatrix(L)
      FstatU <- Wald/q      
      list(df1 = q, F = FstatU)
    }
    for (c in seq_along(fixed.effects)) {
      if (type == 3 | type == "III") tests[[c]] <- getFvalue(full.model, fits[[c]])
      else if (type == 2 | type == "II") {
        order.c <- effect.order[c]
        tmpModel  <- full.model[[order.c]] 
        tests[[c]] <- getFvalue(tmpModel, fits[[c]])
      }
    }
    names(tests) <- fixed.effects
    df.out <- data.frame(Effect = fixed.effects, F = vapply(tests, "[[", i = "F", 0), ndf = vapply(tests, "[[", i = "df1", 0), p.value = NA, stringsAsFactors = FALSE)
    rownames(df.out) <- NULL
  } else stop('Only methods "KR", "PB", "LRT" or "F" currently implemented.')
  ####################
  ### Part IV: prepare output
  ####################
  class(anova_table) <- c("anova", "data.frame")
  attr(anova_table, "heading") <- c(
    paste0("Mixed Model Anova Table (Type ", type , " tests)\n"), 
    paste0("Model: ", deparse(formula.f)),
    paste0("Data: " ,mc[["data"]]),
    anova_tab_addition
    )
  list.out <- list(anova_table = anova_table, full.model = full.model, restricted.models = fits, tests = tests) #, type = type, method = method[[1]]
  class(list.out) <- "mixed"
  attr(list.out, "type") <- type
  attr(list.out, "method") <- method
  list.out
}

get_mixed_warnings <- function(x) {
  ntry <- function(x) tryCatch(x, error = function(e) NULL)
  if (is.list(x$full)) {
    warnings1 <- c(full = lapply(x[[2]], function(y) y@optinfo$warnings), lapply(x[[3]], function(y) y@optinfo$warnings))  
    warnings2 <- c(full = lapply(x[[2]], function(y) ntry(y@optinfo$conv$lme4$messages)), lapply(x[[3]], function(y) ntry(y@optinfo$conv$lme4$messages)))
  } else {
    warnings1 <- c(full = list(x$full.model@optinfo$warnings), lapply(x[[3]], function(y) y@optinfo$warnings))  
    warnings2 <- c(full = list(ntry(x$full.model@optinfo$conv$lme4$messages)), lapply(x[[3]], function(y) ntry(y@optinfo$conv$lme4$messages)))
  }
  warnings <- mapply(function(x, y) c(unlist(x), y), warnings1, warnings2, SIMPLIFY=FALSE)  
  warn <- vapply(warnings, function(y) !length(y)==0, NA)
  for (i in names(warn)[warn]) warning("lme4 reported (at least) the following warnings for '", i, "':\n  * ", paste(warnings[[i]], collapse = "\n  * "), call. = FALSE)
}

check_likelihood <- function(object) {
  
   if (is.null(attr(object, "type"))) {
    attr(object, "type") <- object$type
  }
  
  if (attr(object, "type") == 3 | attr(object, "type") == "III") {
    logLik_full <- as.numeric(logLik(object[["full.model"]]))
    logLik_restricted <- as.numeric(vapply(object[["restricted.models"]], logLik, 0))
    if(any(logLik_restricted > logLik_full)) return(rownames(object$anova_table)[logLik_restricted > logLik_full])
  } else if (attr(object, "type") == 2 | attr(object, "type") == "II") {
    NULL
#     logLik_full <- as.numeric(vapply(fits[1:max.effect.order],logLik, 0))
#     logLik_restricted <- as.numeric(vapply(fits[(max.effect.order+1):length(fits)], logLik, 0))
#     warn_logLik <- c()
#     for (c in seq_along(fixed.effects)) {
#       order.c <- effect.order[c]
#       if(logLik_restricted[[c]] > logLik_full[[order.c]]) warn_logLik <- c(warn_logLik, fixed.effects[c])
#     }
#     if(length(warn_logLik)>0) return(warn_logLik)
  }
  return(TRUE)    
}


#' @rdname mixed
#' @export
lmer_alt <- function(formula, data, check.contrasts = FALSE, ...) {
  mc <- match.call()
  #assign(all.vars(mc[["data"]]), data)
  mc[[1]] <- as.name("mixed")
  mc[["return"]] <- "merMod"
  mc[["expand_re"]] <- TRUE
  mc[["progress"]] <- FALSE
  mc[["check.contrasts"]] <- check.contrasts
  #browser()
  eval(mc)
}

#' @method print mixed
#' @export
print.mixed <- function(x, ...) {
  if(!isREML(x[["full.model"]]) && !isTRUE(check_likelihood(x))) 
    warning(paste("Following nested model(s) provide better fit than full model:", paste(check_likelihood(x), collapse = ", "), "\n  It is highly recommended to try different optimizer via lmerControl or allFit!"), call. = FALSE)
  get_mixed_warnings(x)
  tmp <- nice.mixed(x, ...)
  print(tmp)
  invisible(tmp)
}


#anova.mixed <- 

#' @method summary mixed
#' @export
summary.mixed <- function(object, ...) summary(object = if (length(object[["full.model"]]) == 1) object[["full.model"]] else object[["full.model"]][[length(object[["full.model"]])]], ...)

#' @method anova mixed
#' @export
anova.mixed <- function(object, ..., refit = FALSE) {
  mCall <- match.call(expand.dots = TRUE)
  dots <- list(...)
  modp <- (as.logical(vapply(dots, is, NA, "merMod")) |
             as.logical(vapply(dots, is, NA, "lm")) |
             as.logical(vapply(dots, is, NA, "mixed"))
           )
  if (any(modp)) {
    model.names <- c(deparse(mCall[["object"]]), vapply(which(modp), function(x) deparse(mCall[[x+2]]), ""))
    for (i in which(as.logical(vapply(dots, is, NA, "mixed")))) dots[[i]] <- dots[[i]]$full.model
    return(do.call(anova, args = c(object = object, dots, model.names = list(model.names), refit = refit)))
  } else {
    if(!isREML(object[["full.model"]]) && !isTRUE(check_likelihood(object))) 
      warning(paste("Following nested model(s) provide better fit than full model:", paste(check_likelihood(object), collapse = ", "), "\n  It is highly recommended to try different optimizer via lmerControl or allFit!"), call. = FALSE)
    get_mixed_warnings(object)
    object$anova_table
  }
}




## support for lsmeans for mixed objects:
#' @importFrom lsmeans recover.data lsm.basis
#' @method recover.data mixed
#' @export
recover.data.mixed <- function(object, ...) {
  recover.data(object$full.model, ...)
}


#' @method lsm.basis mixed 
#' @export
lsm.basis.mixed <- function(object, trms, xlev, grid, ...) {
  lsm.basis(object$full.model, trms, xlev, grid, ...)
}






### old stuff (actually not usable right now)
# is.mixed <- function(x) inherits(x, "mixed")

## some code copied from pbkrtest.

.restrictionMatrixBA<-function(B,A) {
  ## <A> in <B>
  ## determine L such that  <A>={Bb| b in Lb=0}
  d <- rankMatrix(cbind(A,B)) - rankMatrix(B)
  if (d > 0) {
    stop('Error:  <A> not subspace of <B> \n')
  }
  Q  <- qr.Q(qr(cbind(A,B)))
  Q2 <- Q[,(rankMatrix(A)+1):rankMatrix(B)] 
  L  <- t(Q2)  %*% B
  ##make rows of L2 orthogonal
  L <-t(qr.Q(qr(t(L))))
  L
}

.model2restrictionMatrix <- function (largeModel, smallModel) {
  L <- if(is.matrix(smallModel)) {
    ## ensures  that L is of full row rank:
    LL <- smallModel
    q  <- rankMatrix(LL)
    if (q < nrow(LL) ){
      t(qr.Q(qr(t(LL)))[,1:qr(LL)$rank])
    } else {
      smallModel
    }
  } else  { #smallModel is mer model
    .restrictionMatrixBA(getME(largeModel,'X'),getME(smallModel,'X'))
  }
  L<-.makeSparse(L)
  L
}

.makeSparse<-function(X) {
  X<-as.matrix(X)
  w<-cbind(c(row(X)),c(col(X)),c(X))
  w<-w[abs(w[,3])>1e-16,,drop=FALSE]
  Y<-sparseMatrix(w[,1],w[,2],x=w[,3],dims=dim(X))
}
