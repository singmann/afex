#' p-values for fixed effects of mixed-model via lme4::lmer()
#'
#' @description Estimates mixed models with \pkg{lme4} and calculates p-values
#'   for all fixed effects. The default method \code{"KR"} (= Kenward-Roger) as
#'   well as  \code{method="S"} (Satterthwaite) support LMMs and estimate the
#'   model with \code{\link[lmerTest]{lmer}} and then pass it to the
#'   \code{\link[lmerTest]{lmerTest}} \code{anova} method (or
#'   \code{\link[car]{Anova}}). The other methods (\code{"LRT"} =
#'   likelihood-ratio tests and \code{"PB"} = parametric bootstrap) support both
#'   LMMs (estimated via \code{\link[lme4]{lmer}}) and GLMMs (i.e., with
#'   \code{family} argument which invokes estimation via
#'   \code{\link[lme4]{glmer}}) and estimate a full model and restricted models
#'   in which the parameters corresponding to one effect (i.e., model term) are
#'   withhold (i.e., fixed to 0). Per default tests are based on Type 3 sums of
#'   squares. \code{print}, \code{nice}, \code{anova}, and \code{summary}
#'   methods for the returned object of class \code{"mixed"} are available.
#'   \code{summary} invokes the default \pkg{lme4} summary method and shows
#'   parameters instead of effects.
#'
#'   \code{lmer_alt} is simply a wrapper for mixed that only returns the
#'   \code{"lmerModLmerTest"} or \code{"merMod"} object and correctly uses the
#'   \code{||} notation for removing correlations among factors. This function
#'   otherwise behaves like \code{g/lmer} (as for \code{mixed}, it calls
#'   \code{glmer} as soon as a \code{family} argument is present). Use
#'   \code{\link{afex_options}}\code{("lmer_function")} to set which function
#'   for estimation should be used. This option determines the class of the
#'   returned object (i.e., \code{"lmerModLmerTest"} or \code{"merMod"}).
#'
#'
#' @param formula a formula describing the full mixed-model to be fitted. As
#'   this formula is passed to \code{lmer}, it needs at least one random term.
#' @param data \code{data.frame} containing the data. Should have all the
#'   variables present in \code{fixed}, \code{random}, and \code{dv} as columns.
#' @param type type of test on which effects are based. Default is to use type 3
#'   tests, taken from \code{\link{afex_options}}.
#' @param method character vector indicating which methods for obtaining
#'   p-values should be used: \code{"S"} corresponds to the Satterthwaite
#'   approximation for degrees of freedom (via \code{\link[lmerTest]{lmerTest}},
#'   only LMMs), \code{"KR"} corresponds to the Kenward-Roger approximation for
#'   degrees of freedom (only LMMs), \code{"PB"} calculates p-values based on
#'   parametric bootstrap, \code{"LRT"} calculates p-values via the likelihood
#'   ratio tests implemented in the \code{anova} method for \code{merMod}
#'   objects (only recommended for models with many [i.e., > 50] levels for the
#'   random factors). The default (currently \code{"S"}) is taken from
#'   \code{\link{afex_options}}. For historical compatibility \code{"nested-KR"}
#'   is also supported which was the default KR-method in previous versions.
#' @param per_parameter \code{character} vector specifying for which variable
#'   tests should be run for each parameter (instead for the overall effect).
#'   Can be useful e.g., for testing ordered factors. Uses \code{\link{grep}}
#'   for selecting parameters among the fixed effects so regular expressions
#'   (\code{\link{regex}}) are possible. See Examples.
#' @param args_test \code{list} of arguments passed to the function calculating
#'   the p-values. See Details.
#' @param test_intercept logical. Whether or not the intercept should also be
#'   fitted and tested for significance. Default is \code{FALSE}. Only relevant
#'   if \code{type = 3}.
#' @param check_contrasts \code{logical}. Should contrasts be checked and (if
#'   necessary) changed to \code{"contr.sum"}? See Details. The default
#'   (\code{"TRUE"}) is taken from \code{\link{afex_options}}.
#' @param expand_re logical. Should random effects terms be expanded (i.e.,
#'   factors transformed into numerical variables) before fitting with
#'   \code{(g)lmer}? Allows to use "||" notation with factors.
#' @param all_fit logical. Should \code{\link[lme4]{allFit}} be used to fit each
#'   model with each available optimization algorithm and the results that
#'   provided the best fit in each case be used? Warning: This can dramatically
#'   increase the optimization time. Adds two new attributes to the returned
#'   object designating which algorithm was selected and the log-likelihoods for
#'   each algorithm. Note that only warnings from the initial fit are emitted
#'   during fitting. The warnings of the chosen models are emitted when printing
#'   the returned object.
#' @param set_data_arg \code{logical}. Should the data argument in the slot
#'   \code{call} of the \code{merMod} object returned from \code{lmer} be set to
#'   the passed data argument? If \code{FALSE} (currently the default) the name
#'   will be \code{data}. \code{TRUE} may be helpful when fitted objects are
#'   used afterwards (e.g., compared using \code{anova} or when using the
#'   \code{effects} package, see examples). \pkg{emmeans} functions appear to
#'   work better with \code{FALSE}. Default is given by
#'   afex_options("set_data_arg").
#' @param progress  if \code{TRUE}, shows progress with a text progress bar and
#'   other status messages during estimation. The default is to set \code{TRUE}
#'   for interactive usage and \code{FALSE} for non-interactive usage.
#' @param cl  A vector identifying a cluster; used for distributing the
#'   estimation of the different models using several cores (if seveal models
#'   are calculated). See examples. If \code{ckeck_contrasts = TRUE}, mixed sets
#'   the current contrasts (\code{getOption("contrasts")}) at the nodes. Note
#'   this does \emph{not} distribute calculation of p-values (e.g., when using
#'   \code{method = "PB"}) across the cluster. Use \code{args_test} for this.
#' @param return the default is to return an object of class \code{"mixed"}.
#'   \code{return = "merMod"} will skip the calculation of all submodels and
#'   p-values and simply return the full model estimated with \code{lmer} (note
#'   that somewhat unintuiviely, the returned object can either be of class
#'   \code{"lmerModLmerTest"} or of class \code{"merMod"}, depending on the
#'   value of \code{\link{afex_options}}\code{("lmer_function")}). Can be useful
#'   in combination with \code{expand_re = TRUE} which allows to use "||" with
#'   factors. \code{return = "data"} will not fit any models but just return the
#'   data that would have been used for estimating the model (note that the data
#'   is also part of the returned object).
#' @param sig_symbols Character. What should be the symbols designating
#'   significance? When entering an vector with \code{length(sig.symbol) < 4}
#'   only those elements of the default (\code{c(" +", " *", " **", " ***")})
#'   will be replaced. \code{sig_symbols = ""} will display the stars but not
#'   the \code{+}, \code{sig_symbols = rep("", 4)} will display no symbols. The
#'   default is given by \code{afex_options("sig_symbols")}.
#' @param ... further arguments (such as \code{weights}, \code{family}, or
#'   \code{control}) passed to
#'   \code{\link[lme4]{lmer}}/\code{\link[lme4]{glmer}}. Note that additional
#'   data (e.g., \code{weights}) need to be passed fully and not only
#'   by name (e.g., \code{weights = df$weights} and not \code{weights =
#'   weights}).
#'
#'
#' @return An object of class \code{"mixed"} (i.e., a list) with the following
#'   elements:
#'
#' \enumerate{
#'   \item \code{anova_table} a data.frame containing the statistics returned
#'     from \code{\link[pbkrtest]{KRmodcomp}}. The \code{stat} column in this
#'     data.frame gives the value of the test statistic, an F-value for
#'     \code{method = "KR"} and a chi-square value for the other two methods.
#'   \item \code{full_model} the \code{"lmerModLmerTest"} or \code{"merMod"}
#'     object returned from estimating the full model. Use
#'     \code{\link{afex_options}}\code{("lmer_function")} for setting which
#'     function for estimation should be used. The possible options are
#'     \code{"lmerTest"} (the default returning an object of class
#'     \code{"lmerModLmerTest"}) and \code{"lme4"} returning an object of class
#'     (\code{"merMod"}). Note that in case a \code{family} argument is present
#'     an object of class \code{"glmerMod"} is always returned.
#'   \item \code{restricted_models} a list of \code{"g/lmerMod"} (or
#'     \code{"lmerModLmerTest"}) objects from estimating the restricted models
#'     (i.e., each model lacks the corresponding effect)
#'   \item \code{tests} a list of objects returned by the function for
#'     obtaining the p-values.
#'   \item \code{data} The data used for estimation (i.e., after excluding
#'     missing rows and applying expand_re if requested).
#'   \item \code{call} The matched call.
#' }
#'
#' It also has the following attributes, \code{"type"} and \code{"method"}. And
#' the attributes \code{"all_fit_selected"} and \code{"all_fit_logLik"} if
#' \code{all_fit=TRUE}.
#'
#' Two similar methods exist for objects of class \code{"mixed"}: \code{print}
#' and \code{anova}. They print a nice version of the \code{anova_table} element
#' of the returned object (which is also invisibly returned). This methods omit
#' some columns and nicely round the other columns. The following columns are
#' always printed:
#' \enumerate{
#'   \item \code{Effect} name of effect
#'   \item \code{p.value} estimated p-value for the effect
#' }
#'
#' For LMMs with \code{method="KR"} or \code{method="S"} the following further
#' columns are returned (note: the Kenward-Roger correction does two separate
#' things: (1) it computes an effective number for the denominator df; (2) it
#' scales the statistic by a calculated amount, see also
#' \url{https://stackoverflow.com/a/25612960/289572}):
#' \enumerate{
#'   \item \code{F} computed F statistic
#'   \item \code{ndf} numerator degrees of freedom (number of parameters used
#'     for the effect)
#'   \item \code{ddf} denominator degrees of freedom (effective residual
#'     degrees of freedom for testing the effect), computed from the
#'     Kenward-Roger correction using \code{pbkrtest::KRmodcomp}
#'   \item \code{F.scaling} scaling of F-statistic computing from Kenward-Roger
#'     approximation (only printed if \code{method="nested-KR"})
#' }
#'
#' For models with \code{method="LRT"} the following further columns are
#' returned:
#' \enumerate{
#'   \item \code{df.large} degrees of freedom (i.e., estimated paramaters) for
#'     full model (i.e., model containing the corresponding effect)
#'   \item \code{df.small} degrees of freedom (i.e., estimated paramaters) for
#'     restricted model (i.e., model without the corresponding effect)
#'   \item \code{chisq} 2 times the difference in likelihood (obtained with
#'     \code{logLik}) between full and restricted model
#'   \item \code{df} difference in degrees of freedom between full and
#'     restricted model (p-value is based on these df).
#' }
#'
#' For models with \code{method="PB"} the following further column is returned:
#' \enumerate{
#'   \item \code{stat} 2 times the difference in likelihood (obtained with
#'     \code{logLik}) between full and restricted model (i.e., a chi-square
#'     value).
#' }
#'
#' Note that  \code{anova} can also be called with additional mixed and/or
#' \code{merMod} objects. In this casethe full models are passed on to
#' \code{anova.merMod} (with \code{refit=FALSE}, which differs from the default
#' of \code{anova.merMod}) which produces the known LRT tables.
#'
#' The \code{summary} method for objects of class \code{mixed} simply calls
#' \code{\link[lme4]{summary.merMod}} on the full model.
#'
#' If \code{return = "merMod"} (or when invoking \code{lmer_alt}), an object of
#' class \code{"lmerModLmerTest"} or of class \code{"merMod"} (depending on the
#' value of \code{\link{afex_options}}\code{("lmer_function")}), as returned
#' from \code{g/lmer}, is returned. The default behavior is to return an object
#' of class \code{"lmerModLmerTest"} estimated via \code{\link[lmerTest]{lmer}}.
#'
#'@details For an introduction to mixed-modeling for experimental designs see
#'  our chapter
#'  (\href{http://singmann.org/download/publications/singmann_kellen-introduction-mixed-models.pdf}{Singmann
#'   & Kellen, in press}) or Barr, Levy, Scheepers, & Tily (2013). Arguments for
#'  using the Kenward-Roger approximation for obtaining p-values are given by
#'  Judd, Westfall, and Kenny (2012). Further introductions to mixed-modeling
#'  for experimental designs are given by Baayen and colleagues (Baayen, 2008;
#'  Baayen, Davidson & Bates, 2008; Baayen & Milin, 2010). Specific
#'  recommendations on which random effects structure to specify for
#'  confirmatory tests can be found in Barr and colleagues (2013) and Barr
#'  (2013), but also see Bates et al. (2015).
#'
#'  \subsection{p-value Calculations}{
#'
#'  When \code{method = "KR"} (implemented via
#'  \code{\link[pbkrtest]{KRmodcomp}}), the Kenward-Roger approximation for
#'  degrees-of-freedom is calculated using \code{\link[lmerTest]{lmerTest}} (if
#'  \code{test_intercept=FALSE}) or \code{\link[car]{Anova}} (if
#'  \code{test_intercept=TRUE}), which is only applicable to linear-mixed models
#'  (LMMs). The test statistic in the output is an F-value (\code{F}). A similar
#'  method that requires less RAM is \code{method = "S"} which calculates the
#'  Satterthwaite approximation for degrees-of-freedom via
#'  \code{\link[lmerTest]{lmerTest}} and is also only applicable to LMMs.
#'  \code{method = "KR"} or \code{method = "S"} provide the best control for
#'  Type 1 errors for LMMs (Luke, 2017).
#'
#'  \code{method = "PB"} calculates p-values using parametric bootstrap using
#'  \code{\link[pbkrtest]{PBmodcomp}}. This can be used for linear and also
#'  generalized linear mixed models (GLMMs) by specifying a
#'  \code{\link[stats]{family}} argument to \code{mixed}. Note that you should
#'  specify further arguments to \code{PBmodcomp} via \code{args_test},
#'  especially \code{nsim} (the number of simulations to form the reference
#'  distribution) or \code{cl} (for using multiple cores). For other arguments
#'  see \code{\link[pbkrtest]{PBmodcomp}}. Note that \code{REML} (argument to
#'  \code{[g]lmer}) will be set to \code{FALSE} if method is \code{PB}.
#'
#'  \code{method = "LRT"} calculates p-values via likelihood ratio tests
#'  implemented in the \code{anova} method for \code{"merMod"} objects. This is
#'  the method recommended by Barr et al. (2013; which did not test the other
#'  methods implemented here). Using likelihood ratio tests is only recommended
#'  for models with many levels for the random effects (> 50), but can be pretty
#'  helpful in case the other methods fail (due to memory and/or time
#'  limitations). The
#'  \href{http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html}{lme4 faq} also
#'  recommends the other methods over likelihood ratio tests. }
#'
#'  \subsection{Implementation Details}{
#'
#'  For methods \code{"KR"} and \code{"S"} type 3 and 2 tests are implemented as
#'  in \code{\link[car]{Anova}}.
#'
#'  For all other methods, type 3 tests are obtained by comparing a model in
#'  which only the tested effect is excluded with the full model (containing all
#'  effects). For method \code{"nested-KR"} (which was the default in previous
#'  versions) this corresponds to the (type 3) Wald tests given by
#'  \code{car::Anova} for \code{"lmerMod"} models. The submodels in which the
#'  tested effect is excluded are obtained by manually creating a model matrix
#'  which is then fitted in \code{"lme4"}.
#'
#'  Type 2 tests are truly sequential. They are obtained by comparing a model in
#'  which the tested effect and all higher oder effect (e.g., all three-way
#'  interactions for testing a two-way interaction) are excluded with a model in
#'  which only effects up to the order of the tested effect are present and all
#'  higher order effects absent. In other words, there are multiple full models,
#'  one for each order of effects. Consequently, the results for lower order
#'  effects are identical of whether or not higher order effects are part of the
#'  model or not. This latter feature is not consistent with classical ANOVA
#'  type 2 tests but a consequence of the sequential tests (and
#'  \href{https://stat.ethz.ch/pipermail/r-sig-mixed-models/2012q3/018992.html}{I
#'  didn't find a better way} of implementing the Type 2 tests). This
#'  \strong{does not} correspond to the (type 2) Wald test reported by
#'  \code{car::Anova}.
#'
#'  If \code{check_contrasts = TRUE}, contrasts will be set to
#'  \code{"contr.sum"} for all factors in the formula if default contrasts are
#'  not equal to \code{"contr.sum"} or \code{attrib(factor, "contrasts") !=
#'  "contr.sum"}. Furthermore, the current contrasts (obtained via
#'  \code{getOption("contrasts")}) will be set at the cluster nodes if \code{cl}
#'  is not \code{NULL}. }
#'
#'  \subsection{Expand Random Effects}{ \code{expand_re = TRUE} allows to expand
#'  the random effects structure before passing it to \code{lmer}. This allows
#'  to disable estimation of correlation among random effects for random effects
#'  term containing factors using the \code{||} notation which may aid in
#'  achieving model convergence (see Bates et al., 2015). This is achieved by
#'  first creating a model matrix for each random effects term individually,
#'  rename and append the so created columns to the data that will be fitted,
#'  replace the actual random effects term with the so created variables
#'  (concatenated with +), and then fit the model. The variables are renamed by
#'  prepending all variables with rei (where i is the number of the random
#'  effects term) and replacing ":" with "_by_".
#'
#'  \code{lmer_alt} is simply a wrapper for \code{mixed} that is intended to
#'  behave like \code{lmer} (or \code{glmer} if a \code{family} argument is
#'  present), but also allows the use of \code{||} with factors (by always using
#'  \code{expand_re = TRUE}). This means that \code{lmer_alt} per default does
#'  not enforce a specific contrast on factors and only returns the
#'  \code{"lmerModLmerTest"} or \code{"merMod"} object without calculating any
#'  additional models or p-values (this is achieved by setting \code{return =
#'  "merMod"}). Note that it most likely differs from \code{g/lmer} in how it
#'  handles missing values so it is recommended to only pass data without
#'  missing values to it!
#'
#'  One consequence of using \code{expand_re = TRUE} is that the data that is
#'  fitted will not be the same as the passed data.frame which can lead to
#'  problems with e.g., the \code{predict} method. However, the actual data used
#'  for fitting is also returned as part of the \code{mixed} object so can be
#'  used from there. Note that the \code{set_data_arg} can be used to change
#'  whether the \code{data} argument in the call to \code{g/lmer} is set to
#'  \code{data} (the default) or the name of the data argument passed by the
#'  user. }
#'
#' @note When \code{method = "KR"}, obtaining p-values is known to crash due too
#'   insufficient memory or other computational limitations (especially with
#'   complex random effects structures). In these cases, the other methods
#'   should be used. The RAM demand is a problem especially on 32 bit Windows
#'   which only supports up to 2 or 3GB RAM (see
#'   \href{https://CRAN.R-project.org/bin/windows/base/rw-FAQ.html}{R Windows
#'   FAQ}). Then it is probably a good idea to use methods "S", "LRT", or "PB".
#'
#'   \code{"mixed"} will throw a message if numerical variables are not centered
#'   on 0, as main effects (of other variables then the numeric one) can be hard
#'   to interpret if numerical variables appear in interactions. See Dalal &
#'   Zickar (2012).
#'
#'   Per default \code{mixed} uses \code{\link[lmerTest]{lmer}}, this can be
#'   changed to \code{\link[lme4]{lmer}} by calling:
#'   \code{afex_options(lmer_function = "lme4")}
#'
#'   Formulas longer than 500 characters will most likely fail due to the use of
#'   \code{\link{deparse}}.
#'
#'   Please report bugs or unexpected behavior by opening a guthub issue:
#'   \url{https://github.com/singmann/afex/issues}
#'
#' @author Henrik Singmann with contributions from
#'   \href{https://stackoverflow.com/q/11335923/289572}{Ben Bolker and Joshua
#'   Wiley}.
#'
#' @seealso \code{\link{aov_ez}} and \code{\link{aov_car}} for convenience
#'   functions to analyze experimental desIgns with classical ANOVA or ANCOVA
#'   wrapping \code{\link[car]{Anova}}.
#'
#'   see the following for the data sets from Maxwell and Delaney (2004) used
#'   and more examples: \code{\link{md_15.1}}, \code{\link{md_16.1}}, and
#'   \code{\link{md_16.4}}.
#'
#' @references Baayen, R. H. (2008). \emph{Analyzing linguistic data: a
#'   practical introduction to statistics using R}. Cambridge, UK; New York:
#'   Cambridge University Press.
#'
#'   Baayen, R. H., Davidson, D. J., & Bates, D. M. (2008). Mixed-effects
#'   modeling with crossed random effects for subjects and items. \emph{Journal
#'   of Memory and Language}, 59(4), 390-412. \doi{10.1016/j.jml.2007.12.005}
#'
#'   Baayen, R. H., & Milin, P. (2010). Analyzing Reaction Times.
#'   \emph{International Journal of Psychological Research}, 3(2), 12-28.
#'
#'   Barr, D. J. (2013). Random effects structure for testing interactions in
#'   linear mixed-effects models. \emph{Frontiers in Quantitative Psychology and
#'   Measurement}, 328. \doi{10.3389/fpsyg.2013.00328}
#'
#'   Barr, D. J., Levy, R., Scheepers, C., & Tily, H. J. (2013). Random effects
#'   structure for confirmatory hypothesis testing: Keep it maximal.
#'   \emph{Journal of Memory and Language}, 68(3), 255-278.
#'   \doi{10.1016/j.jml.2012.11.001}
#'
#'   Bates, D., Kliegl, R., Vasishth, S., & Baayen, H. (2015).
#'   \emph{Parsimonious Mixed Models}. arXiv:1506.04967 [stat]. Retrieved from
#'   \url{https://arxiv.org/abs/1506.04967}
#'
#'   Dalal, D. K., & Zickar, M. J. (2012). Some Common Myths About Centering
#'   Predictor Variables in Moderated Multiple Regression and Polynomial
#'   Regression. \emph{Organizational Research Methods}, 15(3), 339-362.
#'   \doi{10.1177/1094428111430540}
#'
#'   Judd, C. M., Westfall, J., & Kenny, D. A. (2012). Treating stimuli as a
#'   random factor in social psychology: A new and comprehensive solution to a
#'   pervasive but largely ignored problem. \emph{Journal of Personality and
#'   Social Psychology}, 103(1), 54-69. \doi{10.1037/a0028347}
#'
#'   Luke, S. (2017). Evaluating significance in linear mixed-effects models in
#'   R. \emph{Behavior Research Methods}.
#'   \doi{10.3758/s13428-016-0809-y}
#'
#'   Maxwell, S. E., & Delaney, H. D. (2004). \emph{Designing experiments and
#'   analyzing data: a model-comparisons perspective.} Mahwah, N.J.: Lawrence
#'   Erlbaum Associates.
#'
#'
## @import pbkrtest
#' @importFrom lme4 glmer getME isREML
#' @importFrom reformulas nobars
#' @importFrom parallel clusterCall clusterExport clusterEvalQ clusterApplyLB
#' @importFrom stats logLik terms as.formula contrasts<- model.matrix model.frame anova
#' @importFrom methods is
#' @encoding UTF-8
#'
#' @example examples/examples.mixed.R
#'
#' @export
mixed <- function(
  formula,
  data,
  type = afex_options("type"),
  method = afex_options("method_mixed"),
  per_parameter = NULL,
  args_test = list(),
  test_intercept = FALSE,
  check_contrasts = afex_options("check_contrasts"),
  expand_re = FALSE,
  all_fit = FALSE,
  set_data_arg = afex_options("set_data_arg"),
  progress = interactive(),
  cl = NULL,
  return = "mixed",
  sig_symbols = afex_options("sig_symbols"),
  ...
) {
  dots <- list(...)
  data <- as.data.frame(data) # adding droplevels() here seems to lead to problems
  # with factors that have contrasts associated with it.

  ### deprercate old argument names:
  if ("per.parameter" %in% names(dots)) {
    warn_deprecated_arg("per.parameter", "per_parameter")
    per_parameter <- dots$per.parameter
  }
  if ("args.test" %in% names(dots)) {
    warn_deprecated_arg("args.test", "args_test")
    args_test <- dots$args.test
  }
  if ("test.intercept" %in% names(dots)) {
    warn_deprecated_arg("test.intercept", "test_intercept")
    test_intercept <- dots$test.intercept
  }
  if ("check.contrasts" %in% names(dots)) {
    warn_deprecated_arg("check.contrasts", "check_contrasts")
    check_contrasts <- dots$check.contrasts
  }
  if ("set.data.arg" %in% names(dots)) {
    warn_deprecated_arg("set.data.arg", "set_data_arg")
    set_data_arg <- dots$set.data.arg
  }
  if ("sig.symbols" %in% names(dots)) {
    #(!missing(sig.symbols)) {
    warn_deprecated_arg("sig.symbols", "sig_symbols")
    sig_symbols <- dots$sig.symbols
  }

  ## real function begins:
  vars.to.check <- all.vars(as.formula(formula))
  data <- check_contrasts(
    data = data,
    factors = vars.to.check,
    check_contrasts = check_contrasts,
    type = type,
    warn = FALSE
  )

  method <- match.arg(
    method,
    c("KR", "S", "PB", "LRT", "nested-KR", "F"),
    several.ok = FALSE
  )

  ####################
  ### Part I: prepare fitting (i.e., obtain model info, check model, ...)
  ####################
  mc <- match.call()
  formula.f <- as.formula(formula)
  if (!inherits(formula, "formula")) {
    message("Formula (the first argument) converted to formula.")
  }
  dv <- as.character(formula.f)[[2]]
  all.terms <- attr(terms(formula.f), "term.labels")
  effect.order <- attr(terms(formula.f), "order")
  effect.order <- effect.order[!grepl("\\|", all.terms)]
  max.effect.order <- max(effect.order)
  random <- paste0(
    paste0("(", all.terms[grepl("\\|", all.terms)], ")"),
    collapse = " + "
  )
  rh2 <- nobars(formula.f)
  rh2[[2]] <- NULL
  m.matrix <- model.matrix(rh2, data = data)
  fixed.effects <- attr(terms(rh2, data = data), "term.labels")
  mapping <- attr(m.matrix, "assign")
  fixed.vars <- all.vars(rh2)
  # check for missing values in variables used:
  if (nrow(model.matrix(nobars(formula.f), data = data)) != nrow(data)) {
    data <- model.frame(
      as.formula(paste0(
        vars.to.check[1],
        "~",
        paste0(vars.to.check[-1], collapse = "+")
      )),
      data = data
    )
    m.matrix <- model.matrix(rh2, data = data)
    warning(
      paste0(
        "Due to missing values, reduced number of observations to ",
        nrow(data)
      ),
      call. = FALSE
    )
    if (set_data_arg) {
      warning(
        "Due to missing values, set_data_arg set to FALSE.",
        call. = FALSE
      )
      set_data_arg <- FALSE
    }
  }

  # check if numerical variables are centered
  c.ns <- fixed.vars[vapply(data[, fixed.vars, drop = FALSE], is.numeric, TRUE)]
  if (length(c.ns) > 0) {
    non.null <- c.ns[
      !abs(vapply(data[, c.ns, drop = FALSE], mean, 0)) <
        .Machine$double.eps^0.5
    ]
    if (length(non.null) > 0) {
      message(paste0(
        "Numerical variables NOT centered on 0: ",
        paste0(non.null, collapse = ", "),
        "\nIf in interactions, interpretation of lower order",
        " (e.g., main) effects difficult."
      ))
    }
  }
  if (expand_re) {
    expand_re_out <- expand_re_fun(all.terms = all.terms, data = data)
    data <- expand_re_out$data
    random <- expand_re_out$random
  }
  if (return == "data") {
    return(data)
  }
  ####################
  ### Part II: obtain the lmer fits
  ####################
  ## Part IIa: prepare formulas
  mf <- mc[
    !names(mc) %in%
      c(
        "type",
        "method",
        "args.test",
        "args_test",
        "progress",
        "check.contrasts",
        "check_contrasts",
        "per.parameter",
        "per_parameter",
        "cl",
        "test.intercept",
        "test_intercept",
        "expand_re",
        "return",
        "all_fit",
        "sig_symbols",
        "sig.symbols",
        "set_data_arg"
      )
  ]
  mf[["formula"]] <-
    as.formula(paste0(dv, deparse(rh2, width.cutoff = 500L), "+", random))
  if ("family" %in% names(mf)) {
    mf[[1]] <- as.name("glmer")
    use_reml <- FALSE
  } else {
    if (afex_options("lmer_function") == "lmerTest") {
      mf[[1]] <- quote(lmerTest::lmer)
    } else if (afex_options("lmer_function") == "lme4") {
      if (!(return %in% c("merMod")) && method %in% c("KR", "S")) {
        stop(
          'afex_options("lmer_function") needs to be "lmerTest" for method="',
          method,
          '"',
          call. = FALSE
        )
      }
      mf[[1]] <- quote(lme4::lmer)
    } else {
      stop(
        "value of afex_options('lmer_function') not supported.",
        call. = FALSE
      )
    }
    use_reml <- TRUE
  }
  mf[["data"]] <- as.name("data")
  if ((method[1] %in% c("PB", "LRT")) & !("family" %in% names(mf))) {
    if ((!"REML" %in% names(mf)) || mf[["REML"]]) {
      message("REML argument to lmer() set to FALSE for method = 'PB' or 'LRT'")
      mf[["REML"]] <- FALSE
      use_reml <- FALSE
    }
  }
  if (return == "merMod") {
    out <- eval(mf)
    if (set_data_arg) {
      out@call[["data"]] <- mc[["data"]]
    }
    return(out)
  }
  if ("family" %in% names(mf)) {
    if (!(method[1] %in% c("LRT", "PB"))) {
      stop("GLMMs can only be estimated with 'LRT' or 'PB'.", call. = FALSE)
    }
  }
  ## do not calculate nested models for these methods:
  if (method[1] %in% c("KR", "S")) {
    if (progress) {
      cat("Fitting one lmer() model. ")
    }
    full_model <- eval(mf)
    if (all_fit) {
      all_fits <- suppressWarnings(lme4::allFit(
        full_model,
        data = data,
        verbose = FALSE
      ))
      all_fits <- c(default = full_model, all_fits)
      tmp_ll <-
        vapply(
          all_fits,
          function(x) tryCatch(logLik(x), error = function(e) NA),
          0
        )
      full_model <- all_fits[[which.max(tmp_ll)]]
      full_model@optinfo$logLik_other <- tmp_ll
    }
    fits <- NULL
    tests <- NULL
    anova_tab_addition <- NULL
    if (progress) {
      cat("[DONE]\nCalculating p-values. ")
    }
    if (method[1] == "KR") {
      #lmerTest_method <- if(method[1] == "KR") "Kenward-Roger" else "Satterthwaite"
      if (test_intercept) {
        anova_out <- car::Anova(full_model, type = type, test.statistic = "F")
        anova_table <- as.data.frame(anova_out)
        anova_table <- anova_table[, c("Df", "Df.res", "F", "Pr(>F)")]
        colnames(anova_table) <- c("num Df", "den Df", "F", "Pr(>F)")
      } else {
        anova_out <- lmerTest_anova(
          full_model,
          ddf = "Kenward-Roger",
          type = type
        )
        anova_table <- as.data.frame(anova_out)
        get <- c("NumDF", "DenDF", "F.value", "F value", "Pr(>F)")
        anova_table <- anova_table[, match(
          get,
          colnames(anova_table),
          nomatch = 0L
        )]
        colnames(anova_table) <- c("num Df", "den Df", "F", "Pr(>F)")
      }
    } else if (method[1] == "S") {
      if (test_intercept) {
        warning("Cannot test intercept with Satterthwaite approximation.")
      }
      anova_out <- lmerTest_anova(
        full_model,
        ddf = "Satterthwaite",
        type = type
      )
      anova_table <- as.data.frame(anova_out)
      if (!("Pr(>F)" %in% colnames(anova_table))) {
        colnames(anova_table)[c(1, 4)] <- c("NumDF", "F.value")
        anova_table$DenDF <- NA_real_
        anova_table$`Pr(>F)` <- NA_real_
      }
      get <- c("NumDF", "DenDF", "F.value", "F value", "Pr(>F)")
      anova_table <- anova_table[, match(
        get,
        colnames(anova_table),
        nomatch = 0L
      )]
      colnames(anova_table) <- c("num Df", "den Df", "F", "Pr(>F)")
    }
    if (progress) {
      cat("[DONE]\n")
    }
    if (set_data_arg) full_model@call[["data"]] <- mc[["data"]]
  } else {
    ## do calculate nested models for the methods below
    ## prepare (g)lmer formulas:
    if (type == 3 | type == "III") {
      if (attr(terms(rh2, data = data), "intercept") == 1) {
        fixed.effects <- c("(Intercept)", fixed.effects)
      }
      # The next part alters the mapping of parameters to effects/variables if
      # per_parameter is not NULL (this does the complete magic).
      if (!is.null(per_parameter)) {
        fixed.to.change <- c()
        for (parameter in per_parameter) {
          fixed.to.change <- c(fixed.to.change, grep(parameter, fixed.effects))
        }
        fixed.to.change <- fixed.effects[sort(unique(fixed.to.change))]
        if ("(Intercept)" %in% fixed.to.change) {
          fixed.to.change <- fixed.to.change[-1]
        }
        fixed.all <- dimnames(m.matrix)[[2]]
        #tf2 <- fixed.to.change[2]
        for (tf2 in fixed.to.change) {
          tf <- which(fixed.effects == tf2)
          fixed.lower <- fixed.effects[seq_len(tf - 1)]
          fixed.upper <-
            if (tf < length(fixed.effects)) {
              fixed.effects[(tf + 1):length(fixed.effects)]
            } else {
              NULL
            }
          fixed.effects <-
            c(fixed.lower, fixed.all[which(mapping == (tf - 1))], fixed.upper)
          map.to.replace <- which(mapping == (tf - 1))
          map.lower <- mapping[seq_len(map.to.replace[1] - 1)]
          map.upper <-
            if (max(map.to.replace) < length(mapping)) {
              mapping[
                (map.to.replace[length(map.to.replace)] + 1):length(mapping)
              ]
            } else {
              NULL
            }
          mapping <- c(
            map.lower,
            seq_along(map.to.replace) +
              map.lower[length(map.lower)],
            map.upper + length(map.to.replace) - 1
          )
        }
      }
      # make formulas
      formulas <- vector("list", length(fixed.effects) + 1)
      formulas[[1]] <- mf[["formula"]]
      for (i in seq_along(fixed.effects)) {
        tmp.columns <- paste0(
          deparse(-which(mapping == (i - 1))),
          collapse = ""
        )
        formulas[[i + 1]] <-
          as.formula(paste0(dv, "~ 0 + m.matrix[,", tmp.columns, "] +", random))
      }
      names(formulas) <- c("full_model", fixed.effects)
      if (!test_intercept && fixed.effects[1] == "(Intercept)") {
        fixed.effects <- fixed.effects[-1]
        formulas[["(Intercept)"]] <- NULL
      }
    } else if (type == 2 | type == "II") {
      if (!is.null(per_parameter)) {
        stop("per_parameter argument only implemented for Type 3 tests.")
      }
      full_model.formulas <- vector("list", max.effect.order)
      submodel.formulas <- vector("list", length(fixed.effects))
      full_model.formulas[[length(full_model.formulas)]] <- mf[["formula"]]
      for (c in seq_len(max.effect.order)) {
        if (c == max.effect.order) {
          next
        }
        tmp.columns <-
          paste0(
            deparse(-which(mapping %in% which(effect.order > c))),
            collapse = ""
          )
        full_model.formulas[[c]] <-
          as.formula(paste0(dv, "~ 0 + m.matrix[,", tmp.columns, "] +", random))
      }
      for (c in seq_along(fixed.effects)) {
        order.c <- effect.order[c]
        tmp.columns <-
          paste0(
            deparse(
              -which(
                mapping == (c) |
                  mapping %in%
                    if (order.c == max.effect.order) {
                      -1
                    } else {
                      which(effect.order > order.c)
                    }
              )
            ),
            collapse = ""
          )
        submodel.formulas[[c]] <- as.formula(
          paste0(dv, "~ 0 + m.matrix[,", tmp.columns, "] +", random)
        )
      }
      formulas <- c(full_model.formulas, submodel.formulas)
    } else {
      stop('Only type 3 and type 2 tests implemented.')
    }
    ## Part IIb: fit models
    # single core
    if (is.null(cl)) {
      if (progress) {
        cat(paste0("Fitting ", length(formulas), " (g)lmer() models:\n["))
      }
      fits <- vector("list", length(formulas))
      if (all_fit) {
        all_fits <- vector("list", length(formulas))
      }
      for (i in seq_along(formulas)) {
        mf[["formula"]] <- formulas[[i]]
        fits[[i]] <- eval(mf)
        if (all_fit) {
          all_fits[[i]] <- suppressWarnings(
            lme4::allFit(fits[[i]], data = data, verbose = FALSE)
          )
          all_fits[[i]] <- c(default = fits[[i]], all_fits[[i]])
          tmp_ll <- vapply(
            all_fits[[i]],
            function(x) tryCatch(logLik(x), error = function(e) NA),
            0
          )
          fits[[i]] <- all_fits[[i]][[which.max(tmp_ll)]]
          fits[[i]]@optinfo$logLik_other <- tmp_ll
        }
        if (progress) cat(".")
      }
      if (progress) cat("]\n")
    } else {
      # multicore
      eval.cl <- function(formula, m.call, progress, all_fit, data) {
        m.call[[2]] <- formula
        res <- eval(m.call)
        if (all_fit) {
          all_fits <- suppressWarnings(lme4::allFit(
            res,
            data = data,
            verbose = FALSE
          ))
          all_fits <- c(default = res, all_fits)
          tmp_ll <- vapply(
            all_fits,
            function(x) tryCatch(logLik(x), error = function(e) NA),
            0
          )
          res <- all_fits[[which.max(tmp_ll)]]
          res@optinfo$logLik_other <- tmp_ll
        }
        if (progress) {
          cat(".")
        }
        return(res)
      }
      if (progress) {
        cat(paste0("Fitting ", length(formulas), " (g)lmer() models.\n"))
      }
      junk <- clusterCall(
        cl = cl,
        "require",
        package = "afex",
        character.only = TRUE
      )
      if (check_contrasts) {
        curr.contrasts <- getOption("contrasts")
        clusterExport(cl = cl, "curr.contrasts", envir = sys.nframe())
        junk <- clusterEvalQ(cl = cl, options(contrasts = curr.contrasts))
      }
      if (progress) {
        junk <- clusterEvalQ(cl = cl, cat("["))
      }
      fits <- clusterApplyLB(
        cl = cl,
        x = formulas,
        eval.cl,
        m.call = mf,
        progress = progress,
        all_fit = all_fit,
        data = data
      )
      if (progress) junk <- clusterEvalQ(cl = cl, cat("]"))
    }

    ####################
    ### Part IIb: likelihood checks and refitting (refitting is DISABLED for the time being!)
    ####################

    check_likelihood <- function(fits) {
      if (type == 3 | type == "III") {
        logLik_full <- as.numeric(logLik(fits[[1]]))
        logLik_restricted <- as.numeric(vapply(fits[2:length(fits)], logLik, 0))
        if (any(logLik_restricted > logLik_full)) {
          return(fixed.effects[logLik_restricted > logLik_full])
        }
      } else if (type == 2 | type == "II") {
        logLik_full <- as.numeric(vapply(fits[1:max.effect.order], logLik, 0))
        logLik_restricted <-
          as.numeric(vapply(
            fits[(max.effect.order + 1):length(fits)],
            logLik,
            0
          ))
        warn_logLik <- c()
        for (c in seq_along(fixed.effects)) {
          order.c <- effect.order[c]
          if (logLik_restricted[[c]] > logLik_full[[order.c]]) {
            warn_logLik <- c(warn_logLik, fixed.effects[c])
          }
        }
        if (length(warn_logLik) > 0) return(warn_logLik)
      }
      return(TRUE)
    }

    # check for smaller likelihood of nested model and refit if test fails:
    if (FALSE) {
      if (!isTRUE(check_likelihood(fits))) {
        if (progress) {
          cat("refitting...")
        }
        refits <- lapply(fits, all_fit, verbose = FALSE, data = data)
        browser()
        str(fits[[1]], 2)
        fits[[1]]@call
        sapply(refits, function(x) {
          sapply(x, function(y) {
            tryCatch(as.numeric(logLik(y)), error = function(e) as.numeric(NA))
          })
        })

        fits <- lapply(refits, function(x) {
          tmp_llk <- vapply(
            x,
            function(y) {
              tryCatch(logLik(y), error = function(e) as.numeric(NA))
            },
            0
          )
          x[[which.min(tmp_llk)]]
        })
      }
    }
    # check again and warn
    if (!isREML(fits[[1]]) & !isTRUE(check_likelihood(fits))) {
      warning(paste(
        "Following nested model(s) provide better fit than full model:",
        paste(check_likelihood(fits), collapse = ", "),
        "\n  Results cannot be trusted.",
        "Try all_fit=TRUE or reduce random effect structure!"
      ))
    }

    if (set_data_arg) {
      for (i in seq_along(fits)) {
        fits[[i]]@call[["data"]] <- mc[["data"]]
      }
    }
    ## prepare for p-values:
    if (type == 3 | type == "III") {
      full_model <- fits[[1]]
      fits <- fits[-1]
    } else if (type == 2 | type == "II") {
      full_model <- fits[1:max.effect.order]
      fits <- fits[(max.effect.order + 1):length(fits)]
    }
    names(fits) <- fixed.effects

    ####################
    ### Part III: obtain p-values
    ####################
    ## obtain p-values:
    #browser()
    if (method[1] == "nested-KR") {
      if (progress) {
        cat(paste0("Obtaining ", length(fixed.effects), " p-values:\n["))
      }
      tests <- vector("list", length(fixed.effects))
      for (c in seq_along(fixed.effects)) {
        if (type == 3 | type == "III") {
          tests[[c]] <- pbkrtest::KRmodcomp(full_model, fits[[c]])
        } else if (type == 2 | type == "II") {
          order.c <- effect.order[c]
          tests[[c]] <- pbkrtest::KRmodcomp(full_model[[order.c]], fits[[c]])
        }
        if (progress) cat(".")
      }
      if (progress) {
        cat("]\n")
      }
      names(tests) <- fixed.effects
      anova_table <- data.frame(
        t(vapply(
          tests,
          function(x) unlist(x[["test"]][1, ]),
          unlist(tests[[1]][["test"]][1, ])
        ))
      )
      rownames(anova_table) <- fixed.effects
      colnames(anova_table) <-
        c("F", "num Df", "den Df", "F.scaling", "Pr(>F)")
      anova_table <-
        anova_table[, c("num Df", "den Df", "F.scaling", "F", "Pr(>F)")]
      anova_tab_addition <- NULL
    } else if (method[1] == "PB") {
      if (progress) {
        cat(paste0("Obtaining ", length(fixed.effects), " p-values:\n["))
      }
      tests <- vector("list", length(fixed.effects))
      for (c in seq_along(fixed.effects)) {
        if (type == 3 | type == "III") {
          tests[[c]] <- do.call(
            pbkrtest::PBmodcomp,
            args = c(largeModel = full_model, smallModel = fits[[c]], args_test)
          )
        } else if (type == 2 | type == "II") {
          order.c <- effect.order[c]
          tests[[c]] <- do.call(
            pbkrtest::PBmodcomp,
            args = c(
              largeModel = full_model[[order.c]],
              smallModel = fits[[c]],
              args_test
            )
          )
        }
        if (progress) cat(".")
      }
      if (progress) {
        cat("]\n")
      }
      names(tests) <- fixed.effects
      anova_table <-
        data.frame(t(vapply(
          tests,
          function(x) unlist(x[["test"]][2, ]),
          unlist(tests[[1]][["test"]][2, ])
        )))
      anova_table <- anova_table[, -2]
      LRT <- vapply(
        tests,
        function(x) unlist(x[["test"]][1, ]),
        unlist(tests[[1]][["test"]][1, ])
      )
      row.names(LRT) <- paste0(row.names(LRT), ".LRT")
      anova_table <- cbind(anova_table, t(LRT))
      rownames(anova_table) <- fixed.effects
      anova_table <-
        anova_table[, c("stat", "df.LRT", "p.value.LRT", "p.value")]
      colnames(anova_table) <- c("Chisq", "Chi Df", "Pr(>Chisq)", "Pr(>PB)")
      anova_tab_addition <- NULL
    } else if (method[1] == "LRT") {
      tests <- vector("list", length(fixed.effects))
      for (c in seq_along(fixed.effects)) {
        if (type == 3 | type == "III") {
          tests[[c]] <- anova(full_model, fits[[c]])
        } else if (type == 2 | type == "II") {
          order.c <- effect.order[c]
          tmpModel <- full_model[[order.c]]
          tests[[c]] <- anova(tmpModel, fits[[c]])
        }
      }
      names(tests) <- fixed.effects
      chisq <- vapply(tests, function(x) x[["Chisq"]][2], 0)
      if (packageVersion("lme4") <= "1.1.21") {
        df.large <- vapply(tests, function(x) x[["Df"]][2], 0)
        df.small <- vapply(tests, function(x) x[["Df"]][1], 0)
        df <- vapply(tests, function(x) x[["Chi Df"]][2], 0)
      } else {
        df.large <- vapply(tests, function(x) x[["npar"]][2], 0)
        df.small <- vapply(tests, function(x) x[["npar"]][1], 0)
        df <- vapply(tests, function(x) x[["Df"]][2], 0)
      }
      p.value <- vapply(tests, function(x) x[["Pr(>Chisq)"]][2], 0)
      anova_table <- data.frame(
        Df = df.small,
        Chisq = chisq,
        "Chi Df" = df,
        "Pr(>Chisq)" = p.value,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      rownames(anova_table) <- fixed.effects
      if (type == 3 | type == "III") {
        anova_tab_addition <- paste0("Df full model: ", df.large[1])
      } else {
        anova_tab_addition <- paste0(
          "Df full model(s): ",
          paste(df.large, collapse = ", ")
        )
      }
    } else {
      stop(
        'Only methods "KR", "PB", "LRT", or "nested-KR" currently implemented.'
      )
    }
  }
  ####################
  ### Part IV: prepare output
  ####################
  class(anova_table) <- c("anova", "data.frame")
  attr(anova_table, "heading") <- c(
    paste0(
      "Mixed Model Anova Table (Type ",
      type,
      " tests, ",
      method,
      "-method)\n"
    ),
    paste0("Model: ", deparse(formula.f)),
    paste0("Data: ", deparse(mc[["data"]])),
    anova_tab_addition
  )
  attr(anova_table, "sig_symbols") <- sig_symbols
  list.out <- list(
    anova_table = anova_table,
    full_model = full_model,
    restricted_models = fits,
    tests = tests,
    data = data,
    call = mc
  ) #, type = type, method = method[[1]]
  class(list.out) <- "mixed"
  attr(list.out, "type") <- type
  attr(list.out, "method") <- method
  if (all_fit) {
    attr(list.out, "all_fit_selected") <-
      rapply(
        c(full_model = list.out$full_model, list.out$restricted_models),
        function(x) x@optinfo$optimizer,
        how = "unlist"
      )
    attr(list.out, "all_fit_logLik") <- as.data.frame(
      rapply(
        c(full_model = list.out$full_model, list.out$restricted_models),
        function(x) x@optinfo$logLik_other,
        how = "replace"
      )
    )
  }
  list.out
}

## expand random effects sructure
expand_re_fun <- function(all.terms, data) {
  random_parts <- paste0(all.terms[grepl("\\|", all.terms)])
  which_random_double_bars <- grepl("\\|\\|", random_parts)
  random_units <- sub("^.+\\|\\s+", "", random_parts)
  tmp_random <- lapply(sub("\\|.+$", "", random_parts), function(x) {
    as.formula(paste0("~", x))
  })

  tmp_model.matrix <- vector("list", length(random_parts))
  re_contains_intercept <- rep(FALSE, length(random_parts))
  new_random <- vector("character", length(random_parts))

  for (i in seq_along(random_parts)) {
    tmp_model.matrix[[i]] <- model.matrix(tmp_random[[i]], data = data)
    if (ncol(tmp_model.matrix[[i]]) == 0) {
      stop("Invalid random effects term, e.g., (0|id)")
    }
    if (colnames(tmp_model.matrix[[i]])[1] == "(Intercept)") {
      tmp_model.matrix[[i]] <- tmp_model.matrix[[i]][, -1, drop = FALSE]
      re_contains_intercept[i] <- TRUE
    }
    if (ncol(tmp_model.matrix[[i]]) > 0) {
      colnames(tmp_model.matrix[[i]]) <-
        paste0("re", i, ".", gsub(":", "_by_", colnames(tmp_model.matrix[[i]])))
      colnames(tmp_model.matrix[[i]]) <- make.names(colnames(tmp_model.matrix[[
        i
      ]]))
      new_random[i] <-
        paste0(
          "(",
          as.numeric(re_contains_intercept[i]),
          "+",
          paste0(colnames(tmp_model.matrix[[i]]), collapse = "+"),
          if (which_random_double_bars[i]) "||" else "|",
          random_units[i],
          ")"
        )
    } else {
      new_random[i] <- paste0(
        "(",
        as.numeric(re_contains_intercept[i]),
        if (which_random_double_bars[i]) "||" else "|",
        random_units[i],
        ")"
      )
    }
  }
  data <- cbind(data, as.data.frame(do.call(cbind, tmp_model.matrix)))
  random <- paste0(new_random, collapse = "+")
  return(list(data = data, random = random))
}

get_mixed_warnings <- function(x) {
  full_model_name <- names(x)[[2]]
  ntry <- function(x) tryCatch(x, error = function(e) NULL)
  if (is.list(x$full)) {
    warnings1 <- c(
      full = lapply(x[[2]], function(y) y@optinfo$warnings),
      lapply(x[[3]], function(y) y@optinfo$warnings)
    )
    warnings2 <-
      c(
        full = lapply(x[[2]], function(y) ntry(y@optinfo$conv$lme4$messages)),
        lapply(x[[3]], function(y) ntry(y@optinfo$conv$lme4$messages))
      )
  } else {
    warnings1 <- c(
      full = list(x[[full_model_name]]@optinfo$warnings),
      lapply(x[[3]], function(y) y@optinfo$warnings)
    )
    warnings2 <-
      c(
        full = list(ntry(x[[full_model_name]]@optinfo$conv$lme4$messages)),
        lapply(x[[3]], function(y) ntry(y@optinfo$conv$lme4$messages))
      )
  }
  warnings <- mapply(
    function(x, y) c(unlist(x), y),
    warnings1,
    warnings2,
    SIMPLIFY = FALSE
  )
  warn <- vapply(warnings, function(y) !length(y) == 0, NA)
  for (i in names(warn)[warn]) {
    warning(
      "lme4 reported (at least) the following warnings for '",
      i,
      "':\n  * ",
      paste(warnings[[i]], collapse = "\n  * "),
      call. = FALSE
    )
  }
}

check_likelihood <- function(object) {
  full_model_name <- names(object)[[2]]
  restricted_models_name <- names(object)[[3]]
  if (is.null(attr(object, "type"))) {
    attr(object, "type") <- object$type
  }

  if (attr(object, "type") == 3 | attr(object, "type") == "III") {
    logLik_full <- as.numeric(logLik(object[[full_model_name]]))
    logLik_restricted <-
      as.numeric(vapply(object[[restricted_models_name]], logLik, 0))
    if (any(logLik_restricted > logLik_full)) {
      return(rownames(object$anova_table)[logLik_restricted > logLik_full])
    }
  } else if (attr(object, "type") == 2 | attr(object, "type") == "II") {
    NULL
  }
  return(TRUE)
}


#' @rdname mixed
#' @export
lmer_alt <- function(formula, data, check_contrasts = FALSE, ...) {
  mc <- match.call()
  #assign(all.vars(mc[["data"]]), data)
  mc[[1]] <- as.name("mixed")
  mc[["return"]] <- "merMod"
  mc[["expand_re"]] <- TRUE
  mc[["progress"]] <- FALSE
  mc[["check_contrasts"]] <- check_contrasts
  #browser()
  eval(mc)
}

#' @method print mixed
#' @export
print.mixed <- function(x, ...) {
  full_model_name <- names(x)[[2]]
  try(
    if (!isREML(x[[full_model_name]]) && !isTRUE(check_likelihood(x))) {
      warning(
        paste(
          "Following nested model(s) provide better fit than full model:",
          paste(check_likelihood(x), collapse = ", "),
          "\n  Results cannot be trusted. Try all_fit=TRUE!"
        ),
        call. = FALSE
      )
    },
    silent = TRUE
  )
  get_mixed_warnings(x)
  tmp <- nice.mixed(x, ...)
  print(tmp)
  invisible(tmp)
}

#anova.mixed <-

#' @method summary mixed
#' @export
summary.mixed <- function(object, ...) {
  if ("full_model" %in% names(object)) {
    summary(
      object = if (length(object[["full_model"]]) == 1) {
        object[["full_model"]]
      } else {
        object[["full_model"]][[length(object[["full_model"]])]]
      },
      ...
    )
  } else if ("full.model" %in% names(object)) {
    summary(
      object = if (length(object[["full.model"]]) == 1) {
        object[["full.model"]]
      } else {
        object[["full.model"]][[length(object[["full.model"]])]]
      },
      ...
    )
  }
}

#' @method anova mixed
#' @export
anova.mixed <- function(
  object,
  ...,
  sig_symbols = attr(object$anova_table, "sig_symbols"),
  refit = FALSE
) {
  mCall <- match.call(expand.dots = TRUE)
  full_model_name <- names(object)[[2]]
  dots <- list(...)
  modp <- (as.logical(vapply(dots, is, NA, "merMod")) |
    as.logical(vapply(dots, is, NA, "lm")) |
    as.logical(vapply(dots, is, NA, "mixed")))
  if (any(modp)) {
    model.names <- c(
      deparse(mCall[["object"]]),
      vapply(which(modp), function(x) deparse(mCall[[x + 2]]), "")
    )
    for (i in which(as.logical(vapply(dots, is, NA, "mixed")))) {
      dots[[i]] <- dots[[i]][[full_model_name]]
    }
    anova_table <- do.call(
      anova,
      args = c(
        object = object[[full_model_name]],
        dots,
        model.names = list(model.names),
        refit = refit
      )
    )
  } else {
    try(
      if (
        !isREML(object[[full_model_name]]) &&
          !isTRUE(check_likelihood(object))
      ) {
        warning(
          paste(
            "Following nested model(s) provide better fit than full model:",
            paste(check_likelihood(object), collapse = ", "),
            "\n  Results cannot be trusted. Try all_fit=TRUE!"
          ),
          call. = FALSE
        )
      },
      silent = TRUE
    )
    get_mixed_warnings(object)
    anova_table <- object$anova_table
  }

  attr(anova_table, "sig_symbols") <-
    if (!is.null(sig_symbols)) {
      sig_symbols
    } else {
      afex_options("sig_symbols")
    }
  anova_table
}


## support for emmeans for mixed objects:
## @importFrom emmeans recover_data emm_basis
## @method recover_data mixed
## @export
recover_data.mixed <- function(object, ...) {
  full_model_name <- names(object)[[2]]
  if (
    inherits(object[[full_model_name]], "merMod") |
      is_lmerTest_class(object[[full_model_name]])
  ) {
    obj_use <- object[[full_model_name]]
  } else if (
    inherits(object[[full_model_name]][[1]], "merMod") |
      is_lmerTest_class(object[[full_model_name]][[1]])
  ) {
    message("emmeans are based on full model which includes all effects.")
    obj_use <- object[[full_model_name]][[length(object[[full_model_name]])]]
  } else {
    stop("Cannot find 'merMod' object in ", full_model_name, " slot.")
  }
  # if (is_lmerTest_class(obj_use)) {
  #   class(obj_use) <- "lmerMod"
  # }
  emmeans::recover_data(obj_use, ...)
}


## @method lsm_basis mixed
## @export
emm_basis.mixed <- function(object, trms, xlev, grid, ...) {
  full_model_name <- names(object)[[2]]
  if (
    inherits(object[[full_model_name]], "merMod") |
      is_lmerTest_class(object[[full_model_name]])
  ) {
    obj_use <- object[[full_model_name]]
  } else if (
    inherits(object[[full_model_name]][[1]], "merMod") |
      is_lmerTest_class(object[[full_model_name]][[1]])
  ) {
    obj_use <- object[[full_model_name]][[length(object[[full_model_name]])]]
  } else {
    stop("Cannot find 'merMod' object in ", full_model_name, " slot.")
  }
  # if (is_lmerTest_class(obj_use)) {
  #   class(obj_use) <- "lmerMod"
  # }
  emmeans::emm_basis(obj_use, trms, xlev, grid, ...)
}
