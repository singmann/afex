#' Convenient ANOVA estimation for factorial designs
#'
#' These functions allow convenient specification of any type of ANOVAs (i.e.,
#' purely within-subjects ANOVAs, purely between-subjects ANOVAs, and mixed
#' between-within or split-plot ANOVAs) for data in the \strong{long} format
#' (i.e., one observation per row). If the data has more than one observation
#' per individual and cell of the design (e.g., multiple responses per
#' condition), the data will be automatically aggregated. The default settings
#' reproduce results from commercial statistical packages such as SPSS or SAS.
#' \code{aov_ez} is called specifying the factors as character vectors,
#' \code{aov_car} is called using a formula similar to \code{\link{aov}}
#' specifying an error strata for the within-subject factor(s), and \code{aov_4}
#' is called with a \pkg{lme4}-like formula (all ANOVA functions return
#' identical results). The returned object can be passed to e.g., \pkg{emmeans}
#' for further analysis (e.g., follow-up tests, contrasts, plotting, etc.).
#' These functions employ \code{\link[car]{Anova}} (from the \pkg{car} package)
#' to provide test of effects avoiding the somewhat unhandy format of
#' \code{car::Anova}.
#'
#'
#' @param id \code{character} vector (of length 1) indicating the subject
#'   identifier column in \code{data}.
#' @param dv \code{character} vector (of length 1) indicating the column
#'   containing the \strong{dependent variable} in \code{data}.
#' @param between \code{character} vector indicating the
#'   \strong{between}-subject(s) factor(s)/column(s) in \code{data}. Default is
#'   \code{NULL} indicating no between-subjects factors.
#' @param within \code{character} vector indicating the
#'   \strong{within}-subject(s)(or repeated-measures) factor(s)/column(s) in
#'   \code{data}.  Default is \code{NULL} indicating no within-subjects factors.
#' @param covariate \code{character} vector indicating the between-subject(s)
#'   covariate(s) (i.e., column(s)) in \code{data}. Default is \code{NULL}
#'   indicating no covariates. Please note that \code{factorize} needs to be set
#'   to \code{FALSE} in case the covariate is numeric and should be treated as
#'   such.
#' @param observed \code{character} vector indicating which of the variables are
#'   observed (i.e, measured) as compared to experimentally manipulated. The
#'   default effect size reported (generalized eta-squared) requires correct
#'   specification of the observed (in contrast to manipulated) variables.
#' @param formula A formula specifying the ANOVA model similar to
#'   \code{\link{aov}} (for \code{aov_car} or similar to \code{lme4:lmer} for
#'   \code{aov_4}). Must include an error term (i.e., \code{Error(id/...)} for
#'   \code{aov_car} or \code{(...|id)} for \code{aov_4}). Note that the
#'   within-subject factors do not need to be outside the Error term (this
#'   contrasts with \code{aov}). See Details.
#' @param data A \code{data.frame} containing the data. Mandatory.
#' @param fun_aggregate The function for aggregating the data before running the
#'   ANOVA if there is more than one observation per individual and cell of the
#'   design. The default \code{NULL} issues a warning if aggregation is
#'   necessary and uses \code{\link{mean}}. Pass \code{mean} directly to avoid
#'   the warning.
#' @param transformation In \code{aov_ez}, a \code{character} vector (of length
#'   1) indicating the name of a transformation to apply to \code{dv} before
#'   fitting the model. If missing, no transformation is applied. In
#'   \code{aov_car} and \code{aov_4}, a response transformation may be
#'   incorporated in the left-hand side of \code{formula}.
#' @param type The type of sums of squares for the ANOVA. The default is given
#'   by \code{afex_options("type")}, which is \strong{initially set to 3}.
#'   Passed to \code{\link[car]{Anova}}. Possible values are \code{"II"},
#'   \code{"III"}, \code{2}, or \code{3}.
#' @param factorize logical. Should between subject factors be factorized (with
#'   note) before running the analysis. The default is given by
#'   \code{afex_options("factorize")}, which is initially \code{TRUE}. If one
#'   wants to run an ANCOVA, this needs to be set to \code{FALSE} (in which case
#'   centering on 0 is checked on numeric variables).
#' @param print.formula \code{aov_ez} and \code{aov_4} are wrapper for
#'   \code{aov_car}. This boolean argument indicates whether the formula in the
#'   call to \code{car.aov} should be printed.

#' @param anova_table \code{list} of further arguments passed to function
#'   producing the ANOVA table.  Arguments such as \code{es} (effect size) or
#'   \code{correction}  are passed to either \code{anova.afex_aov} or
#'   \code{nice}. Note that those settings can also be changed once an object of
#'   class \code{afex_aov} is created by invoking the \code{anova} method
#'   directly.
#' @param include_aov Boolean. Allows suppressing the calculation of the aov
#'   object. If TRUE the aov model is part of the returned \code{afex_aov}
#'   object. \code{FALSE} (the default) prevents this potentially costly
#'   calculation. Especially for designs with larger N and within-subjects
#'   factors, this is highly advisable. Follow-up analyses using \pkg{emmeans}
#'   using the \code{univariate} model (which is not recommended) require the
#'   aov model and TRUE.
#' @param ... Further arguments passed to \code{fun_aggregate}.
#' @param return What should be returned? The default is given by
#'   \code{afex_options("return_aov")}, which is initially \code{"afex_aov"},
#'   returning an S3 object of class \code{afex_aov} for which various
#'   \link[=afex_aov-methods]{methods} exist (see there and below for more
#'   details). Other values are currently still supported for backward
#'   compatibility.
#    To avoid the (potentially costly) computation via \code{aov} set
#    \code{return} to \code{"nice"} in which case only the nice ANOVA table is
#    returned (produced by \code{\link{nice}}, this was the previous default
#    return value).
#    Possible values are \code{c("Anova", "lm", "data", "nice", "full", "all",
#    "univariate", "marginal", "aov")} (possibly abbreviated).
#'
#' @return \code{aov_car}, \code{aov_4}, and \code{aov_ez} are wrappers for
#'   \code{\link[car]{Anova}} and \code{\link{aov}}, the return value is
#'   dependent on the \code{return} argument. Per default, an S3 object of class
#'   \code{"afex_aov"} is returned containing the following slots:
#'
#' \describe{
#'   \item{\code{"anova_table"}}{An ANOVA table of class \code{c("anova",
#'    "data.frame")}.}
#'   \item{\code{"aov"}}{\code{aov} object returned from \code{\link{aov}}
#'    (should not be used to evaluate significance of effects, but can be passed
#'    to \code{emmeans} for post-hoc tests).}
#'   \item{\code{"Anova"}}{object returned from \code{\link[car]{Anova}}, an
#'   object of class \code{"Anova.mlm"} (if within-subjects factors are present)
#'   or of class \code{c("anova", "data.frame")}.}
#'   \item{\code{"lm"}}{the object fitted with \code{lm} and passed to
#'    \code{Anova} (i.e., an object of class \code{"lm"} or \code{"mlm"}). Also
#'    returned if \code{return = "lm"}.}
#'   \item{\code{"data"}}{a list containing: (1) \code{long} (the possibly
#'    aggregated data in long format used for \code{aov}), \code{wide} (the data
#'    used to fit the \code{lm} object), and \code{idata} (if within-subject
#'    factors are present, the \code{idata} argument passed to
#'   \code{car::Anova}). Also returned if \code{return = "data"}.}
#' }
#' In addition, the object has the following attributes: \code{"dv"},
#' \code{"id"}, \code{"within"}, \code{"between"}, and \code{"type"}.
#'
#' The \link[=afex_aov-methods]{print} method for \code{afex_aov} objects
#' (invisibly) returns (and prints) the same as if \code{return} is
#' \code{"nice"}: a nice ANOVA table (produced by \code{\link{nice}}) with the
#' following columns: \code{Effect}, \code{df}, \code{MSE} (mean-squared
#' errors), \code{F} (potentially with significant symbols), \code{ges}
#' (generalized eta-squared), \code{p}.
#'
#' @details
#'
#' \subsection{Details of ANOVA Specification}{ \code{aov_ez} will concatenate
#' all between-subject factors using \code{*} (i.e., producing all main effects
#' and interactions) and all covariates by \code{+} (i.e., adding only the main
#' effects to the existing between-subject factors). The within-subject factors
#' do fully interact with all between-subject factors and covariates. This is
#' essentially identical to the behavior of SPSS's \code{glm} function.
#'
#' The \code{formula}s for \code{aov_car} or \code{aov_4} must contain a single
#' \code{Error} term specifying the \code{ID} column and potential
#' within-subject factors (you can use \code{\link{mixed}} for running
#' mixed-effects models with multiple error terms). Factors outside the
#' \code{Error} term are treated as between-subject factors (the within-subject
#' factors specified in the \code{Error} term are ignored outside the
#' \code{Error} term; in other words, it is not necessary to specify them
#' outside the \code{Error} term, see Examples).\cr Suppressing the intercept
#' (i.e, via \code{0 +} or \code{- 1}) is ignored. Specific specifications of
#' effects (e.g., excluding terms with \code{-} or using \code{^}) could be okay
#' but is not tested. Using the \code{\link{I}} or \code{\link{poly}} function
#' within the formula is not tested and not supported!
#'
#' To run an ANCOVA you need to set \code{factorize = FALSE} and make sure that
#' all variables have the correct type (i.e., factors are factors and numeric
#' variables are numeric and centered).
#'
#' Note that the default behavior is to include calculation of the effect size
#' generalized eta-squared for which \strong{all non-manipluated (i.e.,
#' observed)} variables need to be specified via the \code{observed} argument to
#' obtain correct results. When changing the effect size to \code{"pes"}
#' (partial eta-squared) or \code{"none"} via \code{anova_table} this becomes
#' unnecessary.
#'
#' Factor contrasts will be set to \code{"contr.sum"} for all between-subject
#' factors if default contrasts are not equal to \code{"contr.sum"} or
#' \code{attrib(factor, "contrasts") != "contr.sum"}. (within-subject factors
#' are hard-coded \code{"contr.sum"}.) }
#'
#' \subsection{Statistical Issues}{ \strong{Type 3 sums of squares are default
#' in \pkg{afex}.} While some authors argue that so-called type 3 sums of
#' squares are dangerous and/or problematic (most notably Venables, 2000), they
#' are the default in many commercial statistical application such as SPSS or
#' SAS. Furthermore, statisticians with an applied perspective recommend type 3
#' tests (e.g., Maxwell and Delaney, 2004). Consequently, they are the default
#' for the ANOVA functions described here. For some more discussion on this
#' issue see \href{https://stats.stackexchange.com/q/6208/442}{here}.
#'
#' Note that lower order effects (e.g., main effects) in type 3 ANOVAs are only
#' meaningful with
#' \href{https://stats.oarc.ucla.edu/other/mult-pkg/faq/general/faqwhat-is-effect-coding/}{effects
#' coding}. Therefore, contrasts are set to \code{\link{contr.sum}} which
#' ensures meaningful results. For a discussion of the other (non-recommended)
#' coding schemes see
#' \href{https://stats.oarc.ucla.edu/r/library/r-library-contrast-coding-systems-for-categorical-variables/}{here}.
#' }
#'
#' \subsection{Follow-Up Contrasts and Post-Hoc Tests}{ The S3 object returned
#' per default can be directly passed to \code{emmeans::emmeans} for further
#' analysis. This allows to test any type of contrasts that might be of interest
#' independent of whether or not this contrast involves between-subject
#' variables, within-subject variables, or a combination thereof. The general
#' procedure to run those contrasts is the following (see Examples for a full
#' example):
#'
#'  \enumerate{
#'    \item Estimate an \code{afex_aov} object with the function returned here. For example: \code{x <- aov_car(dv ~ a*b + (id/c), d)}
#'    \item Obtain a \code{\link[emmeans]{emmGrid-class}} object by running \code{\link[emmeans]{emmeans}} on the \code{afex_aov} object from step 1 using the factors involved in the contrast. For example: \code{r <- emmeans(x, ~a:c)}
#'    \item Create a list containing the desired contrasts on the reference grid object from step 2. For example: \code{con1 <- list(a_x = c(-1, 1, 0, 0, 0, 0), b_x = c(0, 0, -0.5, -0.5, 0, 1))}
#'    \item Test the contrast on the reference grid using \code{\link[emmeans]{contrast}}. For example: \code{contrast(r, con1)}
#'    \item To control for multiple testing p-value adjustments can be specified. For example the Bonferroni-Holm correction: \code{contrast(r, con1, adjust = "holm")}
#'  }
#'
#'  Note that \pkg{emmeans} allows for a variety of advanced settings and
#'  simplifications, for example: all pairwise comparison of a single factor
#'  using one command (e.g., \code{emmeans(x, "a", contr = "pairwise")}) or
#'  advanced control for multiple testing by passing objects to \pkg{multcomp}.
#'  A comprehensive overview of the functionality is provided in the
#'  accompanying vignettes (see
#'  \href{https://CRAN.R-project.org/package=emmeans}{here}).
#'
#'  Since version 1.0, \pkg{afex} per default uses the \code{multivariate} model
#'  (i.e., the \code{lm} slot of the \code{afex_aov} object) for follow-up tests
#'  with \pkg{emmeans}. Compared to the \code{univariate} model (i.e., the
#'  \code{aov} slot), this can handle unbalanced data and addresses sphericity
#'  better. To use the older (and not recommended) \code{model = "univariate"}
#'  make sure to set \code{include_aov = TRUE} when estimating the ANOVA.
#'
#'  Starting with \pkg{afex} version 0.22, \pkg{emmeans} is \emph{not}
#'  loaded/attached automatically when loading \pkg{afex}. Therefore,
#'  \pkg{emmeans} now needs to be loaded by the user via
#'  \code{library("emmeans")} or \code{require("emmeans")}.
#' }
#'
#' \subsection{Methods for \code{afex_aov} Objects}{ A full overview over the
#' methods provided for \code{afex_aov} objects is provided in the corresponding
#' help page: \code{\link{afex_aov-methods}}. The probably most important ones
#' for end-users are \code{summary}, \code{anova}, and \code{\link{nice}}.
#'
#' The \code{summary} method returns, for ANOVAs containing within-subject
#' (repeated-measures) factors with more than two levels, the complete
#' univariate analysis: Results without df-correction, the Greenhouse-Geisser
#' corrected results, the Hyunh-Feldt corrected results, and the results of the
#' Mauchly test for sphericity.
#'
#' The \code{anova} method returns a \code{data.frame} of class \code{"anova"}
#' containing the ANOVA table in numeric form (i.e., the one in slot
#' \code{anova_table} of a \code{afex_aov}). This method has arguments such as
#' \code{correction} and \code{es} and can be used to obtain an ANOVA table with
#' different correction than the one initially specified.
#'
#' The \code{\link{nice}} method also returns a \code{data.frame}, but rounds
#' most values and transforms them into characters for nice printing. Also has
#' arguments like \code{correction} and \code{es} which can be used to obtain an
#' ANOVA table with different correction than the one initially specified. }
#'
#' @author Henrik Singmann
#'
#' The design of these functions was influenced by \code{\link[ez]{ezANOVA}}
#' from package \pkg{ez}.
#'
#' @note Calculation of ANOVA models via \code{aov} (which is done per default)
#'   can be comparatively slow and produce comparatively large objects for
#'   ANOVAs with many within-subjects factors or levels. To avoid this
#'   calculation set \code{include_aov = FALSE}. You can also disable this
#'   globally with: \code{afex_options(include_aov = FALSE)}
#'
#'   The id variable and variables entered as within-subjects (i.e.,
#'   repeated-measures) factors are silently converted to factors. Levels of
#'   within-subject factors are converted to valid variable names using
#'   \code{\link{make.names}(...,unique=TRUE)}. Unused factor levels are
#'   silently dropped on all variables.
#'
#'   Contrasts attached to a factor as an attribute are probably not preserved
#'   and not supported.
#'
#'   The workhorse is \code{aov_car}. \code{aov_4} and \code{aov_ez} only
#'   construe and pass an appropriate formula to \code{aov_car}. Use
#'   \code{print.formula = TRUE} to view this formula.
#'
#'   In contrast to \code{\link{aov}} \code{aov_car} assumes that all factors to
#'   the right of \code{/} in the \code{Error} term are belonging together.
#'   Consequently, \code{Error(id/(a*b))} and \code{Error(id/a*b)} are identical
#'   (which is not true for \code{\link{aov}}).
#'
#' @seealso Various methods for objects of class \code{afex_aov} are available:
#'   \code{\link{afex_aov-methods}}
#'
#' \code{\link{nice}} creates the nice ANOVA tables which is by default printed.
#' See also there for a slightly longer discussion of the available effect
#' sizes.
#'
#' \code{\link{mixed}} provides a (formula) interface for obtaining p-values for
#' mixed-models via \pkg{lme4}. The functions presented here do not estimate
#' mixed models.
#'
#' @references Cramer, A. O. J., van Ravenzwaaij, D., Matzke, D., Steingroever,
#'   H., Wetzels, R., Grasman, R. P. P. P., ... Wagenmakers, E.-J. (2015).
#'   Hidden multiplicity in exploratory multiway ANOVA: Prevalence and remedies.
#'   \emph{Psychonomic Bulletin & Review}, 1-8. \doi{10.3758/s13423-015-0913-5}
#'
#'   Maxwell, S. E., & Delaney, H. D. (2004). \emph{Designing Experiments and
#'   Analyzing Data: A Model-Comparisons Perspective}. Mahwah, N.J.: Lawrence
#'   Erlbaum Associates.
#'
#'   Venables, W.N. (2000). \emph{Exegeses on linear models}. Paper presented to
#'   the S-Plus User's Conference, Washington DC, 8-9 October 1998, Washington,
#'   DC. Available from: \url{http://www.stats.ox.ac.uk/pub/MASS3/Exegeses.pdf}
#'
#' @importFrom car Anova
#' @importFrom reshape2 dcast
#' @importFrom reformulas findbars nobars
#' @importFrom stats terms as.formula xtabs contrasts<- coef
#' @importFrom utils head
#'
#' @example examples/examples.aov_car.R
#'
#'
#' @encoding UTF-8

#' @export
aov_car <- function(
  formula,
  data,
  fun_aggregate = NULL,
  type = afex_options("type"),
  factorize = afex_options("factorize"),
  observed = NULL,
  anova_table = list(),
  include_aov = afex_options("include_aov"),
  return = afex_options("return_aov"),
  ...
) {
  return <- match.arg(
    return,
    c(
      "Anova",
      "lm",
      "data",
      "nice",
      "afex_aov",
      "univariate",
      "marginal",
      "aov"
    )
  )
  dots <- list(...)

  ### deprercate old argument names:
  if ("fun.aggregate" %in% names(dots)) {
    warn_deprecated_arg("fun.aggregate", "fun_aggregate")
    fun_aggregate <- dots$fun.aggregate
    dots <- dots[names(dots) != "fun.aggregate"]
  }

  # transform to data.frame if necessary (e.g., when using dplyr)
  data <- as.data.frame(data)

  # stuff copied from aov:
  Terms <- terms(formula, "Error", data = data)
  indError <- attr(Terms, "specials")$Error
  if (length(indError) > 1L) {
    stop(
      sprintf(
        ngettext(
          length(indError),
          "there are %d Error terms: only 1 is allowed",
          "there are %d Error terms: only 1 is allowed"
        ),
        length(indError)
      ),
      domain = NA
    )
  }

  # from here, code by Henrik Singmann:
  if (is.null(indError)) {
    stop("formula needs an error term identifying the ID column.")
  }

  vars <- all.vars(formula)
  ### check for missing variables
  if (any(!(vars %in% colnames(data)))) {
    mc <- match.call()
    missing_vars <- vars[!(vars %in% colnames(data))]
    stop(
      "variable(s) `",
      paste(missing_vars, collapse = "`, `"),
      "` not in `",
      deparse(mc[["data"]]),
      "`",
      call. = FALSE
    )
  }

  #--- Russ Lenth added/modified code to detect transformed responses:
  lhs <- all.names(formula[[2]])
  transf <- setdiff(lhs, all.vars(formula[[2]]))
  if (length(transf) == 0) {
    transf = NULL
  }
  if (!is.null(transf)) {
    origdv <- setdiff(lhs, transf)
    dv <- paste0(transf[1], ".", origdv)
    data[[dv]] <- eval(formula[[2]], envir = data) # add transformed version
    vars <- vars[!(vars %in% lhs)]
  } else {
    dv <- vars[1]
    if (!is.numeric(data[, dv])) {
      stop("dv needs to be numeric.")
    } #check if dv is numeric
    vars <- vars[-1]
  }
  #--- end RL changes
  parts <- attr(terms(formula, "Error", data = data), "term.labels")
  error.term <- parts[grepl("^Error\\(", parts)]

  id <- all.vars(parse(text = error.term))[1]
  within <- all.vars(parse(text = error.term))[-1]
  between <- vars[!(vars %in% c(id, within))]

  dv.escaped <- escape_vars(dv)
  id.escaped <- escape_vars(id)
  within.escaped <- escape_vars(within)
  between.escaped <- escape_vars(between)

  effect.parts <- parts[!grepl("^Error\\(", parts)]

  if (length(within) > 0) {
    effect.parts.no.within <- character()
    for (term in effect.parts) {
      components <- decomposeTerm(term)
      if (!any(within %in% components)) {
        effect.parts.no.within <- c(effect.parts.no.within, term)
      }
    }
  } else {
    effect.parts.no.within <- effect.parts
  }

  data <- droplevels(data) #remove empty levels.
  # make id and within variables to factors:
  if (!(is.factor(data[, id]))) {
    data[, id] <- factor(data[, id])
  }

  # factorize if necessary
  if (factorize) {
    if (any(!vapply(data[, between, drop = FALSE], is.factor, TRUE))) {
      to.factor <- between[
        !vapply(data[, between, drop = FALSE], is.factor, TRUE)
      ]
      message(paste0(
        "Converting to factor: ",
        paste0(to.factor, collapse = ", ")
      ))
      for (tmp.c in to.factor) {
        data[, tmp.c] <- factor(data[, tmp.c])
      }
    }
  } else {
    # check if numeric variables are centered.
    c.ns <- between[vapply(data[, between, drop = FALSE], is.numeric, TRUE)]
    if (length(c.ns) > 0) {
      non.null <-
        c.ns[
          !abs(vapply(data[, c.ns, drop = FALSE], mean, 0)) <
            .Machine$double.eps^0.5
        ]
      if (length(non.null) > 0) {
        warning(
          paste0(
            "Numerical variables NOT centered on 0 (matters if variable in interaction):\n   ",
            paste0(non.null, collapse = ", ")
          ),
          call. = FALSE
        )
      }
    }
  }

  for (i in c(between, within)) {
    if (is.factor(data[, i]) && length(unique(data[, i])) == 1) {
      stop(paste0(
        "Factor \"",
        i,
        "\" consists of one level only. Remove factor from model?"
      ))
    }
  }

  # make formulas
  rh2 <- if (length(between.escaped) > 0) {
    paste0(effect.parts.no.within, collapse = "+")
  } else {
    "1"
  }
  lh1 <- mypaste(
    id,
    if (length(between.escaped) > 0) {
      paste0(between.escaped, collapse = "+")
    } else {
      NULL
    },
    sep = "+"
  )
  rh1 <- paste0(within.escaped, collapse = "+")
  rh3 <- paste0(within.escaped, collapse = "*")

  # converting all within subject factors to factors and
  # add a leading charcter (x) if starting with a digit.
  for (within.factor in within) {
    if (is.factor(data[, within.factor])) {
      levels(data[, within.factor]) <- make.names(
        levels(data[, within.factor]),
        unique = TRUE
      )
    } else {
      data[, within.factor] <-
        factor(
          as.character(data[, within.factor]),
          levels = unique(as.character(data[, within.factor])),
          labels = make.names(
            unique(as.character(data[, within.factor])),
            unique = TRUE
          )
        )
    }
  }

  # Check if each id is in only one between subjects cell.
  between.factors <- between[vapply(
    data[, between, drop = FALSE],
    is.factor,
    TRUE
  )]
  if (length(between.factors) > 0) {
    split.data <- split(data, lapply(between.factors, function(x) data[, x]))
    ids.per.condition <-
      lapply(split.data, function(x) unique(as.character(x[, id])))
    ids.in.more.condition <-
      unique(unlist(
        lapply(seq_along(ids.per.condition), function(x) {
          unique(unlist(
            lapply(
              ids.per.condition[-x],
              function(y, z = ids.per.condition[[x]]) {
                intersect(z, y)
              }
            )
          ))
        })
      ))
    if (length(ids.in.more.condition) > 0) {
      stop(
        paste0(
          "Following ids are in more than one between subjects condition:\n",
          paste0(ids.in.more.condition, collapse = ", ")
        )
      )
    }
  }

  ## check for structurally missing data
  # within-subjects
  if ((length(within) > 0) && any(table(data[within]) == 0)) {
    stop(
      "Empty cells in within-subjects design",
      " (i.e., bad data structure).\n",
      "",
      paste0("table(data[", deparse(within), "])"),
      "\n# ",
      paste(utils::capture.output(table(data[within])), collapse = "\n# "),
      call. = FALSE
    )
  }

  # Is fun_aggregate NULL and aggregation necessary?
  if (is.null(fun_aggregate)) {
    if (
      any(
        xtabs(
          as.formula(paste0("~", id.escaped, if (length(within) > 0) "+", rh1)),
          data = data
        ) >
          1
      )
    ) {
      warning(
        "More than one observation per design cell, aggregating data using `fun_aggregate = mean`.\nTo turn off this warning, pass `fun_aggregate = mean` explicitly.",
        call. = FALSE
      )
      fun_aggregate <- mean
    }
  }

  # prepare the data:
  tmp.dat <- do.call(
    dcast,
    args = c(
      data = list(data),
      formula = as.formula(paste(
        lh1,
        if (length(within) > 0) {
          rh1
        } else {
          "."
        },
        sep = "~"
      )),
      fun.aggregate = fun_aggregate,
      dots,
      value.var = dv
    )
  )

  # check for missing values:
  if (any(is.na(tmp.dat))) {
    missing.values <- apply(tmp.dat, 1, function(x) any(is.na(x)))
    missing_ids <- unique(tmp.dat[missing.values, 1])
    warning(
      paste0(
        "Missing values for ",
        length(missing_ids),
        " ID(s), which were removed before analysis:\n",
        if (length(missing_ids) < 10) {
          paste0(missing_ids, collapse = ", ")
        } else {
          paste0(
            paste0(missing_ids[1:10], collapse = ", "),
            ", ... [showing first 10 only]"
          )
        },
        "\nBelow the first few rows (in wide format) of the removed cases with missing data.\n  ",
        paste(
          utils::capture.output(head(tmp.dat[missing.values, ])),
          collapse = "\n# "
        )
      ),
      call. = FALSE
    )
    tmp.dat <- tmp.dat[!missing.values, ]
    data <- data[!(data[, id] %in% missing_ids), ]
    if ((nrow(data) == 0) | (nrow(tmp.dat) == 0)) {
      stop(
        "No observations remain after removing missing values.",
        "\n  Try adding to ANOVA call: na.rm = TRUE",
        call. = FALSE
      )
    }
  } else {
    missing_ids <- NULL
  }
  # if (length(between_nn) > 0 && any(table(data[between_nn]) == 0)) {
  #   stop("Empty cells in between-subjects design ",
  #        " (i.e., bad data structure).\n",
  #        "", paste0("table(data[", deparse(between_nn), "])"), "\n# ",
  #        paste(utils::capture.output(table(data[between_nn])), collapse = "\n# "),
  #        call. = FALSE)
  # }

  #   if (length(between) > 0) {
  #     n_data_points <- xtabs(as.formula(paste("~", paste(between, collapse = "+"))), data = tmp.dat)
  #     if (any(n_data_points == 0)) warning("Some cells of the fully crossed between-subjects design are empty. A full model might not be estimable.")
  #   }

  # marginals: (disabled in April 2015), dat.ret is now used for aov()
  dat.ret <- do.call(
    dcast,
    args = c(
      data = list(data),
      formula = as.formula(paste0(
        mypaste(
          lh1,
          if (length(within) > 0) {
            rh1
          } else {
            NULL
          },
          sep = "+"
        ),
        "~."
      )),
      fun.aggregate = fun_aggregate,
      dots,
      value.var = dv
    )
  )
  colnames(dat.ret)[length(colnames(dat.ret))] <- dv
  if (
    suppressWarnings(
      !isTRUE(
        all.equal(
          target = data[, c(id, between, within, dv)],
          current = dat.ret[, c(id, between, within, dv)],
          check.attributes = FALSE
        )
      )
    )
  ) {
    data_changed <- TRUE
  } else {
    data_changed <- FALSE
  }

  if (length(between) > 0) {
    tmp.dat <- check_contrasts(
      data = tmp.dat,
      factors = between,
      check_contrasts = TRUE,
      type = type
    )
  }

  if (return %in% c("aov")) {
    include_aov <- TRUE
  }
  if (include_aov) {
    if (TRUE) {
      ## was: check_contrasts
      factor_vars <-
        vapply(dat.ret[, c(within, between), drop = FALSE], is.factor, NA)
      contrasts <- as.list(rep("contr.sum", sum(factor_vars)))
      names(contrasts) <- c(within, between)[factor_vars]
    }
    tmp_formula <- formula(paste(
      dv.escaped,
      "~",
      if (length(within) > 0) {
        paste(
          if (rh2 == "1") {
            paste(within.escaped, collapse = "*")
          } else {
            paste("(", rh2, ")*(", paste(within.escaped, collapse = "*"), ")")
          },
          "+Error(",
          id.escaped,
          "/(",
          paste(within.escaped, collapse = "*"),
          "))"
        )
      } else {
        rh2
      }
    ))
    aov <- aov(tmp_formula, data = dat.ret, contrasts = contrasts)
  } else {
    aov <- NULL
  }
  if (return == "aov") {
    return(aov)
  }
  data.l <- list(long = dat.ret, wide = tmp.dat)
  if (return == "data") {
    return(tmp.dat)
  }

  # branching based on type of ANOVA
  if (length(within) > 0) {
    # if within-subject factors are present:
    # make idata argument
    if (length(within) > 1) {
      within.levels <- lapply(lapply(data[, within], levels), factor)
      idata <- rev(expand.grid(rev(within.levels)))
    } else {
      idata <- data.frame(levels(data[, within]), stringsAsFactors = TRUE)
      colnames(idata) <- within
    }
    tmp.lm <- do.call(
      "lm",
      list(
        formula = as.formula(paste0(
          "cbind(",
          paste0(
            colnames(
              tmp.dat[-(seq_along(c(id, between)))]
            ),
            collapse = ", "
          ),
          ") ~ ",
          rh2
        )),
        data = tmp.dat
      )
    )
    if (any(is.na(coef(tmp.lm)))) {
      between_design_error(
        data = tmp.dat,
        between = between,
        bad_vars = names(which(apply(is.na(coef(tmp.lm)), 1, any)))
      )
    }
    if (return == "lm") {
      return(tmp.lm)
    }
    Anova.out <- Anova(
      tmp.lm,
      idata = idata,
      idesign = as.formula(paste0("~", rh3)),
      type = type
    )
    data.l <- c(data.l, idata = list(idata))
  } else {
    # if NO within-subject factors are present (i.e., purely between ANOVA):
    colnames(tmp.dat)[ncol(tmp.dat)] <- "dv"
    tmp.lm <- do.call(
      "lm",
      list(formula = as.formula(paste0("dv ~ ", rh2)), data = tmp.dat)
    )
    if (any(is.na(coef(tmp.lm)))) {
      between_design_error(
        data = tmp.dat,
        between = between,
        bad_vars = names(which(is.na(coef(tmp.lm))))
      )
    }
    if (return == "lm") {
      return(tmp.lm)
    }
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
    attr(afex_aov, "within") <-
      if (length(within) > 0) {
        lapply(data[, within, drop = FALSE], levels)
      } else {
        list()
      }
    attr(afex_aov, "between") <-
      if (length(between) > 0) {
        lapply(data[, between, drop = FALSE], levels)
      } else {
        list()
      }
    attr(afex_aov, "type") <- type
    attr(afex_aov, "transf") <- transf
    attr(afex_aov, "incomplete_cases") <- missing_ids
    attr(afex_aov, "data_changed") <- data_changed
    afex_aov$anova_table <-
      do.call(
        "anova",
        args = c(
          object = list(afex_aov),
          observed = list(observed),
          anova_table
        )
      )
    return(afex_aov)
  }
  if (return == "Anova") {
    return(Anova.out)
  } else if (return == "univariate") {
    if (inherits(Anova.out, "Anova.mlm")) {
      return(summary(Anova.out, multivariate = FALSE))
    } else {
      return(Anova.out)
    }
  } else if (return == "nice") {
    afex_aov <- list(
      anova_table = NULL,
      Anova = Anova.out
    )
    class(afex_aov) <- "afex_aov"
    attr(afex_aov, "dv") <- dv
    attr(afex_aov, "id") <- id
    attr(afex_aov, "within") <-
      if (length(within) > 0) {
        lapply(data[, within, drop = FALSE], levels)
      } else {
        list()
      }
    attr(afex_aov, "between") <-
      if (length(between) > 0) {
        lapply(data[, between, drop = FALSE], levels)
      } else {
        list()
      }
    attr(afex_aov, "type") <- type
    afex_aov$anova_table <-
      do.call(
        "anova",
        args = c(
          object = list(afex_aov),
          observed = list(observed),
          anova_table
        )
      )
    return(do.call(
      "nice",
      args = c(object = list(afex_aov), observed = list(observed), anova_table)
    ))
  }
}

#' @describeIn aov_car Allows definition of ANOVA-model using
#'   \code{lme4::lmer}-like Syntax (but still fits a standard ANOVA).
#' @export
aov_4 <- function(
  formula,
  data,
  observed = NULL,
  fun_aggregate = NULL,
  type = afex_options("type"),
  factorize = afex_options("factorize"),
  return = afex_options("return_aov"),
  anova_table = list(),
  include_aov = afex_options("include_aov"),
  ...,
  print.formula = FALSE
) {
  barterms <- findbars(formula)
  if (length(barterms) > 1L) {
    stop("aov_4 only allows one random effect term")
  }
  if (length(barterms) < 1L) {
    stop("aov_4() requires one random-effect term in formula")
  }
  within <- all.vars(barterms[[1]][[2]])
  id <- all.vars(barterms[[1]][[3]])

  id <- escape_vars(id)
  within <- escape_vars(within)

  error <- paste0(
    " + Error(",
    id,
    if (length(within) > 0) "/(" else "",
    paste0(within, collapse = " * "),
    if (length(within) > 0) ")" else "",
    ")"
  )
  lh <- as.character(nobars(formula))
  if (length(lh) == 1) {
    dv <- lh
    rh <- "1"
  } else {
    dv <- lh[2]
    rh <- lh[3]
  }
  formula <- paste0(dv, " ~ ", rh, error)
  if (print.formula) {
    message(paste0("Formula send to aov_car: ", formula))
  }
  aov_car(
    formula = as.formula(formula),
    data = data,
    fun_aggregate = fun_aggregate,
    type = type,
    return = return,
    factorize = factorize,
    observed = observed,
    anova_table = anova_table,
    include_aov = include_aov,
    ...
  )
}


#' @describeIn aov_car Allows definition of ANOVA-model using character strings.
#' @export
aov_ez <- function(
  id,
  dv,
  data,
  between = NULL,
  within = NULL,
  covariate = NULL,
  observed = NULL,
  fun_aggregate = NULL,
  transformation,
  type = afex_options("type"),
  factorize = afex_options("factorize"),
  return = afex_options("return_aov"),
  anova_table = list(),
  include_aov = afex_options("include_aov"),
  ...,
  print.formula = FALSE
) {
  if (is.null(between) & is.null(within)) {
    stop("Either between or within need to be non-NULL!")
  }
  if (!is.null(covariate)) {
    covariate <- escape_vars(covariate)
    covariate <- paste0(covariate, collapse = "+")
  }
  id <- escape_vars(id)
  dv <- escape_vars(dv)
  between <- escape_vars(between)
  within <- escape_vars(within)
  rh <- if (!is.null(between) || !is.null(covariate)) {
    mypaste(
      if (!is.null(between)) paste0(between, collapse = " * ") else NULL,
      covariate,
      sep = "+"
    )
  } else {
    "1"
  }
  error <- paste0(
    " + Error(",
    id,
    if (!is.null(within)) "/(" else "",
    paste0(within, collapse = " * "),
    if (length(within) > 0) ")" else "",
    ")"
  )
  if (!missing(transformation)) {
    dv <- paste0(transformation, "(", dv, ")")
  }
  formula <- paste0(dv, " ~ ", rh, error)
  if (print.formula) {
    message(paste0("Formula send to aov_car: ", formula))
  }
  aov_car(
    formula = as.formula(formula),
    data = data,
    fun_aggregate = fun_aggregate,
    type = type,
    return = return,
    factorize = factorize,
    observed = observed,
    anova_table = anova_table,
    include_aov = include_aov,
    ...
  )
}

between_design_error <- function(data, between, bad_vars) {
  ## check between-subjects design for completeness
  ## select all factor variables
  between_nn <- between[!vapply(data[between], is.numeric, NA)]
  stop(
    "Rank deficient model matrix; insufficient data to estimate full model.\n",
    "Model coefficient(s) estimated as NA: ",
    paste(bad_vars, collapse = ", "),
    "\nLikely empty cells in between-subjects design ",
    "(i.e., bad data structure).\n",
    "",
    paste0("table(data[", deparse(between_nn), "])"),
    "\n# ",
    paste(utils::capture.output(table(data[between_nn])), collapse = "\n# "),
    call. = FALSE
  )
}
