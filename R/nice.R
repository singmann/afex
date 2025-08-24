#' Make nice ANOVA table for printing.
#'
#' This generic function produces a nice ANOVA table for printing for objects of
#' class. \code{nice_anova} takes an object from \code{\link[car]{Anova}}
#' possible created by the convenience functions \code{\link{aov_ez}} or
#' \code{\link{aov_car}}. When within-subject factors are present, either
#' sphericity corrected or uncorrected degrees of freedom can be reported.
#'
#'
#' @param object,x An object of class \code{"afex_aov"} (see
#'   \code{\link{aov_car}}) or of class \code{"mixed"} (see \code{\link{mixed}})
#'   as returned from the \pkg{afex} functions. Alternatively, an object of
#'   class \code{"Anova.mlm"} or \code{"anova"} as returned from
#'   \code{\link[car]{Anova}}.
#' @param es Effect Size to be reported. The default is given by
#'   \code{afex_options("es_aov")}, which is initially set to \code{"ges"}
#'   (i.e., reporting generalized eta-squared, see details). Also supported is
#'   partial eta-squared (\code{"pes"}) or \code{"none"}.
#' @param observed character vector referring to the observed (i.e., non
#'   manipulated) variables/effects in the design. Important for calculation of
#'   generalized eta-squared (ignored if \code{es} is not \code{"ges"}), see
#'   details.
#' @param correction Character. Which sphericity correction of the degrees of
#'   freedom should be reported for the within-subject factors.  The default is
#'   given by \code{afex_options("correction_aov")}, which is initially set to
#'   \code{"GG"} corresponding to the Greenhouse-Geisser correction. Possible
#'   values are \code{"GG"}, \code{"HF"} (i.e., Hyunh-Feldt correction), and
#'   \code{"none"} (i.e., no correction).
#' @param p_adjust_method \code{character} indicating if p-values for individual
#'   effects should be adjusted for multiple comparisons (see
#'   \link[stats]{p.adjust} and details). The default \code{NULL} corresponds to
#'   no adjustment.
#' @param sig_symbols Character. What should be the symbols designating
#'   significance? When entering an vector with \code{length(sig.symbol) < 4}
#'   only those elements of the default (\code{c(" +", " *", " **", " ***")})
#'   will be replaced. \code{sig_symbols = ""} will display the stars but not
#'   the \code{+}, \code{sig_symbols = rep("", 4)} will display no symbols. The
#'   default is given by \code{afex_options("sig_symbols")}.
#' @param MSE logical. Should the column containing the Mean Sqaured Error (MSE)
#'   be displayed? Default is \code{TRUE}.
#' @param intercept logical. Should intercept (if present) be included in the
#'   ANOVA table? Default is \code{FALSE} which hides the intercept.
#' @param round_ps Function that should be used for rounding p-values. The
#'   default is given by \code{afex_options("round_ps")}.
#' @param sig.symbols deprecated argument, only for backwards compatibility, use
#'   \code{"sig_symbols"} instead.
#' @param ... currently ignored.
#'
#' @return A \code{data.frame} of class \code{nice_table} with the ANOVA table
#'   consisting of characters. The columns that are always present are:
#'   \code{Effect}, \code{df} (degrees of freedom), \code{F}, and \code{p}.
#'
#'   \code{ges} contains the generalized eta-squared effect size measure
#'   (Bakeman, 2005), \code{pes} contains partial eta-squared (if requested).
#'
#' @details The returned \code{data.frame} is print-ready when adding to a
#'   document with proper methods. Either directly via \pkg{knitr} or similar
#'   approaches such as via package \pkg{xtable} (nowadays \pkg{knitr} is
#'   probably the best approach, see \href{https://yihui.org/knitr/}{here}).
#'   \pkg{xtable} converts a \code{data.frame} into LaTeX code with many
#'   possible options (e.g., allowing for \code{"longtable"} or
#'   \code{"sidewaystable"}), see \code{\link[xtable]{xtable}} and
#'   \code{\link[xtable]{print.xtable}}. See Examples.
#'
#'   Conversion functions to other formats (such as HTML, ODF, or Word) can be
#'   found at the
#'   \href{https://CRAN.R-project.org/view=ReproducibleResearch}{Reproducible
#'   Research Task View}.
#'
#'   The default reports generalized eta squared (Olejnik & Algina, 2003), the
#'   "recommended effect size for repeated measured designs" (Bakeman, 2005).
#'   Note that it is important that all measured variables (as opposed to
#'   experimentally manipulated variables), such as e.g., age, gender, weight,
#'   ..., must be declared via \code{observed} to obtain the correct effect size
#'   estimate. Partial eta squared (\code{"pes"}) does not require this.
#'
#'   Exploratory ANOVA, for which no detailed hypotheses have been specified a
#'   priori, harbor a multiple comparison problem (Cramer et al., 2015). To
#'   avoid an inflation of familywise Type I error rate, results need to be
#'   corrected for multiple comparisons using \code{p_adjust_method}.
#'   \code{p_adjust_method} defaults to the method specified in the call to
#'   \code{\link{aov_car}} in \code{anova_table}. If no method was specified and
#'   \code{p_adjust_method = NULL} p-values are not adjusted.
#'
#' @seealso \code{\link{aov_ez}} and \code{\link{aov_car}} are the convenience
#'   functions to create the object appropriate for \code{nice_anova}.
#'
#' @author The code for calculating generalized eta-squared was written by Mike
#'   Lawrence.\cr Everything else was written by Henrik Singmann.
#'
#' @references Bakeman, R. (2005). Recommended effect size statistics for
#'   repeated measures designs. \emph{Behavior Research Methods}, 37(3),
#'   379-384. \doi{10.3758/BF03192707}
#'
#'   Cramer, A. O. J., van Ravenzwaaij, D., Matzke, D., Steingroever, H.,
#'   Wetzels, R., Grasman, R. P. P. P., ... Wagenmakers, E.-J. (2015). Hidden
#'   multiplicity in exploratory multiway ANOVA: Prevalence and remedies.
#'   \emph{Psychonomic Bulletin & Review}, 1-8.
#'   \doi{10.3758/s13423-015-0913-5}
#'
#'   Olejnik, S., & Algina, J. (2003). Generalized Eta and Omega Squared
#'   Statistics: Measures of Effect Size for Some Common Research Designs.
#'   \emph{Psychological Methods}, 8(4), 434-447.
#'   \doi{10.1037/1082-989X.8.4.434}
#'
#' @name nice
#' @importFrom stats anova
#' @encoding UTF-8
#'
#' @example examples/examples.nice.R
#'
#' @export nice
nice <- function(object, ...) UseMethod("nice", object)


#' @rdname nice
#' @method nice afex_aov
#' @export
nice.afex_aov <- function(
  object,
  es = attr(object$anova_table, "es"),
  observed = attr(object$anova_table, "observed"),
  correction = attr(object$anova_table, "correction"),
  MSE = NULL,
  intercept = NULL,
  p_adjust_method = attr(object$anova_table, "p_adjust_method"),
  sig_symbols = attr(object$anova_table, "sig_symbols"),
  round_ps = attr(object$anova_table, "round_ps"),
  ...
) {
  # if(is.null(es)) { # Defaults to afex_options("es") because of default set in anova.afex_aov
  #   es <- c("pes", "ges")[c("pes", "ges") %in% colnames(object$anova_table)]
  # }
  dots <- list(...)
  if (is.null(MSE)) {
    # Defaults to TRUE because of default set in anova.afex_aov
    MSE <- "MSE" %in% colnames(object$anova_table)
  }
  if (is.null(intercept)) {
    # Defaults to FALSE because of default set in anova.afex_aov
    intercept <- "(Intercept)" %in% rownames(object$anova_table)
  }
  if ("sig.symbols" %in% names(dots)) {
    #(!missing(sig.symbols)) {
    warn_deprecated_arg("sig.symbols", "sig_symbols")
    sig_symbols <- dots$sig.symbols
  }
  if ("p.adjust.method" %in% names(dots)) {
    #(!missing(sig.symbols)) {
    warn_deprecated_arg("p.adjust.method", "p_adjust_method")
    p_adjust_method <- dots$p.adjust.method
  }
  anova_table <- as.data.frame(anova(
    object,
    es = es,
    observed = observed,
    correction = correction,
    MSE = MSE,
    intercept = intercept,
    p_adjust_method = p_adjust_method
  ))
  nice.anova(
    anova_table,
    MSE = MSE,
    intercept = intercept,
    sig_symbols = sig_symbols,
    round_ps = round_ps
  )
}

#' @rdname nice
#' @method nice anova
#' @export
nice.anova <- function(
  object,
  MSE = NULL,
  intercept = NULL,
  sig_symbols = attr(object, "sig_symbols"),
  round_ps = attr(object, "round_ps"),
  sig.symbols,
  ...
) {
  dots <- list(...)
  if (is.null(MSE)) {
    # Defaults to TRUE because of default set in anova.afex_aov
    MSE <- "MSE" %in% colnames(object)
  }
  if (is.null(intercept)) {
    # Defaults to FALSE because of default set in anova.afex_aov
    intercept <- "(Intercept)" %in% rownames(object)
  }
  if ("sig.symbols" %in% names(dots)) {
    #(!missing(sig.symbols)) {
    warn_deprecated_arg("sig.symbols", "sig_symbols")
    sig_symbols <- dots$sig.symbols
  }
  if (is.null(sig_symbols)) {
    sig_symbols <- afex_options("sig_symbols")
  }
  if (is.null(round_ps)) {
    round_ps <- afex_options("round_ps")
  }

  # internal functions:
  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
    abs(x - round(x)) < tol
  }
  make.fs <- function(anova, symbols) {
    ifelse(
      anova[["Pr(>F)"]] < 0.001,
      paste0(
        formatC(
          anova[["F"]],
          digits = 2,
          format = "f"
        ),
        symbols[4]
      ),
      ifelse(
        anova[["Pr(>F)"]] < 0.01,
        paste0(
          formatC(
            anova[["F"]],
            digits = 2,
            format = "f"
          ),
          symbols[3]
        ),
        ifelse(
          anova[["Pr(>F)"]] < 0.05,
          paste0(
            formatC(
              anova[["F"]],
              digits = 2,
              format = "f"
            ),
            symbols[2]
          ),
          ifelse(
            anova[["Pr(>F)"]] < 0.1,
            paste0(
              formatC(
                anova[["F"]],
                digits = 2,
                format = "f"
              ),
              symbols[1]
            ),
            formatC(anova[["F"]], digits = 2, format = "f")
          )
        )
      )
    )
  }
  anova_table <- object
  anova_table[, "df"] <- paste(
    ifelse(
      is.wholenumber(anova_table[, "num Df"]),
      anova_table[, "num Df"],
      formatC(anova_table[, "num Df"], digits = 2, format = "f")
    ),
    ifelse(
      is.wholenumber(anova_table[, "den Df"]),
      anova_table[, "den Df"],
      formatC(anova_table[, "den Df"], digits = 2, format = "f")
    ),
    sep = ", "
  )
  symbols.use <- c(" +", " *", " **", " ***")
  symbols.use[seq_along(sig_symbols)] <- sig_symbols
  df.out <- data.frame(
    Effect = row.names(anova_table),
    df = anova_table[, "df"],
    stringsAsFactors = FALSE
  )
  if (MSE) {
    df.out <- cbind(
      df.out,
      data.frame(
        MSE = formatC(anova_table[, "MSE"], digits = 2, format = "f"),
        stringsAsFactors = FALSE
      )
    )
  }
  df.out <- cbind(
    df.out,
    data.frame(F = make.fs(anova_table, symbols.use), stringsAsFactors = FALSE)
  )
  if (!is.null(anova_table$ges)) {
    df.out$ges <- round_ps(anova_table$ges)
  }
  if (!is.null(anova_table$pes)) {
    df.out$pes <- round_ps(anova_table$pes)
  }
  df.out$p.value <- round_ps(anova_table[, "Pr(>F)"])
  if (!intercept) {
    if (df.out[1, 1] == "(Intercept)") {
      df.out <- df.out[-1, , drop = FALSE]
    }
  }
  rownames(df.out) <- NULL
  attr(df.out, "heading") <- attr(object, "heading")
  attr(df.out, "p_adjust_method") <- attr(object, "p_adjust_method")
  attr(df.out, "correction") <- attr(object, "correction")
  attr(df.out, "observed") <- attr(object, "observed")
  attr(df.out, "es") <- attr(object, "es")
  attr(df.out, "sig_symbols") <- symbols.use
  attr(df.out, "round_ps") <- round_ps
  class(df.out) <- c("nice_table", class(df.out))
  df.out
}

make.stat <- function(anova, stat, symbols) {
  out <- ifelse(
    anova[[paste0("Pr(>", stat, ")")]] < 0.001,
    paste0(formatC(anova[[stat]], digits = 2, format = "f"), symbols[4]),
    ifelse(
      anova[[paste0("Pr(>", stat, ")")]] < 0.01,
      paste0(formatC(anova[[stat]], digits = 2, format = "f"), symbols[3]),
      ifelse(
        anova[[paste0("Pr(>", stat, ")")]] < 0.05,
        paste0(formatC(anova[[stat]], digits = 2, format = "f"), symbols[2]),
        ifelse(
          anova[[paste0("Pr(>", stat, ")")]] < 0.1,
          paste0(formatC(anova[[stat]], digits = 2, format = "f"), symbols[1]),
          formatC(anova[[stat]], digits = 2, format = "f")
        )
      )
    )
  )
  out[is.na(anova[[paste0("Pr(>", stat, ")")]])] <- formatC(
    anova[[stat]][is.na(anova[[paste0("Pr(>", stat, ")")]])],
    digits = 2,
    format = "f"
  )
  out
}
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
  abs(x - round(x)) < tol
}


#' @rdname nice
#' @method nice mixed
#' @export
nice.mixed <- function(
  object,
  sig_symbols = attr(object$anova_table, "sig_symbols"),
  round_ps = attr(object$anova_table, "round_ps"),
  ...
) {
  anova_table <- object$anova_table
  dots <- list(...)
  if ("sig.symbols" %in% names(dots)) {
    #(!missing(sig.symbols)) {
    warn_deprecated_arg("sig.symbols", "sig_symbols")
    sig_symbols <- dots$sig.symbols
  }
  if (is.null(sig_symbols)) {
    sig_symbols <- afex_options("sig_symbols")
  }
  if (is.null(round_ps)) {
    round_ps <- afex_options("round_ps")
  }

  symbols.use <- c(" +", " *", " **", " ***")
  symbols.use[seq_along(sig_symbols)] <- sig_symbols

  if (is.null(attr(object, "method"))) {
    df.out <- object[[1]]
    warning(
      "mixed object was created with old version of afex, table not nicely formatted."
    )
  } else if (attr(object, "method") %in% c("KR", "S", "nested-KR")) {
    anova_table[, "df"] <- paste(
      ifelse(
        is.wholenumber(anova_table[, "num Df"]),
        round(anova_table[, "num Df"]),
        formatC(anova_table[, "num Df"], digits = 2, format = "f")
      ),
      ifelse(
        is.wholenumber(anova_table[, "den Df"]),
        round(anova_table[, "den Df"]),
        formatC(anova_table[, "den Df"], digits = 2, format = "f")
      ),
      sep = ", "
    )
    if ("F.scaling" %in% anova_table) {
      df.out <- data.frame(
        Effect = row.names(anova_table),
        df = anova_table[, "df"],
        "F.scaling" = formatC(
          anova_table[, "F.scaling"],
          digits = 2,
          format = "f"
        ),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    } else {
      df.out <- data.frame(
        Effect = row.names(anova_table),
        df = anova_table[, "df"],
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }
    df.out <- cbind(
      df.out,
      data.frame(
        F = make.stat(anova_table, stat = "F", symbols.use),
        stringsAsFactors = FALSE
      )
    )
    df.out$p.value <- round_ps(anova_table[, "Pr(>F)"])
  } else if (attr(object, "method") == "PB") {
    anova_table[, "Pr(>Chisq)"] <- anova_table[, "Pr(>PB)"]
    df.out <- data.frame(
      Effect = row.names(anova_table),
      df = anova_table[, "Chi Df"],
      Chisq = make.stat(anova_table, stat = "Chisq", symbols.use),
      p.value = round_ps(anova_table[, "Pr(>Chisq)"]),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  } else if (attr(object, "method") == "LRT") {
    df.out <- data.frame(
      Effect = row.names(anova_table),
      df = anova_table[, "Chi Df"],
      Chisq = make.stat(anova_table, stat = "Chisq", symbols.use),
      p.value = round_ps(anova_table[, "Pr(>Chisq)"]),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  } else {
    stop("method of mixed object not supported.")
  }
  rownames(df.out) <- NULL
  attr(df.out, "heading") <- attr(anova_table, "heading")
  attr(df.out, "sig_symbols") <- symbols.use
  attr(df.out, "round_ps") <- round_ps
  class(df.out) <- c("nice_table", class(df.out))
  df.out
}


#' @rdname nice
#' @method print nice_table
#' @export
print.nice_table <- function(x, ...) {
  if (!is.null(heading <- attr(x, "heading"))) {
    cat(heading, sep = "\n")
  }
  print.data.frame(x)
  if (!is.null(attr(x, "sig_symbols"))) {
    print_legend(x)
  }
  if (
    !is.null(correction_method <- attr(x, "correction")) &&
      correction_method != "none"
  ) {
    cat("\nSphericity correction method:", correction_method, "\n")
  }
  invisible(x)
}
