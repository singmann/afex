#' Make nice ANOVA table for printing.
#'
#' This generic function produces a nice ANOVA table for printin for objects of class. \code{nice_anova} takes an object from \code{\link[car]{Anova}} possible created by the convenience functions \code{\link{aov_ez}} or \code{\link{aov_car}}. When within-subject factors are present, either sphericity corrected or uncorrected degrees of freedom can be reported.
#' 
#'
#' @param object An object of class \code{"Anova.mlm"} or \code{"anova"} as returned from \code{\link[car]{Anova}} or the \pkg{afex} ANOVA functions (see \code{\link{aov_car}}).
#' @param es Effect Size to be reported. The default is given by \code{afex_options("es_aov")}, which is initially set to \code{"ges"} (i.e., reporting generalized eta-squared, see details). Also supported is partial eta-squared (\code{"pes"}) or \code{"none"}.
#' @param observed character vector referring to the observed (i.e., non manipulated) variables/effects in the design. Important for calculation of generalized eta-squared (ignored if \code{es} is not \code{"ges"}), see details.
#' @param correction Character. Which sphericity correction of the degrees of freedom should be reported for the within-subject factors.  The default is given by \code{afex_options("correction_aov")}, which is initially set to \code{"GG"} corresponding to the Greenhouse-Geisser correction. Possible values are \code{"GG"}, \code{"HF"} (i.e., Hyunh-Feldt correction), and \code{"none"} (i.e., no correction).
#' @param sig.symbols Character. What should be the symbols designating significance? When entering an vector with \code{length(sig.symbol) < 4} only those elements of the default (\code{c(" +", " *", " **", " ***")}) will be replaced. \code{sig.symbols = ""} will display the stars but not the \code{+}, \code{sig.symbols = rep("", 4)} will display no symbols.
#' @param MSE logical. Should the column containing the Mean Sqaured Error (MSE) be displayed? Default is \code{TRUE}.
#' @param intercept logical. Should intercept (if present) be included in the ANOVA table? Default is \code{FALSE} which hides the intercept.
#' @param ... currently ignored.
#'
#' @return A \code{data.frame} with the ANOVA table consisting of characters. The columns that are always present are: \code{Effect}, \code{df} (degrees of freedom), \code{F}, and \code{p}.
#'
#' \code{ges} contains the generalized eta-squared effect size measure (Bakeman, 2005), \code{pes} contains partial eta-squared (if requested).
#'
#' @details The returned \code{data.frame} is print-ready when adding to a document with proper methods. I recommend \pkg{ascii} and \pkg{xtable}. \pkg{ascii} provides conversion to \href{http://www.methods.co.nz/asciidoc/}{AsciiDoc} but most notably to \href{http://orgmode.org/}{org-mode} (see \code{\link[ascii]{ascii}} and \code{\link[ascii]{print-ascii}}). \pkg{xtable} converts a \code{data.frame} into LaTeX code with many possible options (e.g., allowing for \code{"longtable"} or \code{"sidewaystable"}), see \code{\link[xtable]{xtable}} and \code{\link[xtable]{print.xtable}}. See Examples.
#'
#' Conversion functions to other formats (such as HTML, ODF, or Word) can be found at the \href{http://cran.r-project.org/web/views/ReproducibleResearch.html}{Reproducible Research Task View}.
#'
#' The default reports generalized eta squared (Olejnik & Algina, 2003), the "recommended effect size for repeated measured designs" (Bakeman, 2005). Note that it is important that all measured variables (as opposed to experimentally manipulated variables), such as e.g., age, gender, weight, ..., must be declared via \code{observed} to obtain the correct effect size estimate. Partial eta squared (\code{"pes"}) does not require this.
#'
#' @seealso \code{\link{aov_ez}} and \code{\link{aov_car}} are the convenience functions to create the object appropriate for \code{nice_anova}.
#'
#' @author The code for calculating generalized eta-squared was written by Mike Lawrence.\cr Everything else was written by Henrik Singmann.
#'
#' @references Bakeman, R. (2005). Recommended effect size statistics for repeated measures designs. \emph{Behavior Research Methods}, 37(3), 379-384. doi:10.3758/BF03192707

#'
#' Olejnik, S., & Algina, J. (2003). Generalized Eta and Omega Squared Statistics: Measures of Effect Size for Some Common Research Designs. \emph{Psychological Methods}, 8(4), 434-447. doi:10.1037/1082-989X.8.4.434
#' 
#' @name nice
#' @export nice
#' 
#' @encoding UTF-8
#'
#' @examples
#'
#' ## example from Olejnik & Algina (2003)
#' # "Repeated Measures Design" (pp. 439):
#' data(md_12.1)
#' # create object of class afex_aov:
#' rmd <- aov_ez("id", "rt", md_12.1, within = c("angle", "noise"))
#' # use different es:
#' nice(rmd, es = "pes") # noise: .82
#' nice(rmd, es = "ges") # noise: .39
#'
#' # exampel using obk.long (see ?obk.long), a long version of the OBrienKaiser dataset from car.
#' data(obk.long)
#' # create object of class afex_aov:
#' tmp.aov <- aov_car(value ~ treatment * gender + Error(id/phase*hour), data = obk.long)
#' 
#' nice(tmp.aov, observed = "gender")
#' 
#' nice(tmp.aov, observed = "gender", sig.symbol = rep("", 4))
#' 
#' \dontrun{
#' # use package ascii or xtable for formatting of tables ready for printing.
#' 
#' full <- nice(tmp.aov, observed = "gender")
#' 
#' require(ascii)
#' print(ascii(full, include.rownames = FALSE, caption = "ANOVA 1"), type = "org")
#' 
#' require(xtable)
#' print.xtable(xtable(full, caption = "ANOVA 2"), include.rownames = FALSE)
#' }
#' 
#' 
nice <- function(object, ...) UseMethod("nice", object)


#' @rdname nice
#' @method nice afex_aov
#' @export
nice.afex_aov <- function(object, es = afex_options("es_aov"), observed = NULL, correction = afex_options("correction_aov"), MSE = TRUE, intercept = FALSE, sig.symbols = c(" +", " *", " **", " ***"), ...) { 
  anova_table <- as.data.frame(anova(object, es = es, observed = observed, correction = correction, MSE = MSE, intercept = intercept))
  nice.anova(anova_table, MSE = MSE, intercept = intercept, sig.symbols = sig.symbols)
}

#' @rdname nice
#' @method nice anova
#' @export
nice.anova <- function(object, MSE = TRUE, intercept = FALSE, sig.symbols = c(" +", " *", " **", " ***"), ...) {
  # internal functions:
  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
  make.fs <- function(anova, symbols) {
    ifelse(anova[["Pr(>F)"]] < 0.001, str_c(formatC(anova[["F"]], digits = 2, format = "f"), symbols[4]), 
           ifelse(anova[["Pr(>F)"]] < 0.01, str_c(formatC(anova[["F"]], digits = 2, format = "f"), symbols[3]), 
                  ifelse(anova[["Pr(>F)"]] < 0.05, str_c(formatC(anova[["F"]], digits = 2, format = "f"), symbols[2]), 
                         ifelse(anova[["Pr(>F)"]] < 0.1, str_c(formatC(anova[["F"]], digits = 2, format = "f"), symbols[1]), formatC(anova[["F"]], digits = 2, format = "f")))))
  }
  anova_table <- object
  anova_table[,"df"] <- paste(ifelse(is.wholenumber(anova_table[,"num Df"]), anova_table[,"num Df"], formatC(anova_table[,"num Df"], digits = 2, format = "f")),  ifelse(is.wholenumber(anova_table[,"den Df"]),anova_table[,"den Df"], formatC(anova_table[,"den Df"], digits = 2, format = "f")), sep = ", ")
  symbols.use <-  c(" +", " *", " **", " ***")
  symbols.use[seq_along(sig.symbols)] <- sig.symbols
  df.out <- data.frame(Effect = row.names(anova_table), df = anova_table[,"df"], stringsAsFactors = FALSE)
  if (!is.null(anova_table$MSE)) df.out <- cbind(df.out, data.frame(MSE = formatC(anova_table[,"MSE"], digits = 2, format = "f"), stringsAsFactors = FALSE))  
  df.out <- cbind(df.out, data.frame(F = make.fs(anova_table, symbols.use), stringsAsFactors = FALSE))
  if (!is.null(anova_table$ges)) df.out$ges <- round_ps(anova_table$ges)
  if (!is.null(anova_table$pes)) df.out$pes <- round_ps(anova_table$pes)
  df.out$p.value  <-  round_ps(anova_table[,"Pr(>F)"])
  if (!intercept) if (df.out[1,1] == "(Intercept)")  df.out <- df.out[-1,, drop = FALSE]
  rownames(df.out) <- NULL
  df.out
}

make.stat <- function(anova, stat, symbols) {
  ifelse(anova[[paste0("Pr(>", stat,")")]] < 0.001, str_c(formatC(anova[[stat]], digits = 2, format = "f"), symbols[4]), 
         ifelse(anova[[paste0("Pr(>", stat,")")]] < 0.01, str_c(formatC(anova[[stat]], digits = 2, format = "f"), symbols[3]), 
                ifelse(anova[[paste0("Pr(>", stat,")")]] < 0.05, str_c(formatC(anova[[stat]], digits = 2, format = "f"), symbols[2]), 
                       ifelse(anova[[paste0("Pr(>", stat,")")]] < 0.1, str_c(formatC(anova[[stat]], digits = 2, format = "f"), symbols[1]), formatC(anova[[stat]], digits = 2, format = "f")))))
}
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol


#' @rdname nice
#' @method nice mixed
#' @export
nice.mixed <- function(object, sig.symbols = c(" +", " *", " **", " ***"), ...) {
  anova_table <- object$anova_table
  symbols.use <-  c(" +", " *", " **", " ***")
  symbols.use[seq_along(sig.symbols)] <- sig.symbols
  if (object$method == "KR") {
    anova_table[,"df"] <- paste(ifelse(is.wholenumber(anova_table[,"num Df"]), anova_table[,"num Df"], formatC(anova_table[,"num Df"], digits = 2, format = "f")),  ifelse(is.wholenumber(anova_table[,"den Df"]),anova_table[,"den Df"], formatC(anova_table[,"den Df"], digits = 2, format = "f")), sep = ", ")

    df.out <- data.frame(Effect = row.names(anova_table), df = anova_table[,"df"], "F.scaling" = anova_table[,"F.scaling"], stringsAsFactors = FALSE, check.names = FALSE)
    df.out <- cbind(df.out, data.frame(F = make.stat(anova_table, stat = "F", symbols.use), stringsAsFactors = FALSE))
    df.out$p.value  <-  round_ps(anova_table[,"Pr(>F)"])
  } else if (object$method == "PB") {
    anova_table[,"Pr(>Chisq)"] <- anova_table[,"Pr(>PB)"]
    df.out <- data.frame(Effect = row.names(anova_table), df = anova_table[,"Chi Df"], Chisq = make.stat(anova_table, stat = "Chisq", symbols.use), p.value = round_ps(anova_table[,"Pr(>Chisq)"]), stringsAsFactors = FALSE, check.names = FALSE)
  } else if (object$method == "LRT") {
    df.out <- data.frame(Effect = row.names(anova_table), df = anova_table[,"Chi Df"], Chisq = make.stat(anova_table, stat = "Chisq", symbols.use), p.value = round_ps(anova_table[,"Pr(>Chisq)"]), stringsAsFactors = FALSE, check.names = FALSE)
  } else stop("method of mixed object not supported.")
  rownames(df.out) <- NULL
  return(df.out)
}
