#' Compare two vectors using various tests.
#'
#' Compares two vectors \code{x} and \code{y} using t-test, Welch-test (also known as Satterthwaite), Wilcoxon-test, and a permutation test implemented in \pkg{coin}.
#'
#' @usage compare.2.vectors(x, y, paired = FALSE, na.rm = FALSE, 
#'      tests = c("parametric", "nonparametric"), coin = TRUE, 
#'      alternative = "two.sided", 
#'      perm.distribution, 
#'      wilcox.exact = NULL, wilcox.correct = TRUE)
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param y a (non-empty) numeric vector of data values.
#' @param paired a logical whether the data is paired. Default is \code{FALSE}.
#' @param na.rm logical. Should \code{NA} be removed?  Default is \code{FALSE}.
#' @param tests Which tests to report, parametric or nonparamteric? The default \code{c("parametric", "nonparametric")} reports both. See details. (Arguments may be abbreviated).
#' @param alternative a character, the alternative hypothesis must be one of \code{"two.sided"} (default), \code{"greater"} or \code{"less"}. You can specify just the initial letter, will be passed to all functions.
#' @param coin logical or character. Should (permutation) tests from the \pkg{coin} package be reported? Default is \code{TRUE} corresponding to all implemented tests. \code{FALSE} calculates no tests from \pkg{coin}. A character vector may include any of the following (potentially abbreviated) implemented tests (see also Details): \code{c("permutation", "Wilcoxon", "median")}
#' @param perm.distribution \code{distribution} argument to \pkg{coin}, see \code{\link[coin]{NullDistribution}} or , \code{\link[coin]{IndependenceTest}}. If missing, defaults to \code{coin::approximate(100000)} indicating an approximation of the excat conditional distribution with 100.000 Monte Carlo samples. One can use \code{"exact"} for small samples and if \code{paired = FALSE}.
#' @param wilcox.exact \code{exact} argument to \code{\link{wilcox.test}}.
#' @param wilcox.correct \code{correct} argument to \code{\link{wilcox.test}}.
#'
#' @details The \code{parametric} tests (currently) only contain the \emph{t}-test and Welch/Statterwaithe/Smith/unequal variance \emph{t}-test implemented in \code{\link{t.test}}. The latter one is only displayed if \code{paired = FALSE}. 
#'
#' The \code{nonparametric} tests (currently) contain the Wilcoxon test implemented in \code{\link{wilcox.test}} (\code{stats::Wilcoxon}) and (if \code{coin = TRUE}) the following tests implemented in \pkg{coin}: 
#'
#' \itemize{
#' \item a \code{permutation} test \code{\link[coin]{oneway_test}} (the only test in this selction not using a rank transformation),
#' \item the \code{Wilcoxon} test \code{\link[coin]{wilcox_test}} (\code{coin::Wilcoxon}), and 
#' \item the \code{median} test \code{\link[coin]{median_test}}. 
#' }
#' Note that the two implementations of the Wilcoxon test probably differ. This is due to differences in the calculation of the Null distributions.
#'
#' @return a list with up to two elements (i.e., \code{paramteric} and/or \code{nonparamteric}) each containing  a \code{data.frame} with the following columns: \code{test}, \code{test.statistic}, \code{test.value}, \code{test.df}, \code{p}.
#'
#' @export compare.2.vectors
# @importFrom coin oneway_test wilcox_test median_test approximate statistic pvalue
#' @importFrom stats t.test wilcox.test
#' @example examples/examples.compare.R
#' 
#' @encoding UTF-8
#' 

compare.2.vectors <- function(x, y, paired = FALSE, na.rm = FALSE, 
                              tests = c("parametric", "nonparametric"), 
                              coin = TRUE, alternative = "two.sided", 
                              perm.distribution, 
                              wilcox.exact = NULL, wilcox.correct = TRUE) {
	tests <- match.arg(tests, c("parametric", "nonparametric"), several.ok = TRUE)
	if (na.rm) {
		x <- x[!is.na(x)]
		y <- y[!is.na(y)]
	} else if (any(is.na(x), is.na(y))) 
	  stop("NAs in data, use na.rm = TRUE.", call. = FALSE)
	out <- list()
	if (paired) if (!length(x) == length(y)) 
	  stop("length(x) needs to be equal to length(y) when paired is TRUE!", 
	       call. = FALSE)
	if ("parametric" %in% tests) {
		res.t <- t.test(x, y, paired = paired, var.equal = TRUE, 
		                alternative = alternative)
		parametric <- data.frame(test = "t", test.statistic = "t", 
		                         test.value = res.t[["statistic"]], 
		                         test.df = res.t[["parameter"]], 
		                         p = res.t[["p.value"]], stringsAsFactors = FALSE)
		if (!paired) {
			res.welch <- t.test(x, y, paired = paired, var.equal = FALSE, 
			                    alternative = alternative)
			parametric <- rbind(parametric, 
			                    data.frame(test = "Welch", test.statistic = "t", 
			                               test.value = res.welch[["statistic"]], 
			                               test.df = res.welch[["parameter"]], 
			                               p = res.welch[["p.value"]], 
			                               stringsAsFactors = FALSE))
		}
		rownames(parametric) <- NULL
		out <- c(out, list(parametric = parametric))
	}
	if ("nonparametric" %in% tests) {
		implemented.tests <- c("permutation", "Wilcoxon", "median")
		res.wilcox <- wilcox.test(x, y, paired = paired, exact = wilcox.exact, 
		                          correct = wilcox.correct, 
		                          alternative = alternative)
		nonparametric <- data.frame(test = "stats::Wilcoxon", 
		                            test.statistic = if (paired) "V" else "W", 
		                            test.value = res.wilcox[["statistic"]], 
		                            test.df = NA, p = res.wilcox[["p.value"]], 
		                            stringsAsFactors = FALSE)
		if (!(coin == FALSE) && !requireNamespace("coin", quietly = TRUE)) {
		  warning("package coin necessary if coin != FALSE.")
		  coin <- FALSE
		}
		if (!(coin == FALSE)) {
			dv <- c(x, y)
			iv <- factor(rep(c("A", "B"), c(length(x), length(y))))
			if (missing(perm.distribution)) {
			  perm.distribution <- coin::approximate(100000)
			}
			if (paired) {
				id <- factor(rep(1:length(x), 2))
				formula.coin <- as.formula(dv ~ iv | id)
			} else formula.coin <- as.formula(dv ~ iv)
			if (isTRUE(coin)) coin <- implemented.tests
			else coin <- match.arg(coin, implemented.tests, several.ok = TRUE)
			tryCatch(if ("permutation" %in% coin) {
			  res.perm <- coin::oneway_test(formula.coin, 
			                                distribution=perm.distribution, 
			                                alternative = alternative)
				nonparametric <- rbind(nonparametric, 
				                       data.frame(test = "permutation", 
				                                  test.statistic = "Z", 
				                                  test.value = 
				                                    coin::statistic(res.perm), 
				                                  test.df = NA, 
				                                  p = coin::pvalue(res.perm)[1],
				                                  stringsAsFactors = FALSE))
			}, error = function(e) 
			  warning(paste("coin::permutation test failed:", e)))
			tryCatch(if ("Wilcoxon" %in% coin) {
			  res.coin.wilcox <- coin::wilcox_test(formula.coin, 
			                                       distribution=perm.distribution, 
			                                       alternative = alternative)
				nonparametric <- rbind(nonparametric, 
				                       data.frame(test = "coin::Wilcoxon", 
				                                  test.statistic = "Z", 
				                                  test.value = 
				                                    coin::statistic(res.coin.wilcox), 
				                                  test.df = NA, 
				                                  p = coin::pvalue(res.coin.wilcox)[1], 
				                                  stringsAsFactors = FALSE))
			}, error = function(e) warning(paste("coin::Wilcoxon test failed:", e)))
			tryCatch(if ("median" %in% coin) {
				res.median <- coin::median_test(formula.coin, 
				                                distribution=perm.distribution, 
				                                alternative = alternative)
				nonparametric <- rbind(nonparametric, 
				                       data.frame(test = "median", 
				                                  test.statistic = "Z", 
				                                  test.value = 
				                                    coin::statistic(res.median), 
				                                  test.df = NA, 
				                                  p = coin::pvalue(res.median)[1], 
				                                  stringsAsFactors = FALSE))
			}, error = function(e) warning(paste("coin::median test failed:", e)))
		}
		rownames(nonparametric) <- NULL
		out <- c(out, nonparametric = list(nonparametric))
	}
	out
}
