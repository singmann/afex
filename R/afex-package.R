#' Analysis of Factorial Experiments.
#'
#' \tabular{ll}{
#' Package: \tab afex\cr
#' Type: \tab Package\cr
#' Version: \tab 0.21-2\cr
#' Date: \tab 2018-06-24\cr
#' Depends: \tab R (>= 3.1.0), lme4 (>= 1.1-8), emmeans (>= 1.1.2)\cr
#' Encoding: \tab UTF-8\cr
#' License: \tab GPL (>=2)\cr
#' URL: \tab http://afex.singmann.science/, https://github.com/singmann/afex\cr
#' }
#'
#' Convenience functions for analyzing factorial experiments using ANOVA or mixed models.
#' aov_ez(), aov_car(), and aov_4() allow specification of between, within (i.e.,
#' repeated-measures), or mixed between-within (i.e., split-plot) ANOVAs for data in long format (i.e.,
#' one observation per row), aggregating multiple observations per individual and cell of the
#' design. mixed() fits mixed models using lme4::lmer() and computes p-values for all fixed
#' effects using either Kenward-Roger or Satterthwaite approximation for degrees of freedom
#' (LMM only), parametric bootstrap (LMMs and GLMMs), or likelihood ratio tests (LMMs and
#' GLMMs). afex uses type 3 sums of squares as default (imitating commercial statistical software).
#'
#' @aliases afex-package
#' @name afex-package
#' @docType package
#' @title The afex Package
#' @author Henrik Singmann, Ben Bolker, Jake Westfall, Frederik Aust, with contributions from Søren Højsgaard, John Fox, Michael A. Lawrence, Ulf Mertens, Jonathon Love, Russell Lenth, Rune Haubo Bojesen Christensen
#' @keywords package
NULL
