#' Assumption Tests for ANOVAs
#' 
#' \code{test_levene} computes Levene's test for homogeneity of variances across
#' groups via \code{car::leveneTest}. \code{test_sphericity} computes Mauchly
#' test of sphericity via \code{car::Anova}.
#'
#' @param afex_aov \code{afex_aov} object.
#' @param center Function to compute the center of each group; \code{mean} (the
#'   default) gives the original Levene's test.
#' @param ... passed to \code{\link[car]{leveneTest}}
#'
#' @author Mattan S. Ben-Shachar and Henrik Singmann
#' 
#' 
#' @example examples/examples.test_assumptions.R
#' 
#' @rdname test_assumptions 
#' @export
test_levene <- function(afex_aov, center = mean, ...){
  if (length(attr(afex_aov,'between')) == 0) {
    stop("Levene test is only aplicable to ANOVAs with between-subjects factors.")
  }
  data <- afex_aov$data$long
  dv <- attr(afex_aov,'dv')
  id <- attr(afex_aov,'id')
  between <- names(attr(afex_aov,'between'))

  form <- stats::formula(paste0(dv,'~',paste0(between,collapse = '*')))

  ag_data <- aggregate(data[,dv],data[,c(between,id)],mean)
  colnames(ag_data)[length(c(between,id))+1] <- dv

  car::leveneTest(form,ag_data, center = center, ...)
}

#' @export
#' @rdname test_assumptions 
test_sphericity<- function(afex_aov){
  if (length(attr(afex_aov,'within')) == 0) {
    stop("Mauchly Test of Sphericity is only aplicable to ANOVAs with within-subjects factors.")
  }
  summary(afex_aov)$sphericity.tests
}
