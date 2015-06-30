#' Data 15.1 / 11.5 from Maxwell & Delaney
#'
#' Hypothetical IQ Data from 12 children at 4 time points: Example data for chapter 11/15 of Maxwell and Delaney (2004, Table 15.1, p. 766) in long format. Has two one within-subjects factor: time.
#' 
#' Description from pp. 534:
#' 
#' The data show that 12 subjects have been observed in each of 4 conditions. To make the example easier to discuss, let's suppose that the 12 subjects are children who have been observed at 30, 36, 42, and 48 months of age. In each case, the dependent variable is the child's age-normed general cognitive score on the McCarthy Scales of Children's Abilities. Although the test is normed so that the mean score is independent of age for the general population, our 12 children may come from a population in which cognitive abilities are either growing more rapidly or less rapidly than average. Indeed, this is the hypothesis our data allow us to address. In other words, although the sample means suggest that the children's cognitive abilities are growing, a significance test is needed if we want to rule out sampling error as a likely explanation for the observed differences.
#' 
#' To replicate the results in chapter 15 several different contrasts need to be applied, see Examples.
#'
#' \code{time} is time in months (centered at 0) and \code{timecat} is the same as a categorical variable.
#'
#' @docType data
#' @keywords dataset
#' @name md_15.1
#' @usage md_15.1
#' @format A data.frame with 48 rows and 4 variables.
#' @source Maxwell, S. E., & Delaney, H. D. (2004). Designing experiments and analyzing data: a model-comparisons perspective. Mahwah, N.J.: Lawrence Erlbaum Associates. p. 766
#' @author R code for examples written by Ulf Mertens and Henrik Singmann
#'
#' @examples
#' ### replicate results from Table 15.2 to 15.6 (Maxwell & Delaney, 2004, pp. 774)
#' data(md_15.1)
#' 
#' ### ANOVA results (Table 15.2)
#' aov_4(iq ~ timecat + (timecat|id),data=md_15.1, anova_table=list(correction = "none"))
#' 
#' ### Table 15.3 (random intercept only)
#' # we need to set the base level on the last level:
#' contrasts(md_15.1$timecat) <- contr.treatment(4, base = 4)
#' # "Type 3 Tests of Fixed Effects"
#' (t15.3 <- mixed(iq ~ timecat + (1|id),data=md_15.1, check.contrasts=FALSE))
#' # "Solution for Fixed Effects" and "Covariance Parameter Estimates"
#' summary(t15.3$full.model)
#' 
#' ### make Figure 15.2
#' plot(NULL, NULL, ylim = c(80, 140), xlim = c(30, 48), ylab = "iq", xlab = "time")
#' plyr::d_ply(md_15.1, plyr::.(id), function(x) lines(as.numeric(as.character(x$timecat)), x$iq))
#' 
#' ### Table 15.4, page 789
#' # random intercept plus slope
#' (t15.4 <- mixed(iq ~ timecat + (1+time|id),data=md_15.1, check.contrasts=FALSE))
#' summary(t15.4$full.model)
#' 
#' ### Table 15.5, page 795
#' # set up polynomial contrasts for timecat
#' contrasts(md_15.1$timecat) <- contr.poly
#' # fit all parameters separately
#' (t15.5 <- mixed(iq ~ timecat + (1+time|id), data=md_15.1, check.contrasts=FALSE,
#'                   per.parameter="timecat"))
#' # quadratic trend is considerably off, conclusions stay the same.
#' 
#' 
#' ### Table 15.6, page 797
#' # growth curve model
#' (t15.6 <- mixed(iq ~ time + (1+time|id),data=md_15.1))
#' summary(t15.6$full.model)
#' 
#' @encoding UTF-8
#' 
NULL
