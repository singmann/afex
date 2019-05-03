#' Levene's Test for Homogeneity
#'
#' Computes Levene's test for homogeneity of variance across groups via \code{car::leveneTest}.
#'
#' @param afex_aov \code{afex_aov} object.
#' @param center Function to compute the center of each group; \code{mean} (the default) gives the original Levene's test.
#' @param ... passed to \code{car::leveneTest}
#'
#' @author Mattan S. Ben-Shachar
#' 
#' @examples 
#' \dontrun{
#' fit_value <- aov_car(value ~ treatment*gender + Error(id), data = obk.long)
#' 
#' test_levene(fit_value)
#' #> Levene's Test for Homogeneity of Variance (center = center)
#' #>       Df F value Pr(>F)
#' #> group  5  1.2671 0.3497
#' #>       10 
#' }
#'
#' @export
test_levene <- function(afex_aov, center = mean, ...){
  if (length(attr(afex_aov,'between'))==0) {
    stop("Levene test is only aplicable to models with between subject factors.")
  }
  data <- afex_aov$data$long
  dv <- attr(afex_aov,'dv')
  id <- attr(afex_aov,'id')
  between <- names(attr(afex_aov,'between'))

  form <- formula(paste0(dv,'~',paste0(between,collapse = '*')))

  ag_data <- aggregate(data[,dv],data[,c(between,id)],mean)
  colnames(ag_data)[length(c(between,id))+1] <- dv

  car::leveneTest(form,ag_data, center = center, ...)
}

#' Mauchly Test of Sphericity
#'
#' Computes Mauchly test of sphericity via \code{car::Anova}.
#'
#' @param afex_aov \code{afex_aov} object.
#'
#' @author Mattan S. Ben-Shachar
#' 
#' @examples 
#' \dontrun{
#' fit_value <- aov_car(value ~ treatment * gender + Error(id/(phase*hour)), data = obk.long)
#' 
#' test_sphericity(fit_value)
#' #>                             Test statistic p-value
#' #> phase                              0.74927 0.27282
#' #> treatment:phase                    0.74927 0.27282
#' #> gender:phase                       0.74927 0.27282
#' #> treatment:gender:phase             0.74927 0.27282
#' #> hour                               0.06607 0.00760
#' #> treatment:hour                     0.06607 0.00760
#' #> gender:hour                        0.06607 0.00760
#' #> treatment:gender:hour              0.06607 0.00760
#' #> phase:hour                         0.00478 0.44939
#' #> treatment:phase:hour               0.00478 0.44939
#' #> gender:phase:hour                  0.00478 0.44939
#' #> treatment:gender:phase:hour        0.00478 0.44939
#' 
#' }
#'
#' @export
test_sphericity<- function(afex_aov){
  if (length(attr(between_1,'within'))==0) {
    stop("Mauchly Test of Sphericity is only aplicable to models with within subject factors.")
  }
  summary(afex_aov)$sphericity.tests
}