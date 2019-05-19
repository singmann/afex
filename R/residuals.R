#' Extract and Plot \code{afex_aov} Model Residuals
#' 
#' @author Mattan S. Ben-Shachar
#' 
#' @example examples/examples.residuals.R
#' 
#' @param object \code{afex_aov} object.
#' @param return_df if \code{TRUE} returns the long data with an additional column \code{residuals}.
#' @param ... Not used.
#' 
#' @return A vector of residualts, or if \code{return_df = TRUE} a data frame with an additional column \code{residuals}.
#' 
#' @export
residuals.afex_aov <- function(object, return_df = FALSE, ...){
  data <- object$data$long
  
  if (length(attr(object, "within")) == 0) {
    afex_residuals <- residuals(object$lm)
  } else {
    dv <- attr(object,'dv')
    id <- attr(object,'id')
    
    formula <- as.formula(object$Anova)[-1]
    formula <- paste0(c(formula,id), collapse = "+")
    formula <- as.formula(paste0(dv,"~",formula))
    
    afex_residuals <- residuals(lm(formula,data))
  }
  
  if (return_df) {
    data$residuals <- afex_residuals
    return(data)
  } else {
    return(afex_residuals)
  }
}