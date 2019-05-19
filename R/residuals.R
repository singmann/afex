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
#' \code{residuals_qqplot} returns a \emph{ggplot2} plot (i.e., object of class \code{c("gg", "ggplot")}) 
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

#' @rdname residuals.afex_aov
#' @export
residuals_qqplot <- function(object){
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("package ggplot2 is required.", call. = FALSE)
  }
  all_residuals <- residuals(object,return_df = TRUE)
  
  id <- attr(object,'id')
  
  
  if (length(attr(object, "within")) == 0) {
    plot_data <- data.frame(residuals = all_residuals$residuals,
                            proj = id)
  } else {
    within <- names(attr(object,'within'))
    between <- names(attr(object,'between'))
    
    # within
    combs <- expand.grid(lapply(within, function(x) c(x,NA)))
    combs$id <- id
    combs <- head(combs,-1)
    combs <- lapply(as.data.frame(t(combs)),na.omit)
    
    model_residuals <- list()
    for (i in seq_along(combs)) {
      temp_factors <- as.character(combs[[i]])
      temp_data <- aggregate(all_residuals$residuals,all_residuals[,temp_factors],mean)
      
      temp_name <- paste0(temp_factors,collapse = '*')
      model_residuals[[temp_name]] <- temp_data$x
    }
    
    # between
    if (!is.null(between)) {
      temp_data <- aggregate(all_residuals$residuals,all_residuals[,c(between,id)],mean)
      model_residuals[[id]] <- temp_data$x
    }
    
    plot_data <- stack(model_residuals)
    colnames(plot_data) <- c("residuals","proj")
  }
  
  # plot
  ggplot2::ggplot(plot_data,ggplot2::aes(sample = .data$residuals)) +
    ggplot2::geom_qq() +
    ggplot2::geom_qq_line() +
    ggplot2::facet_wrap(~.data$proj, scales = "free")
}
