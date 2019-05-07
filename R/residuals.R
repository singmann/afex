#' Extract and Plot \code{afex_aov} Model Residuals
#' 
#' @author Mattan S. Ben-Shachar
#' 
#' @example examples/examples.residuals.R
#' 
#' @param object \code{afex_aov} object.
#' @param model If returned residuals should be multivariate (default) or univariate. Can be abbreviated.
#' @param ... Not used.
#' 
#' @return
#' \code{residuals} returns residuals in one of the following formats:
#' \itemize{
#'   \item For between-subject only ANOVAs, a vector of residuals.
#'   \item For within-subject/mixed design ANOVAs:
#'   \itemize {
#'     \item \code{model = "multivariate"}: a multivariate residual matrix.
#'     \item \code{model = "univariate"}: a list of residuals, with a vector for each of the ANOVA model's error-terms.
#'   }
#' }
#' 
#' \code{residuals_qqplot} returns a \emph{ggplot2} plot (i.e., object of class \code{c("gg", "ggplot")}) 
#' 
#' @export
residuals.afex_aov <- function(object, model = c("multivariate","univariate"), ...){
  model <- match.arg(model,c("multivariate","univariate"))
  if (length(attr(object, "within")) == 0 || model == "multivariate") {
    return(residuals(object$lm))
    # or ?
    # return(as.vector(residuals(object$lm)))
  } else {
    data <- object$data$long
    dv <- attr(object,'dv')
    id <- attr(object,'id')
    between <- names(attr(object,'between'))
    within <- names(attr(object,'within'))
    
    # within
    combs <- expand.grid(lapply(within, function(x) c(x,NA)))
    combs$id <- id
    combs <- head(combs,-1)
    combs <- lapply(as.data.frame(t(combs)),na.omit)
    
    model_residuals <- list()
    for (i in seq_along(combs)) {
      temp_factors <- as.character(combs[[i]])
      temp_data <- aggregate(data[,dv],data[,temp_factors],mean)
      
      temp_name <- paste0(head(temp_factors,-1),collapse = '*')
      temp_form <- formula(paste0("x~",temp_name,"+",id))
      model_residuals[[temp_name]] <- residuals(lm(temp_form,temp_data))
    }
    
    # between
    if (!is.null(between)) {
      temp_data <- aggregate(data[,dv],data[,c(between,id)],mean)
      temp_form <- formula(paste0("x~",paste0(c(between),collapse = '*')))
      model_residuals[[id]] <- residuals(lm(temp_form,temp_data))
    }
    return(model_residuals)
  }
}

#' @rdname residuals.afex_aov
#' @export
residuals_qqplot <- function(object){
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("package ggplot2 is required.", call. = FALSE)
  }
  all_residuals <- residuals(object,model = "univariate")
  
  if (is.list(all_residuals)) {
    all_residuals <- lapply(names(all_residuals), function(x) data.frame(residuals = all_residuals[[x]],proj = x))
    
    plot_data <- do.call("rbind",all_residuals)
  } else {
    plot_data <- data.frame(residuals = all_residuals,
                            proj      = attr(object,"id"))
  }
  
  ggplot2::ggplot(plot_data,ggplot2::aes(sample = residuals)) +
    ggplot2::geom_qq() +
    ggplot2::geom_qq_line() +
    ggplot2::facet_wrap(~proj, scales = "free")
}
