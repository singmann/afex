#' Extract Residuals and Fitted Values from \code{afex_aov} objects
#' 
#' Extract Residuals and Fitted Values from \code{afex_aov} objects.
#' 
#' @author Mattan S. Ben-Shachar
#' 
#' @example examples/examples.residuals.R
#' 
#' @param object \code{afex_aov} object.
#' @param append If set to \code{TRUE} returns the residuals/fitted values appended as an additional column to the long data. 
#'   Recomended when data was aggragated across within conditions.
#' @param colname_residuals,colname_fitted Name of the appended column when \code{append = TRUE}.
#' @param ... Additional arguments passed to \code{residuals.lm}/\code{fitted.lm}.
#' 
#' @return A vector of residualts/fitted values corresponding to the data in \code{object$data$long}, 
#' or if \code{append = TRUE} a data frame with an additional column of residualts/fitted values.
#' 
#' @export
#' @importFrom stats fitted reshape residuals
residuals.afex_aov <- function(object, append = FALSE, colname_residuals = ".residuals",...) {
  if (!append && attr(object, "data_changed")) {
    message("Data was changed during ANOVA calculation. ", 
            "Thus, residuals cannot be added to original data.", 
            "\nresiduals(..., append = TRUE) will return data and residuals.")
  }
  
  e <- residuals(object$lm, ...)
  .clean_model_values(object, model_values = e, values_colname = colname_residuals, append = append)
}

#' @export
#' @rdname residuals.afex_aov
fitted.afex_aov <- function(object, append = FALSE, colname_fitted = ".fitted", ...) {
  if (!append && attr(object, "data_changed")) {
    message("Data was changed during ANOVA calculation. ", 
            "Thus, fitted values cannot be added to original data.", 
            "\nfitted(..., append = TRUE) will return data and fitted values.")
  }
  
  e <- fitted(object$lm, ...)
  .clean_model_values(object, model_values = e, values_colname = colname_fitted, append = append)
}


#' @keywords internal
.clean_model_values <- function(object, model_values, values_colname, append) {
  if (length(attr(object, "within")) > 0) {
    # In long format
    mv <- data.frame(model_values)
    varying <- colnames(mv)
    mv[attr(object, "id")] <- object$data$wide[attr(object, "id")]
    mv <- reshape(
      mv,
      direction = "long",
      varying = varying,
      v.names = values_colname,
      times = varying,
      timevar = ".time.",
      idvar = attr(object, "id")
    )
    
    
    # add within data
    code <- data.frame(.time. = varying,
                       .index. = seq_along(varying))
    index <- object$data$idata
    index$.index. <- seq_len(nrow(index))
    index <- merge(code, index, by = ".index.")
    
    mv <- merge(mv, index, by = ".time.")
    mv$.time. <- NULL
    mv$.index. <- NULL
    
    # add between data
    between_data <- object$data$long
    mv <- merge(between_data, mv, by = c(attr(object, "id"), names(attr(object, "within"))))
  } else {
    mv <- object$data$long
    mv[[values_colname]] <- model_values
  }
  
  if (append) {
    return(mv)
  } else {
    return(setNames(mv[[values_colname]],rownames(mv)))
  }
}