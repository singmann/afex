#' Extract \code{afex_aov} Model Residuals
#' 
#' Extract residuals from \code{afex_aov} objects.
#' 
#' @author Mattan S. Ben-Shachar
#' 
#' @example examples/examples.residuals.R
#' 
#' @param object \code{afex_aov} object.
#' @param append If set to \code{TRUE} returns the residuals appended as an additional column \code{.residuals.} to the long data. 
#'   Recomended when data was aggragated across within conditions.
#' @param ... Not used.
#' 
#' @return A vector of residualts corresponding to the data in \code{object$data$long}, 
#' or if \code{append = TRUE} a data frame with an additional column \code{.residuals.}.
#' 
#' @export
residuals.afex_aov <- function(object, append = FALSE, ...) {
  if (!append && attr(object, "data_changed")) {
    warning("Data was changed during ANOVA calculation. ", 
            "Thus, residuals cannot be added to original data.", 
            "\nresiduals(..., append = TRUE) will return data and residuals.", 
            call. = FALSE)
  }
  
  if (length(attr(object, "within")) > 0) {
    # residuals in long format
    e <- data.frame(residuals(object$lm))
    varying <- colnames(e)
    e[attr(object, "id")] <- object$data$wide[attr(object, "id")]
    e <- reshape(
      e,
      direction = "long",
      varying = varying,
      v.name = ".residuals.",
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
    
    e <- merge(e, index, by = ".time.")
    e$.time. <- NULL
    e$.index. <- NULL
    
    # add between data
    between_data <- object$data$long
    e <- merge(between_data, e, by = c(attr(object, "id"), names(attr(object, "within"))))
  } else {
    e <- object$data$long
    e$.residuals. <- residuals(object$lm)
  }
  
  if (append) {
    return(e)
  } else {
    e$.residuals.
  }
}
