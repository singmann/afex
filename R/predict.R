#' Predict method for \code{afex_aov} objects
#'
#' Predicted values based on \code{afex_aov} objects.
#'
#' @author Mattan S. Ben-Shachar
#'
#' @example examples/examples.predict.R
#'
#' @param newdata An optional data frame in which to look for variables with
#'   which to predict. If omitted, the fitted values are used.
#' @param colname_predict Name of the appended column when \code{append = TRUE}.
#' @inheritParams residuals.afex_aov
#' @param ... Not used.
#'
#' @return A vector of predicted values corresponding to the data in
#'   \code{object$data$long} or to \code{newdata}, or if \code{append = TRUE} a
#'   data frame with an additional column of predicted values.
#'
#' @export
#' @importFrom stats predict fitted setNames
predict.afex_aov <- function(
  object,
  newdata,
  append = FALSE,
  colname_predict = ".predict",
  ...
) {
  # if no newdata, get fitted
  if (missing(newdata)) {
    return(fitted(object, append = append, colname_predict = colname_predict))
  }

  # soft factorize numeric data
  for (col in colnames(newdata)) {
    if (is.numeric(newdata[[col]])) {
      i <- which(colnames(object$data$long) == col)
      if (length(i) && !is.numeric(object$data$long[[i]])) {
        newdata[[col]] <- as.character(newdata[[col]])
      }
    }
  }

  # get predicted values
  pred <- predict(object$lm, newdata = newdata)
  colnames(pred) <- colnames(fitted(object$lm))

  # if any within, needs some clean up
  if (length(attr(object, "within")) > 0) {
    within <- attr(object, "within")

    # make long ~
    pred <- sapply(seq_len(nrow(pred)), function(i) {
      temp_pred <- pred
      w_rm <- sapply(newdata[i, names(within)], make.names, unique = TRUE)
      w_rm <- paste0(w_rm, collapse = "_")
      temp_pred[i, w_rm]
    })

    pred <- setNames(pred, rownames(newdata))
  }

  if (append) {
    newdata[[colname_predict]] <- pred
    return(newdata)
  } else {
    return(pred)
  }
}
