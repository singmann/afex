#' Plot Normal Quantile-Quantile Plots for \code{afex_aov} models
#' 
#' Plot Normal qq-plots for the residuals / error terms used in an
#' \code{afex_aov} model. For models with within-subjects effects, qq-plots can
#' be shown for each error term in the ANOVA table, or for each within-subject
#' cell (in the multivariate model).
#' 
#' @param y \code{afex_aov} object.
#' @param type (ignored if model has only between subject effects.) Type of 
#'   error to plot (can be abbreviated): \describe{
#'     \item{\code{"marginal"}} {Plot the margianl residuals.}  
#'     \item{\code{"univariate"}}{Plot the residuals for each error term used in the ANOVA table.}  
#'     \item{\code{"multivariate"}}{Plot the residuals for each cell in the within-subjects effects.}  
#'   }
#' @param qqbands Should 95\% confidence bands be plotted? (requires
#'   \code{qqplotr}).
#' @param detrend Should the plot objects be detrended? This may help reducing
#'   visual bias. (requires \code{qqplotr}).
#' @param return Should a \code{ggplot} be returned, or a data frame of the
#'   requested errors.
#' @param ... Not used.
#' 
#' @author Mattan S. Ben-Shachar
#' 
#' @example examples/examples.qqnorm.afex_aov.R
#' 
#' @export
#' @importFrom stats na.omit
qqnorm.afex_aov <- function(y,
                            type = c("marginal", "univariate", "multivariate"),
                            qqbands = TRUE, 
                            detrend = FALSE, 
                            return = c("plot", "data"),
                            ...){
  .data <- NULL
  
  type <- match.arg(type)
  return <- match.arg(return)
  
  within <- names(attr(y, "within"))
  id <- attr(y, 'id')
  
  e <- residuals(y, append = TRUE)
  
  
  if (length(within) && type == "univariate") {
    wf <- lapply(within, function(x) c(NA, x))
    wf <- do.call(expand.grid, wf)
    wl <- apply(wf, 1, function(x) c(na.omit(x)))
    wl <- wl[-1]
    
    fin_e <- vector("list", length = length(wl))
    
    for (i in seq_along(wl)) {
      temp_f <- c(id, wl[[i]])
      
      temp_e <- aggregate(e$.residuals, by = e[, temp_f, drop = F], FUN = mean)
      
      fin_e[[i]] <- data.frame(
        term = paste0(temp_f, collapse = ":"),
        .residuals = temp_e[[ncol(temp_e)]]
      )
    }
    e <- do.call(rbind, fin_e)
  } else if (length(within) && type == "multivariate") {
    e$term <- apply(e[, within], 1, function(x) paste0(x, collapse = "/"))
  } else if (length(within) && type == "marginal") {
    e$term <- "Residuals"
  } else {
    e$term <- id
  }
  
  if (return == "data") {
    return(e[,c("term",".residuals")])
  }
  
  
  ## Plot
  qq_stuff <- list(ggplot2::stat_qq(),
                   ggplot2::stat_qq_line())
  
  if (qqbands || detrend) {
    if (requireNamespace("qqplotr")) {
      qq_stuff <- list(if (qqbands) qqplotr::stat_qq_band(detrend = detrend) else NULL,
                       qqplotr::stat_qq_line(detrend = detrend),
                       qqplotr::stat_qq_point(detrend = detrend))  
    } else {
      message("'qqbands' and 'detrend' require 'qqplotr'.")
    }
  }
  
  ggplot2::ggplot(e, ggplot2::aes(sample = .data$.residuals)) +
    qq_stuff +
    ggplot2::facet_wrap(~ .data$term, scales = "free") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    ) +
    ggplot2::labs(x = "Theoretical",
                  y = "Sample")
}
