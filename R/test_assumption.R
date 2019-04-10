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
#' @export
test_levene <- function(afex_aov, center = mean, ...){
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
#' @export
test_sphericity<- function(afex_aov){
  summary(afex_aov)$sphericity.tests
}

#' Plot Residuals' QQ-plot
#'
#' Plot qq-plot and qq-line for the residuals of each error term. Used to test normality of residuals.
#'
#' @param afex_aov \code{afex_aov} object.
#' @return if \code{return='plot'} will return a ggplot2 of the qqplot. Anything else will return a list of residulas.
#'
#' @author Mattan S. Ben-Shachar
#'
#' @export
residuals_qqplot <- function(afex_aov, return = 'plot') {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("package ggplot2 is required.", call. = FALSE)
  }
  # get aovlist
  data <- afex_aov$data$long
  dv <- attr(afex_aov,'dv')
  id <- attr(afex_aov,'id')
  between <- names(attr(afex_aov,'between'))
  within <- names(attr(afex_aov,'within'))

  lhs <- paste0(c(within,between),collapse = '*')
  err <- ''
  if (!is.null(within)) {
    err <- paste0('+Error(',id,'/(',paste0(within,collapse = '*'),'))')
  }

  form <- formula(paste0(dv,'~',lhs,err))

  aov_fit <- aov(form,data)

  # get residuals
  aov_proj <- proj(aov_fit)

  if (!is.list(aov_proj)) aov_proj <- list(aov_proj)

  res_vals <- lapply(aov_proj,function(X) as.data.frame(X)[['Residuals']])

  res_names <- lapply(aov_proj, function(X) paste0(attr(X,"factors")$Residuals,collapse = ':'))

  remove_ <- sapply(res_vals,is.null)
  res_vals <- res_vals[!remove_]
  res_names <- res_names[!remove_]
  names(res_vals) <- res_names

  if (return=='plot') {
    # plot
    plot_data <- data.frame(x    = unlist(res_vals),
                            proj = unlist(lapply(res_names, function(X) rep(X,nrow(data)))),
                            row.names = NULL)

    ggplot(plot_data,aes(sample = x)) +
      geom_qq() +
      geom_qq_line() +
      facet_wrap(~proj)
  } else {
    return(res_vals)
  }
}
