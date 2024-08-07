#' Methods for afex_aov objects
#'
#' Methods defined for objects returned from the ANOVA functions
#' \code{\link{aov_car}} et al. of class \code{afex_aov} containing both the
#' ANOVA fitted via \code{car::Anova} and base R's \code{aov}.
#' 
#' @param object,x object of class \code{afex_aov} as returned from
#'   \code{\link{aov_car}} and related functions.
#' @param p_adjust_method \code{character} indicating if p-values for individual
#'   effects should be adjusted for multiple comparisons (see
#'   \link[stats]{p.adjust} and details).
#' @param ... further arguments passed through, see description of return value
#'   for details.
#' @param trms,xlev,grid same as for \code{\link[emmeans]{emm_basis}}.
#' @param model argument for \code{\link[emmeans]{emmeans}()} and related
#'   functions that allows to choose on which model the follow-up tests for
#'   ANOVAs with repeated-measures factors are based. \code{"multivariate"} (the
#'   default) uses the \code{lm} model and \code{"univariate"} uses the
#'   \code{aov} model. Default given by \code{afex_options("emmeans_mode")}.
#'   Multivariate tests likely work better for unbalanced data and provide a
#'   better correction for violations of sphericity.
#' 
#' @return
#' \describe{
#'   \item{\code{anova}}{Returns an ANOVA table of class \code{c("anova",
#'   "data.frame")}. Information such as effect size (\code{es}) or
#'   df-correction are calculated each time this method is called.}
#'   \item{\code{summary}}{For ANOVAs containing within-subject factors it
#'   returns the full output of the within-subject tests: the uncorrected
#'   results, results containing Greenhousse-Geisser and Hyunh-Feldt correction,
#'   and the results of the Mauchly test of sphericity (all achieved via
#'   \code{summary.Anova.mlm}). For other ANOVAs, the \code{anova} table is
#'   simply returned.}
#'   \item{\code{print}}{Prints (and invisibly returns) the ANOVA table as
#'   constructed from \code{\link{nice}} (i.e., as strings rounded nicely).
#'   Arguments in \code{...} are passed to \code{nice} allowing to pass
#'   arguments such as \code{es} and \code{correction}.}
#'   \item{\code{recover_data} and \code{emm_basis}}{Provide the backbone for
#'   using \code{\link[emmeans]{emmeans}} and related functions from
#'   \pkg{emmeans} directly on \code{afex_aov} objects by returning a
#'   \code{\link[emmeans]{emmGrid-class}} object. Should not be called directly
#'   but through the functionality provided by \pkg{emmeans}.}
#' }
#'
#' @details 
#' Exploratory ANOVA, for which no detailed hypotheses have been specified a
#' priori, harbor a multiple comparison problem (Cramer et al., 2015). To avoid
#' an inflation of familywise Type I error rate, results need to be corrected
#' for multiple comparisons using \code{p_adjust_method}. \code{p_adjust_method}
#' defaults to the method specified in the call to \code{\link{aov_car}} in
#' \code{anova_table}. If no method was specified and \code{p_adjust_method =
#' NULL} p-values are not adjusted.
#' 
#' @seealso \code{residuals} and \code{fitted} methods also exists for
#'   \code{afex_aov} objects, see: \code{\link{residuals.afex_aov}}.
#' 
#' @references 
#' Cramer, A. O. J., van Ravenzwaaij, D., Matzke, D., Steingroever, H., Wetzels,
#' R., Grasman, R. P. P. P., ... Wagenmakers, E.-J. (2015). Hidden multiplicity
#' in exploratory multiway ANOVA: Prevalence and remedies.  \emph{Psychonomic
#' Bulletin & Review}, 1-8. \doi{10.3758/s13423-015-0913-5}
#' 
#' @name afex_aov-methods
#' @importFrom stats p.adjust
NULL

#### methods for afex_aov

#' @rdname afex_aov-methods
#' @inheritParams nice
#' @method anova afex_aov
#' @export
anova.afex_aov <- function(object, 
                           es = afex_options("es_aov"), 
                           observed = NULL, 
                           correction = afex_options("correction_aov"), 
                           MSE = TRUE, intercept = FALSE, 
                           p_adjust_method = NULL, 
                           sig_symbols = attr(object$anova_table, "sig_symbols"),
                           ...) {
  # internal functions:
  # check arguments
  dots <- list(...)
  if("p.adjust.method" %in% names(dots)) {  #(!missing(sig.symbols)) {
    warn_deprecated_arg("p.adjust.method", "p_adjust_method")
    p_adjust_method <- dots$p.adjust.method
  }
  es <- match.arg(es, c("none", "ges", "pes"), several.ok = TRUE)
  correction <- match.arg(correction, c("GG", "HF", "none"))
  if (inherits(object$Anova, "Anova.mlm")) {
    tmp <- tryCatch(
      expr = suppressWarnings(summary(object$Anova, multivariate = FALSE)), 
      error = function(e) 
        stop("summary.Anova.mlm() failed for 'afex_aov' object.", 
             "\nPossible reason, car package updated.", 
             "\nTry refitting model with afex ANOVA function.", call. = FALSE)
    )              
    t.out <- tmp[["univariate.tests"]]
    #browser()
    #t.out <- cbind(t.out, orig_den_df =  t.out[, "den Df"])
    if (correction[1] == "GG") {
      tmp[["pval.adjustments"]] <- tmp[["pval.adjustments"]][
        !is.na(tmp[["pval.adjustments"]][,"GG eps"]),, drop = FALSE]
      t.out[row.names(tmp[["pval.adjustments"]]), "num Df"] <- 
        t.out[row.names(tmp[["pval.adjustments"]]), "num Df"] * 
        tmp[["pval.adjustments"]][,"GG eps"]
      t.out[row.names(tmp[["pval.adjustments"]]), "den Df"] <- 
        t.out[row.names(tmp[["pval.adjustments"]]), "den Df"] * 
        tmp[["pval.adjustments"]][,"GG eps"]
      t.out[row.names(tmp[["pval.adjustments"]]), "Pr(>F)"] <- 
        tmp[["pval.adjustments"]][,"Pr(>F[GG])"]
    } else {
      if (correction[1] == "HF") {
        try(if (any(tmp[["pval.adjustments"]][,"HF eps"] > 1))
          warning("HF eps > 1 treated as 1", call. = FALSE), silent = TRUE)
        tmp[["pval.adjustments"]] <- tmp[["pval.adjustments"]][
          !is.na(tmp[["pval.adjustments"]][,"HF eps"]),, drop = FALSE]
        t.out[row.names(tmp[["pval.adjustments"]]), "num Df"] <- 
          t.out[row.names(tmp[["pval.adjustments"]]), "num Df"] * 
          pmin(1, tmp[["pval.adjustments"]][,"HF eps"])
        t.out[row.names(tmp[["pval.adjustments"]]), "den Df"] <- 
          t.out[row.names(tmp[["pval.adjustments"]]), "den Df"] * 
          pmin(1, tmp[["pval.adjustments"]][,"HF eps"])
        t.out[row.names(tmp[["pval.adjustments"]]), "Pr(>F)"] <- 
          tmp[["pval.adjustments"]][,"Pr(>F[HF])"]
      } else {
        if (correction[1] == "none") {
          TRUE
        } else stop("None supported argument to correction.")
      }
    }
    tmp.df <- t.out    
    tmp2 <- as.data.frame(unclass(tmp.df))
  #} else if (class(object$Anova)[1] == "anova") {
  } else if (inherits(object$Anova, "anova")) {
    #browser()
    tmp.df <- cbind(object$Anova[-nrow(object$Anova),], 
                    data.frame("Error SS" = object$Anova[nrow(object$Anova), 
                                                         "Sum Sq"], 
                               "den Df" = object$Anova[nrow(object$Anova), 
                                                       "Df"], 
                               check.names = FALSE))
    colnames(tmp.df)[1:3] <- c("SS", "num Df", "F")
    #tmp.df$orig_den_df <- tmp.df[, "den Df"]
    tmp2 <- as.data.frame(tmp.df)
  } else stop("Non-supported object passed. Slot 'Anova' needs to be of class 'Anova.mlm' or 'anova'.")
  tmp2[,"MSE"] <- tmp2[,"Error SS"]/tmp2[,"den Df"]
  ## provision for car 3.0 (March 2018), for calculation of es
  if ("Sum Sq" %in% colnames(tmp2)) {
    tmp2$SS <- tmp2[,"Sum Sq"]
  }
  # calculate es
  es_df <- data.frame(row.names = rownames(tmp2))
  if ("pes" %in% es) {
    es_df$pes <- tmp2$SS/(tmp2$SS + tmp2[,"Error SS"])
  }
  if ("ges" %in% es) {
    # This code is basically a copy from ezANOVA by Mike Lawrence!
    if(!is.null(observed) & length(observed) > 0){
      obs <- rep(FALSE,nrow(tmp2))
      for(i in observed){
        if (!any(grepl(paste0("\\b",i,"\\b"), rownames(tmp2)))) 
          stop(paste0("Observed variable not in data: ", i))
        obs <- obs | grepl(paste0("\\b",i,"\\b"), rownames(tmp2))
      }
      obs_SSn1 <- sum(tmp2$SS*obs)
      obs_SSn2 <- tmp2$SS*obs
    } else {
      obs_SSn1 <- 0
      obs_SSn2 <- 0
    }
    es_df$ges <- tmp2$SS/(tmp2$SS+sum(unique(tmp2[,"Error SS"])) + 
                            obs_SSn1-obs_SSn2)
  }
  colname_f <- grep("^F", colnames(tmp2), value = TRUE)
  anova_table <- cbind(tmp2[,c("num Df", "den Df", "MSE")], 
                       F = tmp2[,colname_f], 
                       es_df, 
                       "Pr(>F)" = tmp2[,c("Pr(>F)")])
  #browser()
  if (!MSE) anova_table$MSE <- NULL 
  if (!intercept) if (row.names(anova_table)[1] == "(Intercept)")  
    anova_table <- anova_table[-1,, drop = FALSE]
  # Correct for multiple comparisons
  if(is.null(p_adjust_method)) p_adjust_method <- 
    ifelse(is.null(attr(object$anova_table, "p_adjust_method")), "none", 
           attr(object$anova_table, "p_adjust_method"))
  anova_table[,"Pr(>F)"] <- p.adjust(anova_table[,"Pr(>F)"], 
                                     method = p_adjust_method)
  class(anova_table) <- c("anova", "data.frame")
  p_adj_heading <- if(p_adjust_method != "none") paste0(", ", p_adjust_method, 
                                                        "-adjusted") else NULL
  attr(anova_table, "heading") <- c(paste0("Anova Table (Type ", 
                                           attr(object, "type"), " tests", 
                                           p_adj_heading, ")\n"), 
                                    paste("Response:", attr(object, "dv")))
  attr(anova_table, "p_adjust_method") <- p_adjust_method
  attr(anova_table, "es") <- es
  attr(anova_table, "correction") <- 
    if(length(attr(object, "within")) > 0 && any(
      vapply(object$data$long[, names(attr(object, "within")), 
                              drop = FALSE], nlevels, 0) > 2)) 
      correction else "none"
  attr(anova_table, "observed") <- 
    if(!is.null(observed) & length(observed) > 0) observed else character(0)
  attr(anova_table, "incomplete_cases") <- attr(object, "incomplete_cases")
  attr(anova_table, "sig_symbols") <- 
    if(!is.null(sig_symbols)) sig_symbols else afex_options("sig_symbols")
  anova_table
}

#' @rdname afex_aov-methods
#' @method print afex_aov 
#' @importFrom stats symnum
#' @export
print.afex_aov <- function(x, ...) {
  out <- nice(x$anova_table, ...)
  print(out)
  invisible(out)
}


#' @rdname afex_aov-methods
#' @method summary afex_aov 
#' @export
summary.afex_aov <- function(object, ...) {
  if (inherits(object$Anova, "Anova.mlm")) {
  #if (class(object$Anova)[1] == "Anova.mlm") {
    if(attr(object$anova_table, "p_adjust_method") != "none") 
      message("Note, results are NOT adjusted for multiple comparisons as requested\n(p_adjust_method = '", 
              attr(object$anova_table, "p_adjust_method"), 
              "')\nbecause the desired method of sphericity correction is unknown.\nFor adjusted p-values print the object (to see object$anova_table), or call\none of anova.afex_aov() or nice().")
    return(summary(object$Anova, multivariate = FALSE))
  #} else if (class(object$Anova)[1] == "anova") {
  } else if (inherits(object$Anova, "anova")) {
    return(object$anova_table)
  } else stop("Non-supported object passed. Slot 'Anova' needs to be of class 'Anova.mlm' or 'anova'.")
}


#--------------------------------------------------------------
### afex package - mixed objects ###
# just need to provide an 'emmeans' method here

#' @rdname afex_aov-methods
## @importFrom emmeans recover_data emm_basis
#' @importFrom utils packageVersion
## @method recover.data afex_aov 
## @export
recover_data.afex_aov = function(object, ..., 
                                 model = afex_options("emmeans_model")) {
  model <- match.arg(model, c("univariate", "multivariate"))
  if (model == "univariate" & is.null(object$aov)) {
    message("aov object missing, substituting multivariate/lm model.\nto get univariate tests, refit ANOVA with include_aov = TRUE")
    model <- "multivariate"
  }
  if (model == "univariate") {
    emmeans::recover_data(object = object$aov, ...)
  } else if (model == "multivariate") {
    if (packageVersion("emmeans") < "1.1.2")
      stop("emmeans version >= 1.1.2 required for multivariate tests")
    out <- emmeans::recover_data(object$lm, ...)  
    if (length(attr(object, "within")) > 0) {
      out$misc$ylevs <- rev(attr(object, "within")) 
    }
    return(out)
  }
    
}

#' @rdname afex_aov-methods
## @method lsm.basis afex_aov 
## @export
emm_basis.afex_aov = function(object, trms, xlev, grid, ..., 
                              model = afex_options("emmeans_model")) {
  model <- match.arg(model, c("univariate", "multivariate"))
  if (model == "univariate" & is.null(object$aov)) {
    #message("Substituting multivariate/lm model, as aov object missing.")
    model <- "multivariate"
  }
  if (model == "univariate") {
    out <- emmeans::emm_basis(object$aov, trms, xlev, grid, ...)
  } else if (model == "multivariate") {
    out <- emmeans::emm_basis(object$lm, trms, xlev, grid, ...)
    if (length(attr(object, "within")) > 0) {
      out$misc$ylevs <- rev(attr(object, "within")) 
    }
  }
  out$misc$tran = attr(object, "transf")
  return(out)
}

