#' Set global contrasts
#' 
#' These functions are simple wrappers to set contrasts globally via \code{options(contrasts = ...)}.
#' 
#' @usage set_sum_contrasts()
#' 
#' set_deviation_contrasts()
#' 
#' set_effects_contrasts()
#' 
#' set_default_contrasts()
#' 
#' set_treatment_contrasts()
#' 
#' 
#' @details \code{set_deviation_contrasts} and \code{set_effects_contrasts} are wrappers for \code{set_sum_contrasts}. Likewise, \code{set_default_contrasts} is a wrapper to \code{set_treatment_contrasts()}.
#'
#' @return nothing. These functions are called for their side effects to change the global options.
#'
#' @name set_sum_contrasts
#' @aliases set_sum_contrasts set_deviation_contrasts set_effects_contrasts set_treatment_contrasts set_default_contrasts
#' @export set_sum_contrasts set_deviation_contrasts set_effects_contrasts set_treatment_contrasts set_default_contrasts
#' 

set_sum_contrasts <- function() {
  message("setting contr.sum globally: options(contrasts=c('contr.sum', 'contr.poly'))")
  options(contrasts=c('contr.sum', 'contr.poly'))
}

set_deviation_contrasts <- function() {
  set_sum_contrasts()
}

set_effects_contrasts <- function() {
  set_sum_contrasts()
}

set_treatment_contrasts <- function() {
  message("setting contr.treatment globally: options(contrasts=c('contr.treatment', 'contr.poly'))")
  options(contrasts=c('contr.treatment', 'contr.poly'))
}

set_default_contrasts <- function() {
  set_treatment_contrasts()
}
