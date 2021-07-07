#' Deprecated functions
#'
#' These functions have been renamed or moved and deprecated in \pkg{afex}:
#' \code{aov.car()} (use \code{\link{aov_car}()}), 
#' \code{ez.glm()} (use \code{\link{aov_ez}()}),
#' \code{aov4()} (use \code{\link{aov_4}()}),
#' \code{test_levene()} (use \code{\link[performance]{check_homogeneity}()}),
#' \code{test_sphericity()} (use \code{\link[performance]{check_sphericity}()}).
#' @rdname deprecated
#' @keywords internal
#' @aliases afex-deprecated
#' @param ... arguments passed from the old functions of the style
#'   \code{foo.bar()} to the new functions \code{foo_bar()}
#' @export
aov.car <- function(...) {
  .Deprecated("aov_car", "afex", "aov.car was renamed to aov_car and is now deprecated.")
  aov_car(...)
}
#' @rdname deprecated
#' @export
ez.glm <- function(...) {
  .Deprecated("aov_ez", "afex", "ez.glm was renamed to aov_ez and is now deprecated.")
  aov_ez(...)
}
#' @rdname deprecated
#' @export
aov4 <- function(...) {
  .Deprecated("aov_4", "afex", "aov4 was renamed to aov_4 and is now deprecated.")
  aov_4(...)
}

#' @rdname deprecated
#' @export
test_levene <- function(...){
  .Deprecated("check_homogeneity", "afex", "Functionality has moved to the 'performance' package.\nCalling 'performance::check_homogeneity()'.")
  performance::check_homogeneity(...)
}

#' @rdname deprecated
#' @export
test_sphericity<- function(...){
  .Deprecated("check_sphericity", "afex", "Functionality has moved to the 'performance' package.\nCalling 'performance::check_sphericity()'.")
  performance::check_sphericity(...)
}



warn_deprecated_arg <- function(name, instead) {
  warning(gettextf("'%s' is deprecated; use '%s' instead", name, instead),
          call.=FALSE, domain=NA)
}
        

