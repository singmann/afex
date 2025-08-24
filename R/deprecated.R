#' Deprecated functions
#'
#' These functions have been renamed or moved and deprecated in \pkg{afex}:
#' \code{test_levene()} (use \code{\link[performance]{check_homogeneity}()}),
#' \code{test_sphericity()} (use \code{\link[performance]{check_sphericity}()}).
#' @rdname deprecated
#' @keywords internal
#' @aliases afex-deprecated
#' @param ... arguments passed from the old functions of the style
#'   \code{foo.bar()} to the new functions \code{foo_bar()}
#' @export
test_levene <- function(...) {
  .Deprecated(
    "check_homogeneity",
    "afex",
    "Functionality has moved to the 'performance' package.\nCalling 'performance::check_homogeneity()'."
  )
  performance::check_homogeneity(...)
}

#' @rdname deprecated
#' @export
test_sphericity <- function(...) {
  .Deprecated(
    "check_sphericity",
    "afex",
    "Functionality has moved to the 'performance' package.\nCalling 'performance::check_sphericity()'."
  )
  performance::check_sphericity(...)
}


warn_deprecated_arg <- function(name, instead) {
  warning(
    gettextf("'%s' is deprecated; use '%s' instead", name, instead),
    call. = FALSE,
    domain = NA
  )
}
