#' Helper functions for rounding p-values
#'
#' These functions return a character vector of p-values that are rounded as
#' described below and without the leading zero before the decimal point.
#'
#' For \code{round_ps} p-values are rounded in a sane way: .99 - .01 to two
#' digits, < .01 to three digits, < .001 to four digits.
#'
#' For \code{round_ps_apa} p-values are rounded following APA guidelines: .999 -
#' .001 to three digits, and < .001 for values below this threshold.
#'
#' @param x a numeric vector
#'
#' @return A character vector with the same length as x.
#'
#' @note These functions are useful in \code{\link{nice}} and the default is set
#'   via \code{\link{afex_options}}.
#'
#' @author Henrik Singmann
#'
#' @encoding UTF-8
#'
#' @examples
#' x <- runif(10)
#' y <- runif(10, 0, .01)
#'
#' round_ps(x)
#' round_ps_apa(x)
#'
#' round_ps(y)
#' round_ps_apa(y)
#'
#' round_ps(0.0000000099)
#' round_ps_apa(0.0000000099)
#'
#' @export
round_ps <- function(x) {
  substr(
    as.character(ifelse(
      x < 0.0001,
      " <.0001",
      ifelse(
        x < 0.001,
        formatC(x, digits = 4, format = "f"),
        ifelse(
          x < 0.01,
          formatC(x, digits = 3, format = "f"),
          ifelse(
            round(x, 2) == 1,
            " >.99",
            formatC(x, digits = 2, format = "f")
          )
        )
      )
    )),
    2,
    7
  )
}

#' @rdname round_ps
#' @export
round_ps_apa <- function(x) {
  substr(
    as.character(
      ifelse(
        x < 0.001,
        " <.001",
        ifelse(round(x, 3) == 1, " >.999", formatC(x, digits = 3, format = "f"))
      )
    ),
    2,
    7
  )
}
