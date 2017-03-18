#' Remove leading zero and/or trailing zero from a statistic
#'
#' Use \code{snip()} if you need to trim a leading zero (left of decimal) or trailing zero (right of decimal) from a value (esp. for reporting stats in APA format).
#'
#' @param x A numeric value, coercible to a character.
#' @param lead Optional number of zeros to remove from the left of the decimal. Default is \code{NULL}.
#' See \strong{Leading Zero} for details.
#' @param trail Position for evaluating whether a trailing zero should be dropped? Defaults to \code{3}.
#' See \strong{Trailing Zero} for details.
#' @section  Leading zero:
#'  For values that cannot exceed 1,  (e.g., \emph{p} values), \strong{\href{http://blog.apastyle.org/apastyle/statistics/}{APA}} recommends removing the zero to the left of the decimal.
#'
#'  For example \emph{p} = 0.023 should be reported as \emph{p} = .023. In this case, \code{snip(0.023), lead = 1} would work.
#'
#'  If, for some reason, you need to snip more than one zero, you can specify the number.
#'
#'   Only exact matches (e.g., 1 or 2 "0"s, set by \code{lead}) for \code{x < 1} will be snipped; otherwise, the value to the left of the decimal is left unchanged.
#'
#' @section  Trailing zero:
#'  The \strong{\href{http://blog.apastyle.org/apastyle/statistics/}{APA}} recommends reporting statistics with decimal fractions rounded to two or three places (in most circumstances).
#'
#'  When \code{trail = 3}, the default behavior is to assume that a minimium of 2 places after the decimal should be preserved under all conditions. This basically sets the position being evaluated \strong{= 3}, and assumes the APA-recommended floor threshold of .001: If there is a zero in this position, the zero is snipped.
#'
#'  See examples and \code{\link{APA_p}}.
#'
#'
#' @return  \code{x} as a string with leading and/or trailing zero removed.

#' @examples
#'
#' # Drop leading zero
#' cat(snip("0.22",1))
#'
#' # Drop trailing zero
#' cat(snip("0.220"))
#'
#' @export

snip = function(x,
                lead = NULL,
                trail = 3) {
  x <-
ifelse(formattable::is.formattable(x),as.character(x[1]),x)
  x_left <-
    strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE)[[1]][[1]]
  x_right <-
    strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE)[[1]][[2]]


  if (is.null(lead)) {
    x_left <-
      x_left
  } else if (!(is.numeric(lead))) {
    stop("lead_zero argument must be numeric")
  } else {
    left_zeros <-
      stringr::str_dup("0", lead)
    x_left <-
      ifelse(x_left == left_zeros, "", x_left)
  }

  if (is.na(as.integer(trail))) {
    stop("trail must be an integer or else coercible into one")
  }

  if (nchar(x_right) > trail) {
    x_right <-
      ifelse(
        substr(x_right,
               start = trail + 1,
               stop =  trail + 1) == "0" & substr(x_right,
                                                  start = trail,
                                                  stop =  trail) != "0",
        substr(x_right,
               start = 1,
               stop =  trail),
        x_right
      )
  } else {
    x_right <-
      ifelse(
        substr(x_right,
               start = trail,
               stop =  trail) == "0",
        substr(x_right,
               start = 1,
               stop =  trail - 1),
        substr(x_right,
               start = 1,
               stop =  trail)
      )
  }

  x <-
    paste(x_left, x_right, sep = ".")

}
