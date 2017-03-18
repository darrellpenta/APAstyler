#' Return p-value significance star(s) from a number.
#'
#'
#' Uses the standard range of intervals to determine the nubmer of stars.
#'
#' @param p A number for which a p-value star should be returned:
#'
#'  \describe{
#'   \item{\strong{***}}{0 < \emph{p} < 0.001}
#'   \item{\strong{**}}{0.001 < \emph{p} < 0.01}
#'   \item{\strong{*}}{0.01 < \emph{p} < 0.5}
#'   \item{\strong{.}}{0.05 < \emph{p} < 0.1}
#' }
#'
#' @param show_ns Show "n.s." for non-significant values? Default = TRUE
#'
#' @return A string.
#' @export


p_stars = function(p, show_ns=TRUE) {
  p =
    ifelse(is.na(p)," ",ifelse(is.character(stat),
                                  as.numeric(stat),
                                  stat))
  stars = findInterval(p, c(0, 0.001, 0.01, 0.05, 0.1))
  codes = c("***" , "**", "*", ".", ifelse(isTRUE(show_ns),"n.s.",""))
  codes[stars]
}
options(scipen = 1)
