#' Format a statistic
#'
#' \code{APA_stat} allows you to append your own labels to a statistic.
#' @param stat A number, supplied as a numeric or character value.
#' @param pre_stat A string to be prepended to you number. Default is \code{NULL}.
#' @param post_stat A string to be appended to your number. Default is \code{NULL}.
#' @param snip Do you want to snip a leading zero from a number bounded by 1? Defaults to \code{FALSE}.
#' @param digit How many significance digits (defaults to 2, which is suitable for many APA applications)? Passed to \code{\link[formattable]{formattable}}.
#' @param ... Optional arguments to be passed to \code{\link[formattable]{formattable}} or \code{paste0}
#' @return \code{stat} as a string with APA formatting and/or other options applied.
#' @examples
#'
#' # lapply(runif(n = 100, min = 0.00003, max = .99), APA_p, rmd_format = TRUE)
#' @export


APA_stat = function(stat,
                    pre_stat = NULL,
                    post_stat = NULL,
                    snip = FALSE,
                    digit = 2,
                    ...) {
  stat =
    ifelse(is.character(stat),
           as.numeric(stat),
           stat)

    dots <- list(...)
    if(length(dots) == 0){
      return(NULL)
      } else {

        stat <-
          paste0(
            ifelse(is.null(pre_stat),NULL,pre_stat),
            ifelse(isTRUE(snip),
                   snip(
                     formattable::formattable(
                       stat,
                       digits=digit,
                       ...)
                     ),
                   formattable::formattable(
                     stat,
                     digits=digit,
                     ...)
                   ),
              ifelse(
                is.null(post_stat),
                NULL,
                post_stat)
                   )

  }
}
