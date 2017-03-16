#' Remove leading zero and/or trailing zero from a number.
#'
#' Useful for APA-style reporting of numbers bounded by 1/-1 (e.g., converts 0.23 to .23); can aslo be used to drop a trailing zero (e.g., converts 2.270 to 2.27) . NOTE: that this converts the number to a character vector and is not guaranteed to produce the appropriate result in all cases.
#'
#' @param x A numeric value, coercible to a character.
#' @param lead_zero Remove a zero to the left of the decimal for bounded numbers? Default is \code{FALSE}.
#' @param trail_zero = Optional. Set minimum number of digits to the right of the decimal to keep, and then remove the next digit if it is a zero, or else preserve it; if \code{trail_zero} > the actual number of digits  of \code{x}, returns \code{x} unchanged.
#' @return \code{x} as a string with leading and/or trailing zero removed
#' @examples
#'
#' # Drop leading zero
#' cat(snip("0.2202",lead_zero = TRUE))
#'
#' # Drop leading zero, keep a minimum of 2 digits to the right, or drop if 0
#' cat(snip("0.2202",lead_zero = TRUE,2))
#' cat(snip("0.2230",lead_zero = TRUE,2))
#'
#' @export

snip = function(x,
                lead_zero = FALSE,
                trail_zero) {


  x_left <-
    strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE)[[1]][[1]]
  x_right <-
    strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE)[[1]][[2]]


  x_left <-

    if(isTRUE(lead_zero)){
      ifelse(x_left == "0","",x_left)
    } else {
    x_left
    }

if(missing(trail_zero)){
  trail_zero <- as.integer(nchar(x_right))
}
if(is.na(as.integer(trail_zero))) {
  stop("trail_zero must be an integer or else coercible into one")
}

if(as.integer(trail_zero) > nchar(x_right)) {
  trail_zero <- as.integer(nchar(x_right))
}

x_right_out_keep <-
      substr(x_right,
             start = 1,
             stop = trail_zero)
check_zero <- trail_zero + 1

x_right_out_check <-
  substr(x_right,
         start = check_zero,
         stop = check_zero)

if(x_right_out_check == "0"){
  x_right <-
    x_right_out_keep
}

if(x_right_out_check != "0"){
  x_right <-
    paste0(x_right_out_keep,x_right_out_check)
}

x <-
    paste(x_left, x_right, sep = ".")
}
