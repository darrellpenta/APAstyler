#' APA style options for \emph{p} values
#'
#' \code{APA_p} takes a number (numeric or character) and returns it with appropriate APA formatting, with options for print-friendly labelling.
#'
#' @param stat A number, supplied as a numeric or character value.
#' @param digit How many significance digits? Default = \code{3}
#'
#' Passed to \code{\link[formattable]{formattable}} with options \code{format = "f"} and \code{digits = digit}.
#'
#' @param not_range Should full \emph{p} values be returned instead of significance ranges? Default is \code{TRUE}. \code{FALSE} returns numbers with greater than/ less than indicators and no leading zeros.
#' See \strong{P values}.
#'
#' @param rmd_format Should \emph{p} value be return with a Latex/rMarkdown-friendly label and equal sign prepended (e.g., \strong{"\\\\textit{p} $=$ .012"}).
#' Default is \code{TRUE}.
#'
#' @section P values:
#' For p values less than .001, \strong{\href{http://blog.apastyle.org/apastyle/statistics/}{APA}} recommends using \strong{\emph{p} < .001}. Otherwise, full \emph{p} values should be used, rounded to two or three decimal places, and without a leading zero (e.g., \strong{\emph{p} = .003}) when reporting \emph{p} values in text, or as values in a range (for tables, etc.; e.g., \strong{\emph{p} < .05}). See \code{\link{snip}} for details about leading and trailing zeros.
#'

#' @return \code{stat} as a string with APA formatting and/or other options applied.
#' @examples
#' # lapply(runif(n = 100, min = 0.00003, max = .99), APA_p, rmd_format = TRUE)
#' @export


APA_p = function(stat,
                 digit = 3,
                 not_range = TRUE,
                 rmd_format = TRUE) {

if(missing(digit)){
  digit=3
}
if(missing(not_range)){
  not_range = TRUE
}
if(missing(rmd_format)){
  rmd_format=TRUE
}



  stat =
    ifelse(is.character(stat),
           as.numeric(stat),
           stat)
  if(stat > 1) {
     stop("P value > 1? Something's not right.")} else{

  if (!(isTRUE(not_range))) {
    range = findInterval(stat, c(0, 0.001, 0.01, 0.05, 0.1, 0.99))
    codes = c(
      ifelse(rmd_format == TRUE, "\\textit{p} < .001", "< .001"),
      ifelse(rmd_format == TRUE, "\\textit{p} < .01", "< .01"),
      ifelse(rmd_format == TRUE, "\\textit{p} < .05", "< .05"),
      ifelse(rmd_format == TRUE, "\\textit{p} < .10", "< .10"),
      ifelse(rmd_format == TRUE, "\\textit{p} > .10", "> .10"))
    codes[range]

  } else {
    range = findInterval(stat, c(0, 0.001, 0.99))
    codes = c(
      ifelse(isTRUE(rmd_format),
             "\\textit{p} < .001",
             " < .001"),
      ifelse(
        isTRUE(rmd_format),
        paste0(
          "\\textit{p} $=$ ",
          snip(formattable::formattable(
            stat,
            format = "f",
            digits = digit
          ),lead = 1
        )),
      snip(formattable::formattable(
          stat,
          format = "f",
          digits = digit
        ), lead = 1)
    )
    )
    codes[range]

  }
     }
}



