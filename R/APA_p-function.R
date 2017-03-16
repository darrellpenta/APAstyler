#' APA style options for p values
#'

#' \code{APA_p} takes a number (numeric or character) and returns it with appropriate APA formatting, with options for print-friendly labelling. For p values less than .001, APA recommends using p < .001. Otherwise, numbers can be returned as exact values (rounded and without a leading zero; e.g., \strong{p = .003}) for reporting p values in text, or as values in a range (for tables, etc.; e.g., \strong{p < .05}).
#' @param stat A number, supplied as a numeric or character value.
#' @param digit How many significance digits (defaults to 3)? Passed to \code{\link[formattable]{formattable}} with options \code{format = "f"} and \code{digits = digit}.
#' @param as_range Should values be returned as exact numbers (rounded, with no leading zero)? Defaults is \code{TRUE}. \code{FALSE} returns numbers with greater than/ less than indicators and no leading zero.
#' @param rmd_format Should p value be return with a Latex/rMarkdown-friendly label and equal sign prepended (e.g., \strong{"\\\\textit{p} $=$ .012"}). Default is \code{TRUE}.
#' @return \code{stat} as a string with APA formatting and/or other options applied.
#' @examples
#'
#' # lapply(runif(n = 100, min = 0.00003, max = .99), APA_p, rmd_format = TRUE)
#' @export


APA_p = function(stat,
                 digit = 3,
                 as_range = FALSE,
                 rmd_format = TRUE) {
  stat =
    ifelse(is.character(stat),
           as.numeric(stat),
           stat)
  if(stat > 1) {
     stop("P value > 1? Something's not right.")} else{

  if (isTRUE(as_range)) {
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
          APAstyler::snip(formattable::formattable(
            stat,
            format = "f",
            digits = digit
          ),lead_zero=TRUE)
        ),
        APAstyler::snip(formattable::formattable(
          stat,
          format = "f",
          digits = digit
        ),
        lead_zero=TRUE
        )
      )
    )
    codes[range]

  }
     }
}
