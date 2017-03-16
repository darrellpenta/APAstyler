#' Return a Std. Error of the Mean (SEM) value in APA style
#'

#' \code{APA_sem} takes a number (numeric or character) and returns it with appropriate APA formatting, with options for print-friendly labelling.
#'
#' @param stat A number, supplied as a numeric or character value.
#' @param rmd_format Either \code{TRUE} (the default) to return the number with a Latex/rMarkdown-friendly test label and equal sign prepended (e.g., \strong{"\\\\textit{MS$_e$} $=$ 12.34"}); or, \code{FALSE}, to return just the number with rounding.
#' @param digit How many significant digits (defaults to 3)? Passed to \code{\link[formattable]{formattable}} with options \code{format = "f"} and \code{digits = digit}.
#' @return A string.
#' @examples
#'
#' # lapply(runif(n = 100, min = 0.10, max = 10.0), APA_sem)
#' @export

APA_sem = function(stat,
                   rmd_format = TRUE,
                   digit = 3) {
  stat <- ifelse(is.character(stat), as.numeric(stat), stat)
    stat <-
      ifelse(isTRUE(rmd_format),
             paste0(
        "\\textit{MS$_e$} $=$  ",
        formattable::formattable(stat,
                                 format = "f",
                                 digits = digit)
      ),
      paste0(formattable::formattable(stat,
                                      format = "f",
                                      digits = digit)))}
