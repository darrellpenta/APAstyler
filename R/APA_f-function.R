#' Return an F statistic in APA style
#'

#' \code{APA_f} takes a number (numeric or character) and returns it with appropriate APA formatting, with options for print-friendly labelling.
#'
#' @param stat A number, supplied as a numeric or character value.
#' @param type Either \code{NULL}, to return the just the number; or, if a Latex/rMarkdown-friendly label is desired, one of \code{"f","f1","f2"}, to return the value with the test label and equal sign prepended (e.g., \strong{"\\\\textit{F} $=$ \code{stat}"}; \strong{"\\\\textit{F$_1$} $=$ \code{stat}"}; \strong{"\\\\textit{F$_2$} $=$ \code{stat}"}.
#' @param digit How many significant digits (defaults to 2)? Passed to \code{\link[formattable]{formattable}} with options \code{format = "f"} and \code{digits = digit}.
#' @return \code{stat} as a string with APA formatting and/or other options applied.
#' @examples
#'
#' # lapply(runif(n = 100, min = 0.10, max = 10.0), APA_f, type = "f")
#' @export
APA_f = function(stat,
                 type = c("f", "f1", "f2"),
                 digit = 2) {
  stat <-ifelse(is.character(stat),as.numeric(stat),stat)

  if (missing(type)) {
    stat <-
      paste0(snip(formattable::formattable(stat,
                                      format = "f",
                                      digits = digit)))
  } else{
    if (match.arg(type) == "f") {
      stat <-
        paste0("\\textit{F} $=$ ",
               snip(formattable::formattable(stat,
                                        format = "f",
                                        digits = digit)))
    } else{
      if (match.arg(type) == "f1") {
        stat <-
          paste0(
            "\\textit{F$_1$} $=$ ",
            snip(formattable::formattable(stat,
                                     format = "f",
                                     digits = digit)
          ))
      } else{
        if (match.arg(type) == "f2") {
          stat <-
            paste0(
              "\\textit{F$_2$} $=$ ",
              snip(formattable::formattable(stat,
                                       format = "f",
                                       digits = digit)
            ))
        }
      }
    }
  }
}
