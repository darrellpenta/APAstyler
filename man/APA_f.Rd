% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/APA_f-function.R
\name{APA_f}
\alias{APA_f}
\title{Return an F statistic in APA style}
\usage{
APA_f(stat, type = c("f", "f1", "f2"), digit = 2)
}
\arguments{
\item{stat}{A number, supplied as a numeric or character value.}

\item{type}{Either \code{NULL}, to return the just the number; or, if a Latex/rMarkdown-friendly label is desired, one of \code{"f","f1","f2"}, to return the value with the test label and equal sign prepended (e.g., \strong{"\\\\textit{F} $=$ \code{stat}"}; \strong{"\\\\textit{F$_1$} $=$ \code{stat}"}; \strong{"\\\\textit{F$_2$} $=$ \code{stat}"}.}

\item{digit}{How many significant digits (defaults to 2)? Passed to \code{\link[formattable]{formattable}} with options \code{format = "f"} and \code{digits = digit}.}
}
\value{
\code{stat} as a string with APA formatting and/or other options applied.
}
\description{
\code{APA_f} takes a number (numeric or character) and returns it with appropriate APA formatting, with options for print-friendly labelling.
}
\examples{

# lapply(runif(n = 100, min = 0.10, max = 10.0), APA_f, type = "f")
}
