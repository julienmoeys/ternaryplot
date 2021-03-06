% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ternaryStyle.R
\name{ternaryStyle}
\alias{ternaryStyle}
\alias{ternaryStyle.character}
\title{Style for ternary plots (predefined sets of graphical parameters)}
\usage{
ternaryStyle(style, margin = FALSE)

\method{ternaryStyle}{character}(style, margin = FALSE)
}
\arguments{
\item{style}{Single character string. Name of the style to be used. 
Possible values are \code{publication} and \code{ggplot2}.}

\item{margin}{Single logical value. If \code{TRUE}, the function also 
sets narrower plot margins, so that the ternary plot 
can be (for example) a bit larger. Note that not all 
style may set the margins.}
}
\description{
Style for ternary plots (predefined sets of graphical 
 parameters)
}
\examples{
#   See ?ternaryPlot for an example
}
\seealso{
\code{\link[ternaryplot]{ternaryPlot}}, that can 
 be called \emph{after} \code{ternaryStyle} has been 
 called (and which uses these graphical parameters).
}

