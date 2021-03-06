% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ternaryText.R
\name{ternaryText}
\alias{ternaryText}
\alias{ternaryText.ternaryPolygons}
\alias{ternaryText.ternarySystem}
\title{Add Text to a ternary plot}
\usage{
ternaryText(s, ...)

\method{ternaryText}{ternarySystem}(s, x, labels, adj = NULL, pos = NULL,
  offset = 0.5, vfont = NULL, cex = 1, col = NULL, font = NULL,
  what = "centroids", ...)

\method{ternaryText}{ternaryPolygons}(s, x, labels, adj = NULL, pos = NULL,
  offset = 0.5, vfont = NULL, cex = 1, col = NULL, font = NULL, ...)
}
\arguments{
\item{s}{A \code{\link[ternaryplot]{ternarySystem}} object, or a 
character string naming a pre-defined \code{\link[ternaryplot]{ternarySystem-class}}. 
If missing, set to \code{default}.}

\item{x}{A \code{\link[base]{data.frame}} or a 
\code{\link[base]{matrix}} containing ternary data-points, 
coordinates of the text strings to be added on the plot.}

\item{labels}{A vector of character strings, or expressions to be added 
on the triangle plot. See \code{\link[graphics]{text}}.}

\item{adj}{See \code{\link[graphics]{text}}.}

\item{pos}{See \code{\link[graphics]{text}}.}

\item{offset}{See \code{\link[graphics]{text}}.}

\item{vfont}{See \code{\link[graphics]{text}}.}

\item{cex}{See \code{\link[graphics]{text}}.}

\item{col}{See \code{\link[graphics]{text}}.}

\item{font}{See \code{\link[graphics]{text}}.}

\item{what}{Single character string. When \code{x} is a missing 
and \code{s} is a \code{\link[ternaryplot]{ternarySystem-class}}, what points 
should be extracted: the class vertices 
(\code{what = 'vertices'}, the default) 
or the class centroids 
(\code{what = 'centroids'})}

\item{\dots}{Additional parameters passed to \code{\link[graphics]{text}}.}
}
\value{
Invisibly returns the graphical element as x-y coordinates.
}
\description{
Add Text to a ternary plot
}
\seealso{
\code{\link[graphics]{text}}.
}

