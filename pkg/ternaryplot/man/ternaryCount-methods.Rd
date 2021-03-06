% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ternaryCount.R
\name{ternaryCount}
\alias{ternaryCount}
\alias{ternaryCount.character}
\alias{ternaryCount.ternaryPolygons}
\alias{ternaryCount.ternarySystem}
\title{Count the number of ternary data point falling in each cell or class}
\usage{
ternaryCount(s, x, ...)

\method{ternaryCount}{character}(s, x, grid = TRUE, ...)

\method{ternaryCount}{ternarySystem}(s, x, grid = TRUE, ...)

\method{ternaryCount}{ternaryPolygons}(s, x, ...)
}
\arguments{
\item{s}{Either \itemize{
  \item A \code{\link[ternaryplot]{ternarySystem-class}}. 
    That \code{ternarySystem-class} must contain 
    a ternary classification. 
  \item A single character string, the name of an existing 
    pre-defined \code{\link[ternaryplot]{ternarySystem-class}}.
  \item A \code{\link[ternaryplot]{ternaryPolygons-class}}.
}}

\item{x}{A \code{\link[base]{data.frame}} or a 
\code{\link[base]{matrix}} containing point ternary data 
to be binned and counted. It should contain the 3 columns 
names given by \code{blrNames(s)}.}

\item{grid}{Single logical value. Set to \code{TRUE} (the default) 
to retrieve counts for a systematic ternary grid, and 
to \code{FALSE} to count the number of \code{x}-data 
points per class in \code{s} instead.}

\item{\dots}{Additional parameters passed to specific methods. Not used.}
}
\value{
Returns a \code{\link[ternaryplot]{ternaryPolygons-class}} 
 object with one extra \code{\link[base]{attr}}ibute, 
 \code{data}. \code{data} is a 
 \code{\link[base]{data.frame}} containing two columns: 
 an identifier column, called \code{id} if \code{grid} is 
 \code{TRUE} or \code{abbrev} if \code{grid} is \code{FALSE} 
 (the class abbreviations), and a \code{counts} column.
}
\description{
Count the number of ternary data point falling in each cell 
 of a systematic triangular grid (if \code{grid=TRUE}) or 
 falling in each class of a ternary classification 
 (if \code{grid=FALSE}) or in each polygon of a 
 \code{\link[ternaryplot]{ternaryPolygons-class}} object 
 (if \code{s} is a 
 \code{\link[ternaryplot]{ternaryPolygons-class}} object). 
 This function is used as a basis for the plot function 
 \code{\link[ternaryplot]{ternaryBins}} (binning and plotting 
 bins of ternary data)
}
\examples{

library( "ternaryplot" ) 

#   Load an example dataset ( dummy example )
data( "textureDataset", package = "ternaryplot" ) 



# Count the number of data-points per grid-cell
# =====================================================

counts1 <- ternaryCount( s = "hypres", x = textureDataset ) 

head( attr( x = counts1, which = "data" ) )



# Count the number of data-points per ternary class
# =====================================================

counts2 <- ternaryCount( s = "hypres", x = textureDataset, 
    grid = FALSE ) 

attr( x = counts2, which = "data" )


}
\seealso{
\code{\link[ternaryplot]{createTernaryGrid}} for 
 generating a systematic triangular grid and 
 \code{\link[ternaryplot]{ternaryBins}} for binning and 
 plotting bins of ternary data
}

