% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aa02-ternaryplot-classes.R
\name{ternaryVariables-class}
\alias{ternaryVariables-class}
\title{Class to hold the variables' names and labels of ternary plots and ternary data.}
\description{
Class (\code{S3}) to hold the variables' names and labels 
 of ternary plots (graphics) and ternary data. A 
 \code{ternaryVariables}-class object is 
 a tagged \code{\link[base]{list}} of several 
 objects/items (see below). By variables' names is meant 
 the name of the variables in a ternary data (column names), 
 and by variables' labels is meant the axis labels on a 
 ternary plot. A 'tagged' list means a list of 
 the form \code{list( "tag1" = value1, "tag2" = value2, 
 ... )}.
}
\details{
The \code{S3} \code{\link[base]{class}} system is an 
 \emph{in}formal and lightweight class system.
}
\section{List objects/items}{

\itemize{
 \item \bold{\code{blrNames}}: Vector of 3 character strings. 
   column names of the variables to be displayed on the 
   \bold{b}ottom, \bold{l}eft, and \bold{r}ight axis of a 
   ternary plot. If \code{blrNames} is set to 
   \code{c( "_bottom_","_left_","_right_" )}, 
   \code{\link[ternaryplot]{ternarySystem-class}}es based 
   on the set of \code{ternaryVariables} will be 
   \emph{undefined}, meaning that any variable name goes. 
   In the case of ternary data points, it means that the 
   first three columns will be used independently of how 
   they are called.
 \item \bold{\code{blrLabels}}: Vector of 3 character 
   strings or vector of 3 expressions.\bold{b}ottom, 
   \bold{l}eft, and \bold{r}ight axis' labels of a ternary 
   plots. If \code{blrNames} is set 
   to \code{c( "_bottom_","_left_","_right_" )}, and 
   \code{blrLabels} to \code{rep( NA_character_, 3 )}, 
   \code{\link[ternaryplot]{ternaryPlot}} and 
   \code{\link[ternaryplot]{ternaryPoints}} use the column 
   names of the dataset that is provided (if provided).
}
}
\seealso{
\itemize{
 \item \code{\link[ternaryplot]{ternarySystem-class}} 
  (full ternary system definition), that builds on 
   \code{\link[ternaryplot]{ternaryGeometry-class}}, 
   \code{ternaryVariables-class} and other settings.
 \item \code{\link[ternaryplot]{createTernaryVariables}} 
   for setting up a \code{ternaryVariables-class} object.
 \item \code{\link[ternaryplot]{blrNames}} for fetching 
   or setting the variables names and 
   \code{\link[ternaryplot]{blrLabels}} for fetching 
   or setting the axis labels.
 \item \code{\link[ternaryplot]{ternaryCheck}}  
   for checking the validity of a 
   \code{ternaryVariables-class} object.
}
}

