% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aa01-soiltexture2-options.R
\name{stPar}
\alias{stPar}
\title{Get or set default parameters for the package.}
\usage{
stPar(par = NULL, reset = FALSE, dummy)
}
\arguments{
\item{par}{Three possible cases: \itemize{ \item If \code{par} is \code{NULL}
(default): All the actual value of the parameters will be silently returned.
\item If \code{par} is a vector of character strings representing parameter
names. The value of the parameters named here will be (silently) returned.
\item If \code{par} is a list following the format \code{tag = value}, where
\code{tag} is the name of the parameter to be changed, and \code{value} is
its new value.  Such a list is returned by \code{stPar()}. Notice that
parameters can also be set individually, using the options listed below. }}

\item{reset}{Single logical. If TRUE, all the parameters will be set to their
default value. Values are reset before any change to the parameter values, as
listed below.}

\item{dummy}{A dummy argument}
}
\value{
Returns a partial or complete list of (actual) parameter 
 values, as a tagged list.
}
\description{
Get or set default parameters for the package. Notice changes 
 done to the parameter values are reset everytime the R session 
 is closed and the package is reloaded.
}
\details{
The function has 3 possible, non-exclusive behaviours: \itemize{ \item If
 \code{reset=TRUE}, resetting the parameters to their initial values, as
 defined in this function. \item (Silently) returning the actual value of the
 package parameters. If \code{par=NULL}, all the values are returned.  If
 \code{par} is a vector of parameter names, their value will be returned.
 \item Setting-up the value of some parameters, passing a list of parameter
 value to \code{par} OR setting some of the parameters listed above. }

 Notice that when \code{reset=TRUE} and some new parameter values are
 provided, the parameters are first reset, and then the new parameter values
 are set. If \code{par} is a list, parameters are set first according to
 values in \code{par}, and then according to values in the parameters listed
 below. This combination is not recommended, but nonetheless possible.

 The actual value of the parameters is stored in (and can be retrieved from)
 the environment \code{rspPars}. The default value of the parameters are
 stored in the environment \code{rspPars}. Do not use them directly.
}
\seealso{
\code{\link{getStPar}}.
}

