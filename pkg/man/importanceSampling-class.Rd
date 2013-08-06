\name{importanceSampling-class}
\Rdversion{1.1}
\docType{class}
\alias{importanceSampling-class}

\title{Class \code{"importanceSampling"}}

\description{
   This is the class definition that allows for the
  application of importance sampling to \code{downLog} or
  \code{standingTree} objects. Examples of the class usage can be found
  in the Monte Carlo sampling vignette referenced below.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("importanceSampling", ...)}.
 However, an object constructor
of the same name, \code{\link{importanceSampling}}, has been provided and
is the preferred method for creating objects that are ensured to be
valid.}

\section{Slots}{

  Please note that diameters below are presumed to be in the
  \emph{same} units as length, i.e., meters for \dQuote{metric}, and
  feet for \dQuote{English} \code{units}. Cross-sectional areas are in
  compatible units.

  In addition to the slots provided by the virtual superclass
  \dQuote{\code{\linkS4class{MonteCarloSampling}}}, and the immediate
  superclass \dQuote{\code{\linkS4class{crudeMonteCarlo}}}, the
  following slots are represented (please see the superclasses for
  shared slot definitions)\ldots
  
  \describe{
	
    \item{\code{proxy}:}{Object of class \code{"character"}: The name of
      the proxy function used. For built-in proxies, choose one of
      \dQuote{\code{gvProxy}} or \dQuote{\code{wbProxy}}. These are
      explained in detail in the vignette cited below.}

  }
}

\section{Extends}{
Class \code{"\linkS4class{crudeMonteCarlo}"}, directly. \cr
Class \code{"\linkS4class{MonteCarloSampling}"}, by class "crudeMonteCarlo", distance 2.
}

\section{Methods}{
No new methods defined with class "importanceSampling" in the
signature. However, various methods such as \code{summary} and \code{plot} are
available through inheritance.
}

\references{
\describe{
%
\item{}{Gove, J. H. 2013. Monte Carlo sampling methods in
  \pkg{sampSurf}. Package vignette.}
}
}

\author{
  Jeffrey H. Gove %, \email{jhgove@unh.edu}
}

\seealso{
  \code{\linkS4class{MonteCarloSampling}},
  \code{\linkS4class{crudeMonteCarlo}},
  \code{\linkS4class{controlVariate}},
  \code{\linkS4class{antitheticSampling}}. 
}
\examples{
showClass("importanceSampling")
}
\keyword{classes}
