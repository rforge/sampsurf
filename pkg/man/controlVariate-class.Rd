\name{controlVariate-class}
\Rdversion{1.1}
\docType{class}
\alias{controlVariate-class}

\title{Class \code{"controlVariate"}}

\description{
   This is the class definition that allows for the
  application of control variate sampling to \code{downLog} or
  \code{standingTree} objects. Examples of the class usage can be found
  in the Monte Carlo sampling vignette referenced below.
}

\section{
  Objects from the Class}{ Objects can be created by calls of the
form \code{new("controlVariate", ...)}.  However, an object constructor
of the same name, \code{\link{controlVariate}}, has been provided
and is the preferred method for creating objects that are ensured to be
valid.  }

\section{Slots}{

  Please note that diameters below are presumed to be in the
  \emph{same} units as length, i.e., meters for \dQuote{metric}, and
  feet for \dQuote{English} \code{units}. Cross-sectional areas are in
  compatible units.

  This class has three superclasses that define a number of other slots
  in addition to the what follows. Please see these classes for the
  complete set of slot definitions for the "\code{controlVariate}"
  class\ldots 

  \describe{
	
    \item{\code{diff.s}:}{Object of class \code{"numeric"}: This vector
      holds the \code{n.s} differences between measured and proxy
      cross-sectional areas at the different sample points along the
      bole section. }

  }
}

\section{Extends}{
Class \code{"\linkS4class{importanceSampling}"}, directly.\cr
Class \code{"\linkS4class{crudeMonteCarlo}"}, by class "importanceSampling", distance 2.\cr
Class \code{"\linkS4class{MonteCarloSampling}"}, by class "importanceSampling", distance 3.
}

\section{Methods}{
No new methods defined with class "controlVariate" in the
signature. However, various methods such as \code{summary} and \code{plot} are
available through inheritance.  }

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
  \code{\linkS4class{importanceSampling}},
  \code{\linkS4class{antitheticSampling}}. 
}

\examples{
showClass("controlVariate")
}
\keyword{classes}
