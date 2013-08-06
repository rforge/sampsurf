\name{MonteCarloSamplingIZ-class}
\Rdversion{1.1}
\docType{class}
\alias{MonteCarloSamplingIZ-class}

\title{Class \code{"MonteCarloSamplingIZ"}}

\description{ This virtual class exists to be combined with an areal
  sampling class of choice. This class facilitates creation of a new
  areal sampling method that employs one of the Monte Carlo subsampling
  methods supported through subclasses of
  \code{"MonteCarloSampling"}. Generally it will only be of interest to
  someone desiring to write extensions to \pkg{sampSurf} in the form of
  Monte Carlo subsampling on down logs or standing trees within areal
  methods. Please see the class definition for
  \code{\linkS4class{horizontalPointCMCIZ}} for an example of how this
  can be combined with an areal method.}

\section{Objects from the Class}{A virtual Class: No objects may be
  created from it.}

\section{Slots}{
  \describe{
	
    \item{\code{mcsObj}:}{Object of class \code{"MonteCarloSampling"} A
      subclass objects of \code{\linkS4class{MonteCarloSampling}};
      please see that class for extant subclasses/sampling methods. }
  
    \item{\code{antithetic}:}{Object of class \code{"logical"}
      \code{TRUE}: if antithetic sampling variant has been used for the
      object in the \code{mcsObj} slot; \code{FALSE}: if not. }
	
    \item{\code{proxy}:}{Object of class \code{"character"} The named of
	  the proxy function used in Monte Carlo sampling.}
	
  }
}

\section{Methods}{
No methods defined with class "MonteCarloSamplingIZ" in the signature.
}

\author{
  Jeffrey H. Gove %, \email{jhgove@unh.edu}
}

\seealso{
  \code{\linkS4class{MonteCarloSampling}}, \code{\linkS4class{horizontalPointCMCIZ}}
}

\examples{
showClass("MonteCarloSamplingIZ")
}
\keyword{classes}
