\name{MonteCarloSampling-class}
\Rdversion{1.1}
\docType{class}
\alias{MonteCarloSampling-class}
%\alias{antitheticSampling,MonteCarloSampling-method}
\alias{show,MonteCarloSampling-method}
\alias{summary,MonteCarloSampling-method}

\title{Class \code{"MonteCarloSampling"}}

\description{
  This is a virtual base class for apply Monte Carlo subsampling methods
  within a \dQuote{downLog} or \dQuote{standingTree} object. See below
  for subclasses based on the supported sampling methods.
}

\section{Objects from the Class}{A virtual Class: No objects may be
  created from it.}

\section{Slots}{
  \describe{
	
    \item{\code{stem}:}{Object of class \code{"Stem"}: This can be
      either a \code{\linkS4class{downLog}} or
      \code{\linkS4class{standingTree} subclass object. } }
	
    \item{\code{segBnds}:}{Object of class \code{"numeric"}: A vector of
      length two giving the lower and upper height/length bounds for
      volume estimation within the bole. All of the following slot
      definitions below are relative to the segment of the bole defined
      by these bounds. These bounds correspond to the limits of
      integration along the bole.  }
	
    \item{\code{n.s}:}{Object of class \code{"numeric"}: The number of
      Monte Carlo samples (a scalar). }
	
    \item{\code{startSeed}:}{Object of class \code{"numeric"}: The
      scalar seed for the random number generator used in the call to
      the class constructor. Please see the documentation in
      \code{\link{initRandomSeed}} for possible values and their
      meaning. Suffice it to say that storing this in the object allows
      for object replication. Note that if \code{startSeed = NA}, then
      the seed is not replicable, but the sampling run is by using the
      random numbers in the \code{u.s} slot. }
	
    \item{\code{u.s}:}{Object of class \code{"numeric"}: The uniform
      random numbers used in selecting the sampling points along
      the bole.}
	
    \item{\code{description}:}{Object of class \code{"character"}: A
       description of the object if desired (defaults are given for each
       class). }
	
    \item{\code{userArgs}:}{Object of class \code{"list"}: Some proxy
      functions have extra arguments that are required when called from
      the constructor methods. This slot stores these arguments and
      their values from the call. This is necessary, e.g., for
      re-applying a given Monte Carlo method to the \code{(1-u.s)} points in
      antithetic sampling. }

  } %describe
}  %section

	

\section{Methods}{
  \describe{
    \item{antitheticSampling}{\code{signature(object =
		"MonteCarloSampling")}: Allows for antithetic sampling given a
        subclass object. }
	
    \item{show}{\code{signature(object = "MonteCarloSampling")}: For
      printing the subclass object. }
	
    \item{summary}{\code{signature(object = "MonteCarloSampling")}: A
	  printed summary of the subclass object. }
	 }
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
  The following subclasses and related:
  \code{\linkS4class{crudeMonteCarlo}},
  \code{\linkS4class{importanceSampling}},
  \code{\linkS4class{controlVariate}},
  \code{\linkS4class{antitheticSampling}}. 
}

\examples{
showClass("MonteCarloSampling")
}
\keyword{classes}
