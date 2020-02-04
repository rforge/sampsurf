\name{antitheticSampling-methods}
\docType{methods}
\alias{antitheticSampling-methods}
\alias{antitheticSampling,mcsContainer-method}
\alias{antitheticSampling,MonteCarloSampling-method}

\title{ Methods for \code{antitheticSampling} object construction in Package
  \pkg{sampSurf} }

\description{
 The methods below allow for the construction of individual objects of
 class "\code{\linkS4class{antitheticSampling}}" when applied to individual
 "\code{\linkS4class{MonteCarloSampling}}" subclass objects. Alternatively, the second
 constructor can be applied to a collection of
 "\code{\linkS4class{MonteCarloSampling}}" subclass objects and will return an
 appropriate collection of "\code{\linkS4class{antitheticSampling}}"
 objects (see, e.g., the "\code{\linkS4class{antitheticContainer}}" class).
}

\section{Methods}{
  
\describe{


\item{\strong{\code{signature(object = "MonteCarloSampling")}}}{}

  \item{}{\bold{usage\ldots}
    \preformatted{antitheticSampling(object,
                   alphaLevel = 0.05,
                   description = 'Antithetic Sampling',
                   ... )}
	   
    \itemize{
			
	  \item{\code{object}:}{ An object that is a subclass of
	  "\code{MonteCarloSampling}". }
			
	  \item{\code{alphaLevel}:}{ The two-tailed alpha-level for
        confidence interval construction.}
		
	  \item{\code{description}:}{ A character vector description of the
		object.}
		
    } %itemize
  } %item








%------------------------------------------------------------
\item{\strong{\code{signature(object = "mcsContainer")}}}{

  See the above definitions for the first method for arguments not shown below.

}

  \item{}{\bold{usage\ldots}
    \preformatted{antitheticSampling(object,
                   alphaLevel = 0.05,
                   description = 'Antithetic Sampling collection',
                   ... )}
	   
    \itemize{
			
	  \item{\code{object}:}{ A collection of \code{"MonteCarloSampling"} subclass objects
	    in a valid "\code{\linkS4class{mcsContainer}}" object.}
		
    } %itemize
  } %item


} %describe
} %section
\keyword{methods}
%\keyword{ ~~ other possible keyword(s) ~~ }
