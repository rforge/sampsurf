\name{pointRelascope-methods}
\docType{methods}
\alias{pointRelascope-methods}
\alias{pointRelascope,numeric-method}

\title{ Methods for "pointRelascope" Object Construction in Package
  \sQuote{sampSurf}}

\description{ There is currently only one method based on the
\code{\link{pointRelascope}} generic that is used for object
construction. It is detailed below.
}

\section{Methods}{
  
\describe{

  \item{\strong{\code{signature(angleDegrees = "numeric")}}}{

	This method takes the relascope angle in degrees as the signature
    argument along with other optional aruments described as
    follows\ldots }
  
  \item{}{\bold{usage\ldots}
    \preformatted{function(angleDegrees,
         units = 'metric',
         description = 'point relascope method',
         ...) }
	   
	   
	\itemize{
	  \item{\code{angleDegrees}:}{ The relascope angle in degrees.}

	  \item{\code{units}:}{ Either \dQuote{English} or \dQuote{metric}.}

	  \item{\code{description}:}{ A character vector description of the
		object.}
	
	} %itemize
  } %item

}}

\keyword{methods}
\keyword{ ~~ other possible keyword(s) ~~ }
