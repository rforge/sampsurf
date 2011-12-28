\name{angleGauge-methods}
\docType{methods}
\alias{angleGauge-methods}
\alias{angleGauge,numeric-method}

\title{ Methods \code{"angleGauge"}  Object Construction in Package
  \pkg{sampSurf} }

\description{ There is currently only one method based on the
\code{\link{angleGauge}} generic that is used for object
construction. It is detailed below.
}

\section{Methods}{
  \describe{
  \item{\strong{\code{signature(baf = "numeric")}}}{

	This method takes the basal area factor as the key argument\ldots}
  
  \item{}{\bold{usage\ldots}
    \preformatted{function(baf,
         units = 'metric',
         description = 'angle gauge method',
         ...) }
	   
	\itemize{
	  \item{\code{baf}:}{ The basal area factor.}

	  \item{\code{units}:}{ Either \dQuote{English} or \dQuote{metric}.}

	  \item{\code{description}:}{ A character vector description of the
		object.}
	
	} %itemize
  } %item

}}

\keyword{methods}
\keyword{ ~~ other possible keyword(s) ~~ }
