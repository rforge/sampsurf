\name{distanceLimited-methods}
\docType{methods}
\alias{distanceLimited-methods}
\alias{distanceLimited,numeric-method}

\title{  Methods for \dQuote{distanceLimited} Object Construction in Package
  \sQuote{sampSurf}
}

\description{ There is currently only one method based on the
\code{\link{distanceLimited}} generic that is used for object
construction. It is detailed below.
}

\section{Methods}{
\describe{

\item{\strong{\code{signature(distanceLimit = "numeric")}}}{

	This method takes distance limit in appropriate units as the
    signature argument along with other optional aruments described as
    follows\ldots }
  
  \item{}{\bold{usage\ldots}
    \preformatted{function(distanceLimit,
         units = 'metric',
         description = 'distance limited method',
         ...) }
	   
	\itemize{
	  \item{\code{distanceLimit}:}{ The distance limit in appropriate
	    units (feet or meters).}

	  \item{\code{units}:}{ Either \dQuote{English} or \dQuote{metric}.}

	  \item{\code{description}:}{ A character vector description of the
		object.}  
	
	} %itemize
  } %item
 
}}

\keyword{methods}
\keyword{ ~~ other possible keyword(s) ~~ }
