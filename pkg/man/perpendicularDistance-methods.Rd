\name{perpendicularDistance-methods}
\docType{methods}
\alias{perpendicularDistance-methods}
\alias{perpendicularDistance,numeric-method}

\title{ Methods for "perpendicularDistance" Object Construction in Package
  \sQuote{sampSurf}}

\description{ There is currently only one method based on the
\code{\link{perpendicularDistance}} generic that is used for object
construction. It is detailed below.
}

\section{Methods}{
\describe{

  \item{\strong{\code{signature(kpds = "numeric")}}}{

	This method takes the \emph{Kpds} factor in appropriate units as the
    signature argument along with other optional aruments described as
    follows\ldots }
  
  \item{}{\bold{usage\ldots}
    \preformatted{perpendicularDistance(kpds,
                      units = 'metric',
                      description = 'perpendicular distance method',
                      ...) }
	   
	\itemize{
	  \item{\code{kpds}:}{ The \emph{Kpds} factor in appropriate units.}

	  \item{\code{units}:}{ Either \dQuote{English} or \dQuote{metric}.}

	  \item{\code{description}:}{ A character vector description of the
		object.}  
	
	} %itemize
  } %item
  
}}
\keyword{methods}
%\keyword{ ~~ other possible keyword(s) ~~ }
