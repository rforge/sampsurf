\name{lineSegment-methods}
\docType{methods}
\alias{lineSegment-methods}
\alias{lineSegment,numeric,numeric-method}

\title{ Methods for "\code{\linkS4class{lineSegment}}" object creation in Package
  \pkg{sampSurf} }

\description{There is currently only one method based on the
\code{\link{lineSegment}} generic that is used for object
construction. It is detailed below.
}

\section{Methods}{
  
\describe{

  \item{\strong{\code{signature(length = "numeric", orientation = "numeric")}}}{%\cr

	This method takes the segment length and orientation as the
    signature arguments along with other optional aruments described as
    follows\ldots }
  
  \item{}{\bold{usage\ldots}
    \preformatted{function(length,
         orientation,
         units = 'metric',
         spUnits = CRS(projargs=as.character(NA)),
         centerPoint = c(x=0, y=0),  
         description = 'line segment',
         spID = paste('ls',.StemEnv$randomID(),sep=':'),
         ...) }
	   
	\itemize{
		 
	  \item{\code{length}:}{ The length of the line segment in the
	        appropriate units (feet for \dQuote{English} or meters for
	        \dQuote{metric}). }
		 
	  \item{\code{orientation}:}{  The line orientation from \emph{north} as an
	    azimuth in degrees; e.g. \code{orientation=45}. }
			
	  \item{\code{units}:}{ Either \dQuote{English} or \dQuote{metric}. These
	    must be conformable with the projection in \code{spUnits}.}

	  \item{\code{spUnits}:}{ A valid \code{\link{CRS}} object
        specifying the Coordinate Reference System. This defaults to
        \code{NA}, which means you want to use your own user-defined
        system, say for a sample line located in the field.}

	  \item{\code{centerPoint}:}{ The location of the center of the
	    line segment in the appropriate spatial units. This should be
	    a numeric vector of length 2 with names "x" and "y".}

	  \item{\code{description}:}{ A character vector description of the
		tract.}
	  
	  \item{\code{spID}:}{ Each object should have its own \emph{unique}
        identifier that is used in constructing the \code{\link{Lines}}
        object for the segment. This becomes very important when
        combining individual line segments into a population. If nothing
        is supplied, a random ID is generated.}

	  
	} %itemize
  } %item
}}

\keyword{methods}
