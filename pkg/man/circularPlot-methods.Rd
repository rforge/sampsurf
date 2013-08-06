\name{circularPlot-methods}
\docType{methods}
\alias{circularPlot-methods}
\alias{circularPlot,numeric-method}
\title{Methods for "circularPlot" Object Construction in package
  \sQuote{sampSurf}}

\description{ There is currently only one method based on the
\code{\link{circularPlot}} generic that is used for object
construction. It is detailed below. }

\section{Methods}{
  
\describe{

  \item{\strong{\code{signature(radius = "numeric")}}}{%\cr

	This method takes the plot radius as the signature argument along
    with other optional aruments described as follows\ldots }
  
  \item{}{\bold{usage\ldots}
    \preformatted{circularPlot(radius,
             units = 'metric',
             spUnits = CRS(projargs=as.character(NA)),
             centerPoint = c(x=0, y=0),  
             description = 'fixed area circular plot',
             nptsPerimeter = 100,
             spID = unlist(strsplit(tempfile('cp:',''),'\\/'))[2],
             ...) }
	   
	\itemize{
	  \item{\code{radius}:}{ The fixed-plot
      radius in the correct units. For "English" units, this would be
      feet, for "metric" units, meters.}

	  \item{\code{units}:}{ Either \dQuote{English} or \dQuote{metric}. These
	    must be conformable with the projection in \code{spUnits}.}

	  \item{\code{spUnits}:}{ A valid \code{\link{CRS}} object
        specifying the Coordinate Reference System. This defaults to
        \code{NA}, which means you want to use your own user-defined
        system, say for a sample plot located in the field.}

	  \item{\code{centerPoint}:}{ The location of the center of the
	    fixed-area plot in the appropriate spatial units. This should be
	    a numeric vector of length 2 with names "x" and "y".}

	  \item{\code{description}:}{ A character vector description of the
		tract.}

	  \item{\code{nptsPerimeter}:}{ The number of points that will
		compose the spatial perimeter of the object. Note that the final
		result will always be one more point than requested in order to
		close the polygon. }
	  
	  \item{\code{spID}:}{ Each object should have its own \emph{unique}
        identifier that is used in constructing the \code{\link{Polygons}}
        object for the perimeter. This becomes very important when combining
        individual plots into a population. If nothing is supplied, a
        random ID is generated.}
	  
	} %itemize
  } %item
}}


\keyword{methods}
\keyword{ ~~ other possible keyword(s) ~~ }
