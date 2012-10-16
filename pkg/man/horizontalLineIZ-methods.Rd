\name{horizontalLineIZ-methods}
\docType{methods}
\alias{horizontalLineIZ-methods}
\alias{horizontalLineIZ,standingTree,angleGauge,numeric-method}

\title{ Methods for "\code{horizontalLineIZ}" object constuction in Package
  \pkg{sampSurf} }

\description{
 This is the one method for generic function \code{\link{horizontalLineIZ}} in
 Package \pkg{sampSurf} that allows for creation of objects of class
 "\code{\linkS4class{horizontalLineIZ}}."
}

\section{Methods}{
  
\describe{

\item{\strong{\code{signature(standingTree = "standingTree", angleGauge
	  = "angleGauge", lineLength = "numeric")}}}{}

  Note that an object of class "\code{\linkS4class{lineSegment}}" will
  be constructed from the values passed in the \code{lineLength} and
  \code{orientation} arguments\ldots

  \item{}{\bold{usage\ldots}
    \preformatted{function(standingTree,
         angleGauge,
         lineLength,
         orientation = 0, 
         description = 'inclusion zone for horizontal line sampling method',
         spID = paste('hls',.StemEnv$randomID(),sep=':'),
         spUnits = CRS(projargs=as.character(NA)),
         ... ) }
	   
    \itemize{
				
	  \item{\code{standingTree}:}{ An object of class
        "\code{\linkS4class{standingTree}}" for which the inclusion zone is to be
        determined.}
		
	  \item{\code{angleGauge}:}{ An object of class
	  "\code{\linkS4class{angleGauge}}". }
		
	  \item{\code{lineLength}:}{ The length of the line segment in the
	     appropriate units (feet for \dQuote{English}, meters for
	     \dQuote{metric}). }
		
	  \item{\code{orientation}:}{  The line orientation clockwise from \emph{north} as an
	     azimuth in degrees.}
		   
	  \item{\code{description}:}{ A character vector description of the
		object.}
		
	  \item{\code{spID}:}{ A unique identifier that will be used in
	    displaying the spatial polygon for the inclusion zone component
	    of the object.}

	  \item{\code{spUnits}:}{ A valid \code{\link{CRS}} object
        specifying the Coordinate Reference System. This defaults to
        \code{NA}, which means you want to use your own user-defined
        system, say for a sample plot located in the field.}

	  \item{\code{dots}:}{ Arguments to be passed on. }
		
    } %itemize
  } %item

}}

\keyword{methods}
\keyword{ ~~ other possible keyword(s) ~~ }
