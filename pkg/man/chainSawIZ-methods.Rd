\name{chainSawIZ-methods}
\docType{methods}
\alias{chainSawIZ-methods}
\alias{chainSawIZ,downLog,numeric-method}

\title{ Method for "chainSawIZ" object construction in Package
  \pkg{sampSurf} }

\description{
 This is the one methods for generic function \code{\link{chainSawIZ}} in
 Package `sampSurf' that allows for creation of objects of class
 "\code{\linkS4class{chainSawIZ}}."
}

\section{Methods}{
\describe{

\item{\strong{\code{signature(downLog = "downLog", plotRadius =
	  "numeric")}}}{%\cr

     }%for space
  
  \item{}{\bold{usage\ldots}
    \preformatted{chainSawIZ(downLog,
           plotRadius,
           plotCenter = c(x=0, y=0),
           description = 'inclusion zone for "chainsaw" method',
           spID = unlist(strsplit(tempfile('cs:',''),'\\/'))[2],
           spUnits = CRS(projargs=as.character(NA)),
           ... )}
		  
    \itemize{
				
	  \item{\code{downLog}:}{ An object of class
        "\code{\linkS4class{downLog}}" which the inclusion zone is to be
        determined for under the chainsaw method.}
		
	  \item{\code{plotRadius}:}{ The radius of the circular fixed-area
	    plot in the correct units: feet for "English" and meters for
	    "metric."}
		
	  \item{\code{plotCenter}:}{ The (x,y) positions of the center of
	    the circular plot that intersects the log forming the
	    \dQuote{sliver} that would be cut with a chainsaw at their
	    intersection, and which will be sampled for volume.  This should
	    be a numeric vector of length 2 with names "x" and "y" (e.g.,
	    see \code{\link{circularPlot}}). }
		
	  \item{\code{description}:}{ A character vector description of the
		object.}
		
	  \item{\code{spID}:}{ A unique identifier that will be used in
	    displaying the spatial polygon for the circular plot component
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
%\keyword{ ~~ other possible keyword(s) ~~ }
