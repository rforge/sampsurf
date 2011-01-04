\name{standUpIZ-methods}
\docType{methods}
\alias{standUpIZ-methods}
\alias{standUpIZ,downLog,numeric-method}
\title{ Method for "standUpIZ" object construction in Package
  `sampSurf'}

\description{
 This is the one method for generic function \code{\link{standUpIZ}} in
 Package `sampSurf' that allows for creation of objects of class
 "\code{\linkS4class{standUpIZ}}."
}


\section{Methods}{
\describe{

\item{\strong{\code{signature(downLog = "downLog", plotRadius =
	  "numeric")}}}{} 
  
  \item{}{\bold{usage\ldots}
    \preformatted{function(downLog,
         plotRadius,
         description = 'inclusion zone for "standup" method',
         spID = unlist(strsplit(tempfile('cp:',''),'\\/'))[2],
         spUnits = CRS(projargs=as.character(NA)),
         ... )}
    \itemize{
		
	  \item{\code{downLog}:}{ An object of class
        "\code{\linkS4class{downLog}}" which the inclusion zone is to be
        determined for under the stand-up method.}
		
	  \item{\code{plotRadius}:}{ The radius of the circular fixed-area
	    plot in the correct units: feet for "English" and meters for
	    "metric."}
		
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
\keyword{ ~~ other possible keyword(s) ~~ }
