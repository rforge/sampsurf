\name{horizontalPointIZ-methods}
\docType{methods}
\alias{horizontalPointIZ-methods}
\alias{horizontalPointIZ,standingTree,angleGauge-method}

\title{ Methods for \code{"horizontalPointIZ"} object construction in Package
  \pkg{sampSurf}}

\description{
 This is the one method for generic function \code{\link{horizontalPointIZ}} in
 Package \sQuote{sampSurf} that allows for creation of objects of class
 "\code{\linkS4class{horizontalPointIZ}}."
}

\section{Methods}{
\describe{

\item{\strong{\code{signature(standingTree = "standingTree", angleGauge = "angleGauge")}}}{}
  
  \item{}{\bold{usage\ldots}
    \preformatted{function(standingTree,
         angleGauge,
         description = 'inclusion zone for horizontal point sampling method',
         spID = paste('hps',.StemEnv$randomID(),sep=':'),
         spUnits = CRS(projargs=as.character(NA)),
         ...)
        }
    \itemize{
				
	  \item{\code{standingTree}:}{ An object of class
        "\code{\linkS4class{standingTree}}" for which the inclusion zone is to be
        determined.}
		
	  \item{\code{angleGauge}:}{ An object of class
	  "\code{\linkS4class{angleGauge}}". }
		
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
