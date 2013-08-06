\name{circularPlotIZ-methods}
\docType{methods}
\alias{circularPlotIZ-methods}
\alias{circularPlotIZ,standingTree,numeric-method}

\title{ Method for "circularPlotIZ" object construction in Package
  \pkg{sampSurf} }

\description{
 This is the one method for generic function \code{\link{circularPlotIZ}} in
 Package \sQuote{sampSurf} that allows for creation of objects of class
 "\code{\linkS4class{circularPlotIZ}}."
}

\section{Methods}{
\describe{

\item{\strong{\code{signature(standingTree = "standingTree", plotRadius = "numeric")}}}{}

  
  \item{}{\bold{usage\ldots}
    \preformatted{circularPlotIZ(standingTree,
               plotRadius,
               description = 'inclusion zone for circular plot method',
               spID = paste('cp',.StemEnv$randomID(),sep=':'),
               spUnits = CRS(projargs=as.character(NA)),
               ... )}
			 
    \itemize{
				
	  \item{\code{standingTree}:}{ An object of class
        "\code{\linkS4class{standingTree}}" for which the inclusion zone is to be
        determined.}
		
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
