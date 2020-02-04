\name{criticalHeightIZ-methods}
\docType{methods}
\alias{criticalHeightIZ-methods}
\alias{criticalHeightIZ,standingTree,angleGauge-method}

\title{ Methods for "\code{criticalHeightIZ}" object constuction in Package
  \pkg{sampSurf} }

\description{
 This documents the one method for generic function \code{\link{criticalHeightIZ}} in
 Package \pkg{sampSurf} that allows for creation of objects of class
 "\code{\linkS4class{criticalHeightIZ}}."
}

\section{Methods}{
  
\describe{

\item{\strong{\code{signature(standingTree = "standingTree", angleGauge = "angleGauge")}}}{}


  \item{}{\bold{usage\ldots}
    \preformatted{criticalHeightIZ(standingTree,
                 angleGauge,
                 referenceHeight = .StemEnv$referenceCHSIZ,
                 description = 'inclusion zone for critical height sampling method',
                 spID = paste('chs',.StemEnv$randomID(),sep=':'),
                 spUnits = CRS(projargs=as.character(NA)),
                 ... ) }
	   
    \itemize{
			
	  \item{\code{standingTree}:}{ An object of class
        "\code{\linkS4class{standingTree}}" for which the inclusion zone is to be
        determined.}
		
	  \item{\code{angleGauge}:}{ An object of class
	  "\code{\linkS4class{angleGauge}}". }
		
	  \item{\code{referenceHeight}:}{ The height on the stem at which
	  the inclusion zone is to be determined. Currently the choices are
	  \dQuote{butt} (default) or \dQuote{dbh}. These are found in the
	  argument assignment \code{.StemEnv$referenceCHSIZ} above. }
		   
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
%\keyword{ ~~ other possible keyword(s) ~~ }
