\name{perpendicularDistanceIZ-methods}
\docType{methods}
\alias{perpendicularDistanceIZ-methods}
\alias{perpendicularDistanceIZ,downLog,perpendicularDistance-method}

\title{ Methods for "\code{perpendicularDistanceIZ}" Object
  Construction in Package \pkg{sampSurf}}

\description{
 This is the one method for generic function \code{\link{perpendicularDistanceIZ}} in
 Package \sQuote{sampSurf} that allows for creation of objects of class
 "\code{\linkS4class{perpendicularDistanceIZ}}."
}

\section{Methods}{
\describe{

\item{\strong{\code{signature(downLog = "downLog", pds = "perpendicularDistance")}}}{}
  
  \item{}{\bold{usage\ldots}
    \preformatted{function(downLog,
         pds,
         description = 'inclusion zone for down log perpendicular distance sampling',
         spID = paste('pds',.StemEnv$randomID(),sep=':'),
         spUnits = CRS(projargs=as.character(NA)),
         pdsType = .StemEnv$pdsTypes,
         ... )}
	   
    \itemize{
				
	  \item{\code{downLog}:}{ An object of class
        "\code{\linkS4class{downLog}}" for which the inclusion zone is to be
        determined.}
		
	  \item{\code{pds}:}{ An object of class
	    "\code{\linkS4class{perpendicularDistance}}" that supplies the
	    information on the perpendicular distance method for
	    constructing the inclusion zone. }
		
	  \item{\code{description}:}{ A character vector description of the
		object.}
		
	  \item{\code{spID}:}{ A unique identifier that will be used in
	    displaying the spatial polygon for the inclusion zone component
	    of the object.}

	  \item{\code{spUnits}:}{ A valid \code{\link{CRS}} object
        specifying the Coordinate Reference System. This defaults to
        \code{NA}, which means you want to use your own user-defined
        system, say for a sample plot located in the field.}

      \item{\code{pdsType}:}{ A character string that specifies
        the type of perpendicular distance sampling used with regard to
        the selection (i.e., probability proportional to\ldots) of the
        log. It can be one of "\code{volume}", "\code{surfaceArea}" or
        "\code{coverageArea}". (See also \code{.StemEnv$pdsTypes} for
        legal types.)  }

	  \item{\code{dots}:}{ Arguments to be passed on. }
	  
    } %itemize
  } %item		


}}

\keyword{methods}
\keyword{ ~~ other possible keyword(s) ~~ }
