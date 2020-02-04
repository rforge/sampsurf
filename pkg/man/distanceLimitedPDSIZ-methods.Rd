\name{distanceLimitedPDSIZ-methods}
\docType{methods}
\alias{distanceLimitedPDSIZ-methods}
\alias{distanceLimitedPDSIZ,downLog,perpendicularDistance,dlsNumeric-method}

\title{ Methods for "\code{distanceLimitedPDSIZ}" Object Construction in
  Package \pkg{sampSurf}}

\description{
 This is the one method for generic function \code{\link{distanceLimitedPDSIZ}} in
 Package \sQuote{sampSurf} that allows for creation of objects of class
 "\code{\linkS4class{distanceLimitedPDSIZ}}."
}

\section{Methods}{
\describe{

\item{\strong{\code{signature(downLog = "downLog", pds = "perpendicularDistance", dls = "dlsNumeric")}}}{}
  
  \item{}{\bold{usage\ldots}
    \preformatted{distanceLimitedPDSIZ(downLog,
                     pds,
                     dls,
                     description = 'inclusion zone for down log distance limited PDS',
                     spID = paste('dlpds',.StemEnv$randomID(),sep=':'),
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
		
	  \item{\code{dls}:}{ An object of class
	    "\code{\linkS4class{distanceLimited}}" that supplies the
	    information on the distance limited method for constructing the
	    inclusion zone. Alternatively a "numeric" distance limit that
	    will be converted to a "distanceLimited" object. }
		
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
%\keyword{ ~~ other possible keyword(s) ~~ }
