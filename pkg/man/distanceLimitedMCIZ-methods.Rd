\name{distanceLimitedMCIZ-methods}
\docType{methods}
\alias{distanceLimitedMCIZ-methods}
\alias{distanceLimitedMCIZ,downLog,distanceLimited-method}

\title{ Methods for "\code{distanceLimitedMCIZ}" Object
  Construction in Package \pkg{sampSurf}}

\description{
 This is the one method for generic function \code{\link{distanceLimitedMCIZ}} in
 Package \sQuote{sampSurf} that allows for creation of objects of class
 "\code{\linkS4class{distanceLimitedMCIZ}}."
}

\section{Methods}{
  
\describe{

\item{\strong{\code{signature(downLog = "downLog", dls = "distanceLimited")}}}{}
  
  \item{}{\bold{usage\ldots}
    \preformatted{function(downLog,
         dls,
         description = 'inclusion zone for down log DLMC sampling',
         spID = paste('dlmc',.StemEnv$randomID(),sep=':'),
         spUnits = CRS(projargs=as.character(NA)),
        ... )}
	   
    \itemize{
				
	  \item{\code{downLog}:}{ An object of class
        "\code{\linkS4class{downLog}}" for which the inclusion zone is to be
        determined.}
		
	  \item{\code{dls}:}{ An object of class
	    "\code{\linkS4class{distanceLimited}}" that supplies the
	    information on the distance limited method for
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

	  \item{\code{dots}:}{ Arguments to be passed on. }
	  
    } %itemize
  } %item		
				


}}

\keyword{methods}
\keyword{ ~~ other possible keyword(s) ~~ }
