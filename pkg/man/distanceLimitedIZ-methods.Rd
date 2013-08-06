\name{distanceLimitedIZ-methods}
\docType{methods}
\alias{distanceLimitedIZ-methods}
\alias{distanceLimitedIZ,downLog,distanceLimited-method}

\title{  Methods for "\code{distanceLimitedIZ}" Object
  Construction in Package \pkg{sampSurf} }

\description{
 This is the one method for generic function \code{\link{distanceLimitedIZ}} in
 Package \sQuote{sampSurf} that allows for creation of objects of class
 "\code{\linkS4class{distanceLimitedIZ}}."
}

\section{Methods}{
  
\describe{

\item{\strong{\code{signature(downLog = "downLog", dls =
	  "distanceLimited")}}}{}

  
  \item{}{\bold{usage\ldots}
    \preformatted{distanceLimitedIZ(downLog,
                  dls,
                  description = 'inclusion zone for down log DL sampling',
                  spID = paste('dl',.StemEnv$randomID(),sep=':'),
                  spUnits = CRS(projargs=as.character(NA)),
                  ... )}
	   
    \itemize{
				
	  \item{\code{downLog}:}{ An object of class
        "\code{\linkS4class{downLog}}" for which the inclusion zone is
        to be determined.}
		
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
