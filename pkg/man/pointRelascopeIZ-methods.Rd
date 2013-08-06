\name{pointRelascopeIZ-methods}
\docType{methods}
\alias{pointRelascopeIZ-methods}
\alias{pointRelascopeIZ,downLog,pointRelascope-method}

\title{ Method for "pointRelascopeIZ" Object Construction in Package
  \pkg{sampSurf} }

\description{
 This is the one method for generic function \code{\link{pointRelascopeIZ}} in
 Package \sQuote{sampSurf} that allows for creation of objects of class
 "\code{\linkS4class{pointRelascopeIZ}}."
}

\section{Methods}{ 
\describe{

\item{\strong{\code{signature(downLog = "downLog", prs =
"pointRelascope")}}}{}
  
  \item{}{\bold{usage\ldots}
    \preformatted{pointRelascopeIZ(downLog,
                 prs,
                 nptsCircle = 100,          
                 description = 'inclusion zone for down log point relascope sampling method',
                 spID = paste('prsIZ',format(runif(1,0,10000),digits=8),sep=':'),
                 spUnits = CRS(projargs=as.character(NA)),
                 ... )}
	   
    \itemize{
				
	  \item{\code{downLog}:}{ An object of class
        "\code{\linkS4class{downLog}}" for which the inclusion zone is to be
        determined.}
		
	  \item{\code{prs}:}{ An object of class
	    "\code{\linkS4class{pointRelascope}}" that will contain the
	    correct relascope angle, etc., for constructing the inclusion
	    zone.}
		
	  \item{\code{nptsCircle}:}{ The number of points to use in the
	    construction of each of the dual circles comprising the
	    inclusion area. }
		
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
