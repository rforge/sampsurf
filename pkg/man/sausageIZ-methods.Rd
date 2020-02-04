\name{sausageIZ-methods}
\docType{methods}
\alias{sausageIZ-methods}
\alias{sausageIZ,downLog,numeric-method}

\title{ Method for "sausageIZ" object construction in Package
  `sampSurf'}

\description{
 This is the one method for generic function \code{\link{sausageIZ}} in
 Package \sQuote{sampSurf} that allows for creation of objects of class
 "\code{\linkS4class{sausageIZ}}."
}

\section{Methods}{
\describe{

\item{\strong{\code{signature(downLog = "downLog", plotRadius =
  "numeric")}}}{}
  
  \item{}{\bold{usage\ldots}
    \preformatted{sausageIZ(downLog,
          plotRadius,
          nptsHalfCircle = 50,         
          description = 'inclusion zone for dowed log "sausage" sampling method',
          spID = unlist(strsplit(tempfile('sausageIZ:',''),'\\/'))[2],
          spUnits = CRS(projargs=as.character(NA)),
          ... )}
		 
    \itemize{
				
	  \item{\code{downLog}:}{ An object of class
        "\code{\linkS4class{downLog}}" which the inclusion zone is to be
        determined for under the sausage method.}
		
	  \item{\code{plotRadius}:}{ The radius of the circular fixed-area
	    plot in the correct units: feet for "English" and meters for
	    "metric."}
		
	  \item{\code{nptsHalfCircle}:}{ The number of points to use in the
	  half-circle at each end of the sausage-shaped inclusion zone for
	  the polygon representation of the object. }
		
	  \item{\code{description}:}{ A character vector description of the
		object.}
		
	  \item{\code{spID}:}{ A unique identifier that will be used in
	    displaying the spatial polygon component of the object.}

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
