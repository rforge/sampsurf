\name{fullChainSawIZ-methods}
\docType{methods}
\alias{fullChainSawIZ-methods}
\alias{fullChainSawIZ,downLog,numeric-method}

\title{ Methods for  \dQuote{\code{fullChainSawIZ}} object construction in Package
  \pkg{sampSurf} }

\description{
  The one constructor method that is available for creating valid
  objects of class "\code{\linkS4class{fullChainSawIZ}}" is
  documented below.  Note that because the inclusion zone for
  this method is exactly the same as that for the
  "\code{\linkS4class{sausageIZ}}" class (sausage sampling), the same
  arguments are used here as in that constructor.
}

\section{Methods}{
  
\describe{

\item{\strong{\code{signature(downLog = "downLog", plotRadius = "numeric")}}}{}


  \item{}{\bold{usage\ldots}
    \preformatted{fullChainSawIZ(downLog,
               plotRadius,
               nptsHalfCircle = 50,
               description = 'Inclusion zone for "fullChainSaw" sampling method',
               spID = paste('fcs', .StemEnv$randomID(), sep=':'),
               spUnits = CRS(projargs = as.character(NA)),
               ... )}
	   
    \itemize{

	  \item{\code{downLog}:}{ An object of class
        "\code{\linkS4class{downLog}}" which the inclusion zone is to be
        determined for under this method.}
		
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
			


	  
}} %describe-methods
\keyword{methods}
%\keyword{ ~~ other possible keyword(s) ~~ }
