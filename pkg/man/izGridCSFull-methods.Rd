\name{izGridCSFull-methods}
\docType{methods}
\alias{izGridCSFull-methods}
\alias{izGridCSFull,InclusionZoneGrid,Tract-method}

\title{ Methods for "csFullInclusionZoneGrid" object construction in Package \pkg{sampSurf} }

\description{ The following method will construct valid objects of class
  "\code{\linkS4class{csFullInclusionZoneGrid}}" for the full-log
  sausage-shaped inclusion zone. Please see \emph{"The InclusionZoneGrid
  Class"} vignette for more details and examples.  }

\section{Methods}{
\describe{

\item{\strong{\code{signature(izGridSausage = "InclusionZoneGrid", tract
	  = "Tract")}}}{} 
   
  \item{}{\bold{usage\ldots}
    \preformatted{izGridCSFull(izGridSausage,
             tract,
             description = 'Full chainSaw-sausage inclusion zone grid object',
             runQuiet = FALSE,
             ... )}
		   
	\itemize{
		 
	  \item{\code{izGridSausage}:}{ An object of class
	    "InclusionZoneGrid" whose \code{iz} slot contains an object of
	    class "\code{\linkS4class{sausageIZ}}". This object will be
	    used to delineate the grid cells where the chainsaw method will
	    be applied.  }
		 
	  \item{\code{tract}:}{ An object of class "Tract" or subclass.}
		 
	  \item{\code{description}:}{ A description of the object as a
	    character string.}
		 
	  \item{\code{runQuiet}:}{ This routine will print a report of the
	  grid cells as it visits them; set this to \code{TRUE}:
	  (default) to see the report; \code{FALSE}: to run quietly, which
	  is useful for larger simulations.}
	  
	  \item{... :}{ Other arguments to be passed along--not used at present.}
	} %itemize
 } %item
	  
}}

\keyword{methods}
\keyword{ ~~ other possible keyword(s) ~~ }
