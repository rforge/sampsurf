\name{izGridMirage-methods}
\docType{methods}
\alias{izGridMirage-methods}
\alias{izGridMirage,InclusionZone,mirageTract-method}

\title{ Methods for Function \code{izGridMirage} in Package
  \pkg{sampSurf} }

\description{
 There is one method for generic function \code{\link{izGridMirage}} that will
 create objects of class "\code{\linkS4class{mirageInclusionZoneGrid}}".
}

\section{Methods}{
  
\describe{

\item{\strong{\code{signature(izObject = "InclusionZone", tract =
	  "mirageTract")}}}{

  Any arguments not defined below have the same definitions as those for
  the \code{\link{izGrid}} constructor (which is called first for the
  given \code{izObject}).
}
  
  \item{}{\bold{usage\ldots}
    \preformatted{izGridMirage(izObject,
             tract,
             description = 'izGrid object using mirage',
             wholeIZ = TRUE, 
             truncateOverlap = FALSE, 
             ... )}
	 
	\itemize{
		 
	  \item{\code{izObject}:}{ An object of class "InclusionZone"} 
		 
	  \item{\code{tract}:}{ An object of class "mirageTract".}
		 
	  \item{\code{description}:}{ A description of the object as a
	    character string.}
		 
	  \item{\code{truncateOverlap}:}{ \code{TRUE}: truncate the
	  estimates to zero for cells external to the tract; \code{FALSE}:
	  keep the estimates. This only applies to the object in the
	  \code{izGrid.extended} slot of the object. If \code{TRUE}, the
	  extended grid will still be there, only with background cells
	  rather than estimates. If \code{FALSE}, the estimates for the
	  cells in the extended grid will be preserved.}
	  
	  \item{... :}{ Other arguments to be passed along as described in
        the \code{\link{izGrid-methods}} constructor documentation.}	  
	} %itemize
 } %item


	  
}} %describe, section
\keyword{methods}
\keyword{ ~~ other possible keyword(s) ~~ }
