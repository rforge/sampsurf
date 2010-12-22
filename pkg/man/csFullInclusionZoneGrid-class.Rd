\name{csFullInclusionZoneGrid-class}
\Rdversion{1.1}
\docType{class}
\alias{csFullInclusionZoneGrid-class}

\title{Class "csFullInclusionZoneGrid"}

\description{ This is a class definition for objects that enumerate the
  full set of chainsaw inclusion zones at each grid point within the
  sausage-shaped inclusion zone for the whole log under protocol 1 of
  Gove and Van Deusen (2011). It is a useful method to visualize how the chainsaw
  method generates different estimates at each sample point (grid cell
  center) within the log's zone, by \dQuote{cutting} off a different
  sliver at each point, due to the differing intersection of the
  circluar plot with the log.
}

\section{Objects from the Class}{
Objects can be created by calls of the form
\code{new("csFullInclusionZoneGrid", ...)}. However, this is not recommended
because the objects are fairly complex. Instead, one can use the object
constructor \code{\link{izGridCSFull}} to create new objects. More details are
found in \emph{"The InclusionZoneGrid Class"} vignette.
}
 
\section{Slots}{
  As noted below, this class is a subclass of
  "\code{\linkS4class{InclusionZoneGrid}}"; it adds only one slot to the
  class, all others not defined below are as described in the superclass\ldots
  
  \describe{

	\item{\code{chiz}:}{Object of class \code{"list"}: This is a list
      object containing \code{NA}s for cells outside the inclusion zone,
      but containing the full set of “InclusionZoneGrid” objects
      corresponding to each grid cell within the inclusion zone. The
      grid cell center is used as the center point of the circular plot
      that defines the chainsaw intersection of the plot with the log
      for each cell.  }
	
    \item{\code{iz}:}{Object of class \code{"InclusionZone"}: In this
      class, this slot contains the overall
      "\code{\linkS4class{sausageIZ}}" inclusion zone object which
      delineates the grid cells that intersect the log under protocol
      1. Note that one can find the plot radius, etc. within the object
      contained in this slot. }
	
  }
}

\section{Extends}{
Class \code{"\linkS4class{InclusionZoneGrid}"}, directly.
}

\section{Methods}{
No methods defined with class "csFullInclusionZoneGrid" in the
signature. Because this is simple extension subclass of
\dQuote{InclusionZoneGrid,} it's plotting and other routines will work
on this class.
}

\references{
Gove, J. H. and Van Deusen, P. C. 2011. On fixed-area plot sampling for
downed coarse woody debris. \emph{Forestry}. (Accepted).\cr
}

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}


\seealso{
\code{\linkS4class{InclusionZoneGrid}}, \code{\linkS4class{Stem}},
\code{\linkS4class{Tract}}, \code{\linkS4class{sampSurf}}, \code{\linkS4class{InclusionZone}}
}
\examples{
showClass("csFullInclusionZoneGrid")
}
\keyword{classes}
