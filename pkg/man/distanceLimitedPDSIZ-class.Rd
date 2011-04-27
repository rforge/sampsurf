\name{distanceLimitedPDSIZ-class}
\Rdversion{1.1}
\docType{class}
\alias{distanceLimitedPDSIZ-class}
%\alias{izGrid,distanceLimitedPDSIZ,Tract-method}
%\alias{plot,distanceLimitedPDSIZ,missing-method}
\alias{summary,distanceLimitedPDSIZ-method}

\title{Class \code{"distanceLimitedPDSIZ"}}

\description{This class holds the inclusion zone definition for the
\sQuote{distance limited perpendicular distance sampling} method (Ducey
et. al) for sampling down coarse woody debris. This class is fairly
complicated because there are three possibilities for the components of
the inclusion zone. It is best to read \emph{"The InclusionZone Class"}
vignette, along with the paper cited above for more information---the
why's and wherefore's are not presented here, only the class documentation.
}

\section{Objects from the Class}{
Objects can be created by calls of the form
\code{new("distanceLimitedPDSIZ", ...)}. However, this is not recommended
because the objects are fairly complex. Instead, one can use the object
constructor \code{\link{distanceLimitedPDSIZ}} to create new objects.
}

\section{Slots}{
  
  Most of the slots are described in the superclasses (see below),
  please see their help pages for more information. This class adds a
  few slots to the \code{"\linkS4class{perpendicularDistanceIZ}"} class
  specification as described below, please especially see that class for
  more details on other slots.
  
  \describe{
    \item{\code{dls}:}{Object of class \code{"distanceLimited"}
      supplying the distance limit for the inclusion zone. }
	
    \item{\code{dlsDiameter}:}{Object of class \code{"numeric"}: The
      limiting diameter for the zone transition between the regular PDS
      portion and the DLMC portion (see below, either or both may be missing). }
	
    \item{\code{pdsPart}:}{Object of class \code{"\linkS4class{pdsIZNull}"} holding
      the perpendicular distance sampling portion of the inclusion zone
      as a \code{"\linkS4class{perpendicularDistanceIZ}"} object (or
      subclass, i.e., \code{"\linkS4class{omnibusPDSIZ}"} object), or
      \code{NULL} if none exists. }
	
    \item{\code{dlsPart}:}{Object of class \code{"\linkS4class{dlmcIZNull}"} holding
      the distance limited Monte Carlo portion of the inclusion zone as a
      \code{"\linkS4class{distanceLimitedMCIZ}"} object, or
      \code{NULL} if none exists. }
	
    \item{\code{pdsFull}:}{Object of class \code{"\linkS4class{pdsIZNull}"} holding a
       fully valid \code{"\linkS4class{perpendicularDistanceIZ}"} (or
       subclass, i.e., \code{"\linkS4class{omnibusPDSIZ}"} object)
       delineating the full PDS zone as it would appear if the entire
       log were treated as a PDS sampling object.  }

  }
}

\section{Extends}{
Class \code{"\linkS4class{perpendicularDistanceIZ}"}, directly.\cr
Class \code{"\linkS4class{downLogIZ}"}, by class "perpendicularDistanceIZ", distance 2.\cr
Class \code{"\linkS4class{pdsIZNull}"}, by class "perpendicularDistanceIZ", distance 2.\cr
Class \code{"\linkS4class{InclusionZone}"}, by class "perpendicularDistanceIZ", distance 3.
}

\section{Methods}{
  \describe{
    \item{izGrid}{\code{signature(izObject = "distanceLimitedPDSIZ",
		tract = "Tract")}:  \dQuote{InclusionZoneGrid} constructor }
	
    \item{plot}{\code{signature(x = "distanceLimitedPDSIZ", y =
		"missing")}: Plot the object }
	
    \item{summary}{\code{signature(object = "distanceLimitedPDSIZ")}: Object summary }
	 }
}

\references{
\describe{
%
\item{}{Ducey, M. J., Williams, M. S., Roberge, S., Kenning, R. S.,
and Gove ,J. H. 2005. Distance limited per- pendicular distance sampling
for coarse woody material: Theory and field results. Unpublished.}

}
}

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}


\seealso{
\code{\linkS4class{downLogIZs}} container class
}

\examples{
showClass("distanceLimitedPDSIZ")
}
\keyword{classes}
