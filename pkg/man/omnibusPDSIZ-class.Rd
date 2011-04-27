\name{omnibusPDSIZ-class}
\Rdversion{1.1}
\docType{class}
\alias{omnibusPDSIZ-class}
\alias{summary,omnibusPDSIZ-method}

\title{Class \code{"omnibusPDSIZ"}}

\description{This class holds the inclusion zone definition for the
\sQuote{omnibus perpendicular distance sampling} method (Ducey et. al, 2008) for sampling down
coarse woody debris.
}

\section{Objects from the Class}{ Objects can be created by calls of the
form \code{new("omnibusPDSIZ", ...)}. However, this is not recommended
because the objects are fairly complex. Instead, one can use the object
constructor \code{\link{omnibusPDSIZ}} to create new objects.  }

\section{Slots}{
  
  This class is a direct descendent (subclass) of
  \code{"perpendicularDistanceIZ"} and adds no new slots to that
  definition. Please see the
  \code{"\linkS4class{perpendicularDistanceIZ}"} class definition for
  details.  

}

\section{Extends}{
Class \code{"\linkS4class{perpendicularDistanceIZ}"}, directly.\cr
Class \code{"\linkS4class{pdsIZNull}"}, directly.\cr
Class \code{"\linkS4class{downLogIZ}"}, by class "perpendicularDistanceIZ", distance 2.\cr
Class \code{"\linkS4class{InclusionZone}"}, by class "perpendicularDistanceIZ", distance 3.
}

\section{Methods}{
  \describe{
    \item{izGrid}{\code{signature(izObject = "omnibusPDSIZ",
		tract = "Tract")}: \dQuote{InclusionZoneGrid} generic constructor }
	
	 }
   }
   
\references{
\describe{
%
\item{}{Ducey, M. J., Williams, M. S., Gove, J. H. and Valentine,
  H. T. 2008. Simultaneous unbiased estimates of multiple downed wood
  attributes in perpendicular distance sampling. \emph{Canadian Journal of
  Forest Research} \bold{38}:2044--2051.}
%
\item{}{Williams, M. S. and Gove, J. H. 2003. Perpendicular distance sampling:
  an alternative method for sampling downed coarse woody
  debris. \emph{Canadian Journal of Forest Research}
  \bold{33}:1564--1579.}
}
}

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}


\seealso{
\code{\linkS4class{downLogIZs}} container class
}

\examples{
showClass("omnibusPDSIZ")
}
\keyword{classes}
