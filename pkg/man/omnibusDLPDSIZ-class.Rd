\name{omnibusDLPDSIZ-class}
\Rdversion{1.1}
\docType{class}
\alias{omnibusDLPDSIZ-class}

\title{Class \code{"omnibusDLPDSIZ"}}

\description{ This class holds the inclusion zone definition for the
\sQuote{omnibus distance limited perpendicular distance sampling} method
for sampling down coarse woody debris. This class is fairly complicated
because there are three possibilities for the components of the
inclusion zone. It is best to read \emph{"The InclusionZone Class"}
vignette, along with the references for more information---the
why's and wherefore's are not presented here, only the class
documentation.}

\section{Objects from the Class}{ Objects can be created by calls of the
form \code{new("omnibusDLPDSIZ", ...)}. However, this is not recommended
because the objects are fairly complex. Instead, one can use the object
constructor \code{\link{omnibusDLPDSIZ}} to create new objects.  }

\section{Slots}{

  This class is a direct descendent (subclass) of
  \code{"distanceLimitedPDSIZ"} and adds no new slots to that
  definition. Please see the \code{"\linkS4class{distanceLimitedPDSIZ}"}
  class definition for details.
  

}
\section{Extends}{
Class \code{"\linkS4class{distanceLimitedPDSIZ}"}, directly.\cr
Class \code{"\linkS4class{perpendicularDistanceIZ}"}, by class "distanceLimitedPDSIZ", distance 2.\cr
Class \code{"\linkS4class{downLogIZ}"}, by class "distanceLimitedPDSIZ", distance 3.\cr
Class \code{"\linkS4class{pdsIZNull}"}, by class "distanceLimitedPDSIZ", distance 3.\cr
Class \code{"\linkS4class{InclusionZone}"}, by class "distanceLimitedPDSIZ", distance 4.
}

\section{Methods}{
No new methods are defined with class "omnibusDLPDSIZ" in the signature; instead
see those defined for \code{"\linkS4class{distanceLimitedPDSIZ}"}.
}

\references{
This method has not been published yet, please see the vignette
mentioned above for details.
}

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}

\seealso{
\code{\linkS4class{downLogIZs}} container class
}

\examples{
showClass("omnibusDLPDSIZ")
}
\keyword{classes}
