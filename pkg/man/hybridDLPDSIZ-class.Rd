\name{hybridDLPDSIZ-class}
\Rdversion{1.1}
\docType{class}
\alias{hybridDLPDSIZ-class}

\title{Class \code{"hybridDLPDSIZ"}}

\description{This class holds the inclusion zone definition for the
\sQuote{hybrid distance limited perpendicular distance sampling} method
for sampling down coarse woody debris (Ducey et. al 2012). This class is fairly complicated
because there are three possibilities for the components of the
inclusion zone. It is best to read \emph{"The InclusionZone Class"}
vignette, along with the references for more information---the
why's and wherefore's are not presented here, only the class
documentation.

}

\section{Objects from the Class}{ Objects can be created by calls of the
form \code{new("hybridDLPDSIZ", ...)}. However, this is not recommended
because the objects are fairly complex. Instead, one can use the object
constructor \code{\link{hybridDLPDSIZ}} to create new objects.
}

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
Class \code{"\linkS4class{InclusionZone}"}, by class "distanceLimitedPDSIZ", distance 4.\cr
}
\section{Methods}{
No methods defined with class "hybridDLPDSIZ" in the signature; instead
see those defined for \code{"\linkS4class{distanceLimitedPDSIZ}"}.
}
\references{
\describe{
%
\item{}{Ducey, M. J., Williams, M. S.,  Gove, J. H., Roberge, S. and Kenning, R. S.,
2012. Distance limited perpendicular distance sampling
for coarse woody material: Theory and field results. \emph{Forestry} (in
review).}

}
}

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}


\seealso{
\code{\linkS4class{downLogIZs}} container class
}

\examples{
showClass("hybridDLPDSIZ")
}
\keyword{classes}
