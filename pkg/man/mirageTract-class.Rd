\name{mirageTract-class}
\Rdversion{1.1}
\docType{class}
\alias{mirageTract-class}
\alias{izGridMirage,InclusionZone,mirageTract-method}

\title{Class \code{"mirageTract"}}

\description{ This class is a subclass of the
\code{"\linkS4class{Tract}"} class. It provides no new functionality on
its own. Its purpose is to define a class that will convey to other
components in the system the implicit request to apply the mirage method
to any stems and inclusion zones on the tract that overlap the boundary. For
example, using a \dQuote{mirageTract} in one of the
\code{\link{sampSurf}} constructors will tell the constructor that the
mirage method should be used on the simulation.  }

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("mirageTract",
  ...)}.  However, this is not recommended due to the complexity of the
class. Instead, use the \code{\link{mirageTract}} constructor to
create objects.
}

\section{Slots}{
  There are no new slots add to this class, please see the slot
  definition for the superclass \code{"\linkS4class{Tract}"}.
}


\section{Extends}{
Class \code{"\linkS4class{Tract}"}, directly.\cr
Class \code{"\linkS4class{RasterLayer}"}, by class "Tract", distance 2.\cr
Class \code{"\linkS4class{Raster}"}, by class "Tract", distance 3.\cr
Class \code{"\linkS4class{RLNULL}"}, by class "RasterLayer", distance 3.\cr
Class \code{"\linkS4class{BasicRaster}"}, by class "Tract", distance 4.\cr
}

\section{Methods}{
  \describe{
	
    \item{izGridMirage}{\code{signature(izObject = "InclusionZone",
        tract = "mirageTract")}: Creates
        \code{"\linkS4class{mirageInclusionZoneGrid}"} objects. }

	 }
   }
   
\references{
Gregoire and Valentine (2008) for the mirage method.
}

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}


\seealso{
\code{\linkS4class{Tract}}, \code{\linkS4class{bufferedTract}}
}

\examples{
showClass("mirageTract")
}

\keyword{classes}
