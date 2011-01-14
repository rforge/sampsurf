\name{pointRelascope-class}
\Rdversion{1.1}
\docType{class}
\alias{pointRelascope-class}
\alias{summary,pointRelascope-method}

\title{Class "pointRelascope": Point Relascope Sampling}

\description{
A subclass of "\code{\linkS4class{ArealSampling}}" that can be used to
create objects that encapsulate all the parameters necessary for point
relascope sampling of down woody debris. 
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("pointRelascope",
  ...)}, and while this is reasonable with this class because there are
no graphical slots, it is still not recommended. The preferred method
for creating new objects is via the \code{\link{pointRelascope}} constructor.
}

\section{Slots}{
  In addition to those slots in class "ArealSampling," the following are defined\ldots
  \describe{
	
    \item{\code{angleDegrees}:}{Object of class \code{"numeric"}: The
      relascope angle in degrees such that \eqn{0<\nu \le 90} degrees.  }
	
    \item{\code{angleRadians}:}{Object of class \code{"numeric"}: The
      relascope angle in radians. }
	
    \item{\code{phi}:}{Object of class \code{"numeric"}: The area factor
      multiplier, \eqn{\phi}, for angle \eqn{\nu}, as described in the
      references below. }
	
    \item{\code{slFactor}:}{Object of class \code{"numeric"}: The
      squared length factor, \eqn{L}, defining the constant amount of
      length-square per unit area (acre or hectare) as described in the
      references below.  }
	
    \item{\code{rwFactor}:}{Object of class \code{"numeric"}: The
      reach:width ratio or factor that makes it simpler to keep track of
      some of the more useful relascope angles, especially when
      constructing a relascope.  }
  }
}

\section{Extends}{
Class \code{"\linkS4class{ArealSampling}"}, directly.
}

\section{Methods}{
  \describe{
    \item{summary}{\code{signature(object = "pointRelascope")}: prints a
      summary of the object}
	 }
}
\references{
\describe{
\item{}{J. H. Gove, A. Ringvall, G. Stahl, and M. J. Ducey. 1999. Point
  relascope sampling of downed coarse woody debris. \emph{Canadian
	Journal of Forest Research} \bold{29}(11):1718--1726.}

\item{}{J. H. Gove, M. J. Ducey, A. Ringvall, and G. Stahl. 2001. Point
  relascope sampling: A new way to assess down coarse woody
  debris. \emph{Journal of Forestry} \bold{4}:4--11.}
}}

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}

\seealso{
The "\code{\linkS4class{ArealSampling}}" class.
}

\examples{
showClass("pointRelascope")
}
\keyword{classes}
