\name{perpendicularDistance-class}
\Rdversion{1.1}
\docType{class}
\alias{perpendicularDistance-class}
%\alias{distanceLimitedPDSIZ,downLog,perpendicularDistance,dlsNumeric-method}
%\alias{omnibusDLPDSIZ,downLog,perpendicularDistance,dlsNumeric-method}
%\alias{omnibusPDSIZ,downLog,perpendicularDistance-method}
%\alias{perpendicularDistanceIZ,downLog,perpendicularDistance-method}
\alias{summary,perpendicularDistance-method}

\title{Class \code{"perpendicularDistance"}: Perpendicular Distance Sampling}

\description{A subclass of "\code{\linkS4class{ArealSampling}}" that can be used to
create objects that encapsulate all the parameters necessary for any
variant of perpendicular distance sampling of down woody debris.
}

\section{Objects from the Class}{ Objects can be created by calls of the
form \code{new("perpendicularDistance", ...)}, and while this is
reasonable with this class because there are no graphical slots, it is
still not recommended. The preferred method for creating new objects is
via the \code{\link{perpendicularDistance}} constructor.  }

\section{Slots}{
  In addition to those slots in class "ArealSampling," the following are defined\ldots
  \describe{
    \item{\code{factor}:}{Object of class \code{"numeric"}: The volume,
      surface area, or coverage area factor in the appropriate units. }
	
    \item{\code{kpds}:}{Object of class \code{"numeric"}: The
       \emph{Kpds} factor in the appropriate units. }
  }
}

\section{Extends}{
Class \code{"\linkS4class{ArealSampling}"}, directly.
}

\section{Methods}{
  \describe{
    \item{summary}{\code{signature(object = "perpendicularDistance")}: prints a
      summary of the object }
	 }
}
\references{
\describe{
\item{}{Williams, M. S. and Gove, J. H. 2003. Perpendicular distance sampling:
  an alternative method for sampling downed coarse woody
  debris. \emph{Canadian Journal of Forest Research}
  \bold{33}:1564--1579.}
%
\item{}{Williams, M. S., Ducey, M. J. and Gove, J. H. 2005. Assessing
  surface area of coarse woody debris with line intersect and
  perpendicular distance sampling. \emph{Canadian Journal of Forest Research}
  \bold{35}:949--960.}
}}

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}

\seealso{
The "\code{\linkS4class{ArealSampling}}" class.
}

\examples{
showClass("perpendicularDistance")
}
\keyword{classes}
