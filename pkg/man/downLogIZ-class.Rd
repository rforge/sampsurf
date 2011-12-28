\name{downLogIZ-class}
\Rdversion{1.1}
\docType{class}
\alias{downLogIZ-class}
\alias{summary,downLogIZ-method}

\title{Class "downLogIZ"}

\description{
  This represents an incremental change from the virtual base 
  "\code{\linkS4class{InclusionZone}}" class. It is meant to clearly
  facilitate the division between standing trees and down logs with
  regard to inclusion zones, and it is a virtual class like its direct
  parent.
}

\section{Objects from the Class}{A virtual Class: No objects may be
  created from it.}

\section{Slots}{
  
  Only one slot has been added to this class, please see the definition
  of the "\code{\linkS4class{InclusionZone}}" class for other inherited
  slots\ldots
  
  \describe{
    \item{\code{downLog}:}{Object of class \code{"downLog"}: Holds and
	  object of class "\code{\linkS4class{downLog}}". }
  }
}

\section{Extends}{
Class \code{"\linkS4class{InclusionZone}"}, directly.
}

\section{Methods}{
  \describe{
    \item{summary}{\code{signature(object = "downLogIZ")}: Print a summary for
      any subclass object }
	 }
   }
   

\references{
\dQuote{\emph{The InclusionZone Class}} vignette.
}

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  For subclasses, see: 
  \code{\linkS4class{standUpIZ}},
  \code{\linkS4class{chainSawIZ}},
  \code{\linkS4class{sausageIZ}},
  \code{\linkS4class{pointRelascopeIZ}},
  \code{\linkS4class{perpendicularDistanceIZ}},
  \code{\linkS4class{omnibusPDSIZ}},
  \code{\linkS4class{distanceLimitedPDSIZ}},
  \code{\linkS4class{omnibusDLPDSIZ}}, 
  \code{\linkS4class{distanceLimitedIZ}},  
  \code{\linkS4class{distanceLimitedMCIZ}}, 
  and the \code{\linkS4class{downLogIZs}} container class.  }

\examples{
showClass("downLogIZ")
}
\keyword{classes}
