\name{standingTreeIZ-class}
\Rdversion{1.1}
\docType{class}
\alias{standingTreeIZ-class}
\alias{summary,standingTreeIZ-method}

\title{Class \code{"standingTreeIZ"}}

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
    \item{\code{standingTree}:}{Object of class \code{"standingTree"}: Holds an
	  object of class "\code{\linkS4class{standingTree}}". }
  }
}

\section{Extends}{
Class \code{"\linkS4class{InclusionZone}"}, directly.
}

\section{Methods}{
  \describe{
    \item{summary}{\code{signature(object = "standingTreeIZ")}: Print a summary for
      any subclass object }
	 }
   }
   

\references{
\dQuote{\emph{The InclusionZone Class}} vignette.
}

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}


\seealso{
  For subclasses, see: 
  \code{\linkS4class{circularPlotIZ}},
  and the \code{\linkS4class{standingTreeIZs}} container class.  
}

\examples{
showClass("standingTreeIZ")
}
\keyword{classes}
