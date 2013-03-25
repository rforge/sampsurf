\name{criticalHeightIZ-class}
\Rdversion{1.1}
\docType{class}
\alias{criticalHeightIZ-class}
\alias{izGrid,criticalHeightIZ,Tract-method}
\alias{summary,criticalHeightIZ-method}

\title{Class \code{"criticalHeightIZ"}}

\description{This class holds the inclusion zone definition for critical
  height sampling for standing trees.
}

\section{Objects from the Class}{
Objects can be created by calls of the form
\code{new("criticalHeightIZ", ...)}. However, this is not recommended
because the objects are fairly complex. Instead, one can use the object
constructor "\code{\link{criticalHeightIZ}}" to create new objects.
}

\section{Slots}{

  This class is a direct descendant of
  "\code{\linkS4class{horizontalPointIZ}}" and adds one new slot to the
  class definition; refer to that class description for other slots and their
  meanings. Please do note that the other slots listed below have slightly
  different meanings here\ldots
  
  \describe{
    \item{\code{referenceHeight}:}{Object of class \code{"character"}:
    The reference height at which the inclusion zone is determined. It
    can be either "butt" for the base of the tree, or "dbh" for breast
    height as specified in  \code{.StemEnv$referenceCHSIZ}.
     }
  
    \item{\code{puaBlowup}:}{Object of class \code{"numeric"}: The
     exansion factor for every tree is simply the basal area factor (BAF).  }
  
    \item{\code{puaEstimates}:}{Object of class \code{"list"}: Only
       volume may be estimated under critical height sampling, all other
	   attribute estimates are \code{NA}. }
	 
  }
}

\section{Extends}{
Class \code{"\linkS4class{horizontalPointIZ}"}, directly.\cr
Class \code{"\linkS4class{circularPlotIZ}"}, by class "horizontalPointIZ", distance 2.\cr
Class \code{"\linkS4class{standingTreeIZ}"}, by class "horizontalPointIZ", distance 3.\cr
Class \code{"\linkS4class{InclusionZone}"}, by class "horizontalPointIZ", distance 4.
}

\section{Methods}{
  \describe{
    \item{izGrid}{\code{signature(izObject = "criticalHeightIZ", tract =
		"Tract")}: \dQuote{InclusionZoneGrid} constructor }
	
    \item{summary}{\code{signature(object = "criticalHeightIZ")}: Object summary }
	
	 }
   }
   
\references{
There are numerous references to critical height sampling, the following
are a sample\ldots
\describe{
\item{}{T. G. Gregroire and H. T. Valentine. 2009. \emph{Sampling
	strategies for natural resources and the environment}. Chapman and
  Hall/CRC, 496p.}
%
\item{}{\dQuote{\emph{The InclusionZone Class}} vignette.}
}
}

\author{
  Jeffrey H. Gove
}

\seealso{
See also the
\code{"\linkS4class{circularPlotIZ}"} and
\code{"\linkS4class{horizontalPointIZ}"} classes, and the "\code{\linkS4class{standingTreeIZs}}" container class
}

\examples{
showClass("criticalHeightIZ")
}

\keyword{classes}
