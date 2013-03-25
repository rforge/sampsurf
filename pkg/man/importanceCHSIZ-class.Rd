\name{importanceCHSIZ-class}
\Rdversion{1.1}
\docType{class}
\alias{importanceCHSIZ-class}
\alias{izGrid,importanceCHSIZ,Tract-method}

\title{Class \code{"importanceCHSIZ"}}

\description{This class holds the inclusion zone definition for the
  importance sampling variant/protocol of critical
  height sampling for standing trees.
}

\section{
  Objects from the Class}{ Objects can be created by calls of the
form \code{new("importanceCHSIZ", ...)}. However, this is not
recommended because the objects are fairly complex. Instead, one can use
the object constructor "\code{\link{importanceCHSIZ}}" to create new
objects.  }

\section{Slots}{

  This class is a subclass of \dQuote{criticalHeightIZ} (see below). It
  adds no new slots to the definition of that class, so please refer to
  it for the slot definitions.
  
}

\section{Extends}{
Class \code{"\linkS4class{criticalHeightIZ}"}, directly.\cr
Class \code{"\linkS4class{horizontalPointIZ}"}, by class "criticalHeightIZ", distance 2.\cr
Class \code{"\linkS4class{circularPlotIZ}"}, by class "criticalHeightIZ", distance 3.\cr
Class \code{"\linkS4class{standingTreeIZ}"}, by class "criticalHeightIZ", distance 4.\cr
Class \code{"\linkS4class{InclusionZone}"}, by class "criticalHeightIZ", distance 5.
}

\section{Methods}{  
  \describe{
	
    \item{izGrid}{\code{signature(izObject = "importanceCHSIZ", tract =
		"Tract")}:  \dQuote{InclusionZoneGrid} constructor }
	
	 }
   }
   
\references{
\describe{
\item{}{T. B. Lynch and J. H. Gove. 2013. An antithetic variate to
  facilitate upper-stem height measurements for critical height sampling
  and fixed-radius plot sampling with importance
  sampling. \emph{Canadian Journal of Forest Research} (forthcoming). }
%
\item{}{\dQuote{\emph{The InclusionZone Class}} vignette.}
}}

\author{
  Jeffrey H. Gove
}


\seealso{
See also the \code{"\linkS4class{circularPlotIZ}"},
\code{"\linkS4class{horizontalPointIZ}"} and
\code{"\linkS4class{criticalHeightIZ}"} classes, and the
"\code{\linkS4class{standingTreeIZs}}" container class
}

\examples{
showClass("importanceCHSIZ")
}

\keyword{classes}
