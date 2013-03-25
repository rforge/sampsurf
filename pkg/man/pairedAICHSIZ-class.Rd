\name{pairedAICHSIZ-class}
\Rdversion{1.1}
\docType{class}
\alias{pairedAICHSIZ-class}
\alias{izGrid,pairedAICHSIZ,Tract-method}

\title{Class \code{"pairedAICHSIZ"}}

\description{This class holds the inclusion zone definition for the
  paired antithetic importance sampling variant/protocol of critical
  height sampling for standing trees.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("pairedAICHSIZ",
  ...)}.  However, this is not
recommended because the objects are fairly complex. Instead, one can use
the object constructor "\code{\link{pairedAICHSIZ}}" to create new
objects.
}

\section{Slots}{

  This class is a subclass of \dQuote{importanceCHSIZ} (see below). It
  adds no new slots to the definition of that class, so please refer to
  it for the slot definitions.
  
}

\section{Extends}{
Class \code{"\linkS4class{importanceCHSIZ}"}, directly.\cr
Class \code{"\linkS4class{criticalHeightIZ}"}, by class "importanceCHSIZ", distance 2.\cr
Class \code{"\linkS4class{horizontalPointIZ}"}, by class "importanceCHSIZ", distance 3.\cr
Class \code{"\linkS4class{circularPlotIZ}"}, by class "importanceCHSIZ", distance 4.\cr
Class \code{"\linkS4class{standingTreeIZ}"}, by class "importanceCHSIZ", distance 5.\cr
Class \code{"\linkS4class{InclusionZone}"}, by class "importanceCHSIZ", distance 6.
}

\section{Methods}{
  \describe{
    \item{izGrid}{\code{signature(izObject = "pairedAICHSIZ", tract =
  "Tract")}: \dQuote{InclusionZoneGrid} constructor } 
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
\code{"\linkS4class{horizontalPointIZ}"}, 
\code{"\linkS4class{criticalHeightIZ}"},
\code{"\linkS4class{importanceCHSIZ}"}, and
\code{"\linkS4class{antitheticICHSIZ}"} classes, and the
"\code{\linkS4class{standingTreeIZs}}" container class
}

\examples{
showClass("pairedAICHSIZ")
}
\keyword{classes}
