\name{horizontalPointCVIZ-class}
\Rdversion{1.1}
\docType{class}
\alias{horizontalPointCVIZ-class}

\title{Class \code{"horizontalPointCVIZ"}}

\description{
  This class allows one to perform control variate sampling within a
  given tree as a second stage sample, when the tree has been selected
  by horizontal point sampling in the first stage. Thus, the class
  definition is simply a combination of the
  \code{"\linkS4class{MonteCarloSamplingIZ}"} and
  \code{"\linkS4class{horizontalPointIZ}"} classes. No new slots have
  been added to this class, please see the above two classes for the
  totality of slot definitions in the class structure.
}

\section{Objects from the Class}{
Objects can be created by calls of the form
\code{new("horizontalPointCVIZ", ...)}.  However, because the
object is fairly complex, using the object constructor of the same name
\code{\link{horizontalPointCVIZ}} is the preferred method for creating
valid objects of this class.
}

\section{Slots}{

  Please see the slot definitions in the \code{"\linkS4class{MonteCarloSamplingIZ}"} and
  \code{"\linkS4class{horizontalPointIZ}"} classes and their subclasses
  where applicable for all of the slot definitions.

  In addition, the two vignettes below will be of some help as the class
  structures are also described therein.
}

\section{Extends}{
Class \code{"\linkS4class{MonteCarloSamplingIZ}"}, directly.\cr
Class \code{"\linkS4class{horizontalPointIZ}"}, directly.\cr
Class \code{"\linkS4class{horizontalPointMonteCarloSamplingIZ}"}, directly.\cr
Class \code{"\linkS4class{circularPlotIZ}"}, by class "horizontalPointIZ", distance 2.\cr
Class \code{"\linkS4class{standingTreeIZ}"}, by class "horizontalPointIZ", distance 3.\cr
Class \code{"\linkS4class{InclusionZone}"}, by class "horizontalPointIZ", distance 4.
}

\section{Methods}{
No methods defined with class "horizontalPointCVIZ" in the signature.
}

\references{
\describe{
%
\item{}{Gove, J. H. 2013. Monte Carlo sampling methods in
  \pkg{sampSurf}. Package vignette.}
%
\item{}{Gove, J. H. 2013. The \dQuote{InclusionZone} Class. \pkg{sampSurf} Package vignette.}
}
}

\author{
  Jeffrey H. Gove %, \email{jhgove@unh.edu}
}

\seealso{ Other Monte Carlo methods that work with horizontal point
  sampling include: 
  \code{\linkS4class{horizontalPointCMCIZ}},
  \code{\linkS4class{horizontalPointISIZ}},
  \code{\linkS4class{criticalHeightIZ}},
  \code{\linkS4class{importanceCHSIZ}},
  \code{\linkS4class{antitheticICHSIZ}},
  \code{\linkS4class{pairedAICHSIZ}}.
}

\examples{
showClass("horizontalPointCVIZ")
}
\keyword{classes}
