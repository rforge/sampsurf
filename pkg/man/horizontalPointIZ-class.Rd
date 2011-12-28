\name{horizontalPointIZ-class}
\Rdversion{1.1}
\docType{class}
\alias{horizontalPointIZ-class}
\alias{summary,horizontalPointIZ-method}

\title{Class \code{"horizontalPointIZ"}}

\description{ This class holds the inclusion zone definition for the
horizontal point sampling method for standing trees.
}

\section{Objects from the Class}{
Objects can be created by calls of the form
\code{new("horizontalPointIZ", ...)}. However, this is not recommended
because the objects are fairly complex. Instead, one can use the object
constructor \code{\link{horizontalPointIZ}} to create new objects.
}

\section{Slots}{
  
  This class adds one slot to the "circularPlotIZ" class specification,
  please see the superclasses (see below) for more definitions.
  
  \describe{ \item{\code{angleGauge}:}{Object of class
    \code{"angleGauge"}: A subclass object from the
    "\code{\linkS4class{ArealSampling}}" class. Please see the help for this
    class for more information on the slots associated with angle
    gauge objects. } } }

\section{Extends}{
Class \code{"\linkS4class{circularPlotIZ}"}, directly.\cr
Class \code{"\linkS4class{standingTreeIZ}"}, by class "circularPlotIZ", distance 2.\cr
Class \code{"\linkS4class{InclusionZone}"}, by class "circularPlotIZ", distance 3.
}

\section{Methods}{
  \describe{
    \item{summary}{\code{signature(object = "horizontalPointIZ")}: Show
      an object summary. }
	 }
   }
   
\references{
\describe{
\item{}{T. G. Gregroire and H. T. Valentine. 2009. \emph{Sampling
	strategies for natural resources and the environment}. Chapman and
  Hall/CRC, 496p.}
%
\item{}{\dQuote{\emph{The InclusionZone Class}} vignette.}
}
}

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}


\seealso{
\code{"\linkS4class{circularPlotIZ}"}, and the \code{\linkS4class{standingTreeIZs}} container class
}

\examples{
showClass("horizontalPointIZ")
}
\keyword{classes}
