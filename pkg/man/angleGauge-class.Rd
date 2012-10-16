\name{angleGauge-class}
\Rdversion{1.1}
\docType{class}
\alias{angleGauge-class}
%\alias{horizontalPointIZ,standingTree,angleGauge-method}
\alias{summary,angleGauge-method}

\title{Class \code{"angleGauge"}: For Angle Gauge Sampling Methods}

\description{ A subclass of "\code{\linkS4class{ArealSampling}}" that can be used to
create objects that encapsulate all the parameters necessary for, e.g.,
horizontal point (prism) sampling of standing trees (or downed logs).
}

\section{Objects from the Class}{ Objects can be created by calls of the
form \code{new("angleGauge", ...)}, and while this is reasonable with
this class because there are no graphical slots, it is still not
recommended. The preferred method for creating new objects is via the
\code{\link{angleGauge}} constructor.
}

\section{Slots}{
  
  In addition to those slots in class "ArealSampling," the following are defined\ldots
	
  \describe{
	
    \item{\code{angleDegrees}:}{Object of class \code{"numeric"}: The
      gauge angle in degrees, normally in the range 0 <
      \code{angleDegrees} <= 6.5 degrees. }
	
    \item{\code{angleRadians}:}{Object of class \code{"numeric"}: The
      gauge angle in radians. }
	
    \item{\code{diopters}:}{Object of class \code{"numeric"}: The prism
      angle in diopters (Delta).}
	
    \item{\code{k}:}{Object of class \code{"numeric"}: Angle gauge
	  constant (dimensionless); i.e., \eqn{2 \times
	  \sin(\frac{\eta}{2})}{2*sin(angleRadians/2)}, where
	  \eqn{\eta}{angleRadians} is \code{angleRadians} above. }
	
    \item{\code{prf}:}{Object of class \code{"numeric"}: The plot radius
      factor; English=ft/in, metric=m/cm. }
	
    \item{\code{PRF}:}{Object of class \code{"numeric"}  The plot radius
      factor; English=ft/ft, metric=m/m. }
	
    \item{\code{alpha}:}{Object of class \code{"numeric"} The plot
    radius proportionality constant; English=ft/ft, metric=m/m.  }
	  
    \item{\code{baf}:}{Object of class \code{"numeric"}: The
      corresponding basal area factor in the appropriate units;
      English=ft\eqn{{}^2}/acre, metric=m\eqn{^2}/ha. }
	  
    \item{\code{df}:}{Object of class \code{"numeric"}: Diameter factor
      for horizontal line sampling in inches or cm.}
	  
    \item{\code{DF}:}{Object of class \code{"numeric"}: Diameter factor
      for horizontal line sampling in feet or meters.}
	
  }
}

\section{Extends}{
Class \code{"\linkS4class{ArealSampling}"}, directly.
}

\section{Methods}{
  \describe{
	
    \item{horizontalPointIZ}{\code{signature(standingTree =
      "standingTree", angleGauge = "angleGauge")}: Object constructor
      for horizontal point sampling. }

	\item{summary}{\code{signature(object = "angleGauge")}: Show a
	  summary of the \code{object}. }
		
	 }
   }
   
\references{
\describe{
\item{}{T. G. Gregroire and H. T. Valentine. 2009. \emph{Sampling
	strategies for natural resources and the environment}. Chapman and
  Hall/CRC, 496p.}
%
}
}

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}

\seealso{
The "\code{\linkS4class{ArealSampling}}" class.
}

\examples{
showClass("angleGauge")
}
\keyword{classes}
