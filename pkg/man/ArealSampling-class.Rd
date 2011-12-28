\name{ArealSampling-class}
\Rdversion{1.1}
\docType{class}
\alias{ArealSampling-class}
%\alias{plot,ArealSampling,missing-method}
\alias{show,ArealSampling-method}
\alias{summary,ArealSampling-method}

\title{Class "ArealSampling"}
\description{
This virtual class facilitates the definition of subclasses
that will encapsulate the basic information required to define
individual areal sampling methods used in forestry.

For example, as detailed in \dQuote{The ArealSampling Class} vignette,
we may define subclasses based on whether the sampling methods are for
standing trees or downed logs. Other classes, like fixed-area plots,
could span these two main subclasses.
}
\section{Objects from the Class}{A virtual Class: No objects may be created from it.}
\section{Slots}{
  \describe{
    \item{\code{description}:}{Object of class \code{"character"}: Some
	  descriptive text about this class.}
	
    \item{\code{units}:}{Object of class \code{"character"}: A character
	  string specifying the units of measure. Legal values are \dQuote{English}
	  and \dQuote{metric.}  }

  }
}

\section{Methods}{
  \describe{
    \item{plot}{\code{signature(x = "ArealSampling", y = "missing")}: Graphical
	  display of the object. Minimal display is the object
	  \code{location}. }
	
    \item{show}{\code{signature(object = "ArealSampling")}: Show main components or
	  summary of an object. }
	
    \item{summary}{\code{signature(object = "ArealSampling")}:  Concise summary of
	  the object. }
	 }
}
\references{
Gove, J. H. and Van Deusen, P. C. 2011. On fixed-area plot sampling for
downed coarse woody debris. \emph{Forestry}. \emph{Forestry} \bold{84}:109--117.\cr
}

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 "\code{\linkS4class{circularPlot}}" for a circular plot
 subclass.\cr
% 
 "\code{\linkS4class{pointRelascope}}" for the point relascope
 sampling subclass.\cr
% 
 "\code{\linkS4class{perpendicularDistance}}" for the
 perpendicular distance  sampling subclass.\cr
% 
 "\code{\linkS4class{distanceLimited}}" for the
 distance limited sampling subclass.\cr
% 
 "\code{\linkS4class{angleGauge}}" for horizontal point sampling subclass.
}
\examples{
showClass("ArealSampling")
}
\keyword{classes}
