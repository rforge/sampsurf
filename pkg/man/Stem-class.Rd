\name{Stem-class}
\Rdversion{1.1}
\docType{class}
\alias{Stem-class}
\alias{plot,Stem,missing-method}
\alias{summary,Stem-method}

\title{Class "Stem" for building tree and log like subclasses}
\description{
  This class is virtual. It provides the minimal structure to build
  subclasses that can be tailored to the desired application. Notably,
  "downLog" and perhaps "standingTree" subclasses are obvious
  possibilities. It provides for graphical display via the
  \code{\link{sp}} package. 
}
\section{Objects from the Class}{A virtual Class: No objects may be
  created from it.}

\section{Slots}{
  \describe{
    \item{\code{species}:}{Object of class \code{"character"}: Some
	  form of identifier to species or group level. }
    \item{\code{units}:}{Object of class \code{"character"}: The units of
	  measure used, must be either "English" or "metric". }
    \item{\code{location}:}{Object of class \code{"SpatialPoints"}: The
	  x,y location of the object, can include z (elevation) but this is
	  not used currently. }
    \item{\code{spUnits}:}{Object of class \code{"CRS"}: A legal proj.4
	  coordinate system; the default is to have user-defined coordinates. }
    \item{\code{description}:}{Object of class \code{"character"}: Some
	  helpful comment about the object. }
    \item{\code{userExtra}:}{Object of class \code{"ANY"}: This could be
	  anyting the user wants to store with the object; specifically, it
	  might hold information that is difficult to pass incorporate in
	  any way given the constraints on "Stem" objects. }
  }
}

\section{Methods}{
  \describe{
    \item{plot}{\code{signature(x = "Stem", y = "missing")}: Graphical
	  display of the object. Minimal display is the object
	  \code{location}. }
	
    \item{show}{\code{signature(object = "Stem")}: Show main components or
	  summary of an object. }
	
    \item{summary}{\code{signature(object = "Stem")}: Concise summary of
	the object. }
	 }
}
%\references{
%%  ~~put references to the literature/web site here~~
%}
\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 Subclass "\code{\linkS4class{downLog}}" for downed logs.
}
\examples{
showClass("Stem")
}
\keyword{classes}
