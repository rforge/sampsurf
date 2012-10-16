\name{lineSegment-class}
\Rdversion{1.1}
\docType{class}
\alias{lineSegment-class}
\alias{bbox,lineSegment-method}
%\alias{plot,lineSegment,missing-method}
\alias{summary,lineSegment-method}

\title{Class \code{"lineSegment"}: sample line segments}

\description{A subclass of "\code{\linkS4class{ArealSampling}}" that can be used to
create line segment objects for use in methods where sampling is
conducted along a line.
}

\section{Objects from the Class}{Objects can be created by calls of the form \code{new("lineSegment",
  ...)}; however, this is not recommended due to the necessity to have
the spatial representation correct. Preferably, one should use the
\code{\link{lineSegment}} constructor for this class.
}

\section{Slots}{
  \describe{
	
    \item{\code{orientation}:}{Object of class \code{"numeric"}: The
    orientation of the line segment clockwise from \emph{north} as an
    azimuth in radians. Please note that this is different from the
    \code{logAngle} slot in "\code{\linkS4class{downLog}}" objects,
    which is defined counter-clockwise from due east, rather then
    north. Note that the constructor expects the orientation
    to be in degrees, not radians. }
  
  \item{\code{length}:}{Object of class \code{"numeric"}:  The length of the line segment in the correct units. }
  
  \item{\code{segment}:}{Object of class \code{"SpatialLines"}: The graphical object corresponding to the
    line segment itself. }
  
  \item{\code{location}:}{Object of class \code{"SpatialPoints"}: This
    is a \code{"SpatialPoints"} representation of the location of the
    object. In the \dQuote{lineSegment} class, this is the center of the
    line segment, which will often correspond to the \code{location}
    slot in the \dQuote{Stem} object under sampling surface
    simulations. }
  
  \item{\code{spID}:}{Object of class \code{"character"}: A unique
    identifier that is used in the \dQuote{SpatialPolygons}
    representation of the object. }
  
  \item{\code{spUnits}:}{Object of class \code{"CRS"}:  A valid string
      of class \dQuote{CRS} denoting the spatial units coordinate system (?CRS
      for more information) as in package \pkg{sp}. }
  
  }
}

\section{Extends}{
Class \code{"\linkS4class{ArealSampling}"}, directly.
}
\section{Methods}{
  \describe{
    \item{bbox}{\code{signature(obj = "lineSegment")}: Return the
      bounding box. }
    \item{plot}{\code{signature(x = "lineSegment", y = "missing")}: Graphical display of the object. }
    \item{summary}{\code{signature(object = "lineSegment")}: A summary
      of the object. }
	 }
}
%\references{
%%  ~~put references to the literature/web site here~~
%}

\author{
Jeffrey H. Gove 
}

%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  The "\code{\linkS4class{ArealSampling}}" and "\code{\linkS4class{horizontalLineIZ}}" classes.
}

\examples{
showClass("lineSegment")
}

\keyword{classes}
