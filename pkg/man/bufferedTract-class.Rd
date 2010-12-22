\name{bufferedTract-class}
\Rdversion{1.1}
\docType{class}
\alias{bufferedTract-class}
\alias{downLogs,numeric,bufferedTract-method}
\alias{plot,bufferedTract,missing-method}
\alias{show,bufferedTract-method}

\title{Class "bufferedTract"}
\description{
This class is a subclass of the \code{"\linkS4class{Tract}"} class. It
provides, in addition to the functionality in \dQuote{Tract}, the
ability to create an internal buffer within the tract. This might be
important, for example, when drawning a population of logs for sampling
surface generation. The buffer width can be set such that no log's
inclusion zone extends outside the tract. Objects of this class are also
descendants of class \code{"\linkS4class{RasterLayer}"}, and so share
the full functionality of this class.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("bufferedTract",
  ...)}. However, this is not recommended due to the complexity of the
class. Instead, use the \code{\link{bufferedTract}} constructor to
create objects.
}

\section{Slots}{
  In addition to those slots that are defined within the
  \code{"\linkS4class{Tract}"}, the following are available\ldots
  \describe{
    \item{\code{bufferRect}:}{Object of class \code{"matrix"}: A matrix
      in the form of a bounding box with row names \code{c("x","y")} 
      and column names \code{c("min","max").}}
    \item{\code{spBuffer}:}{Object of class \code{"SpatialPolygons"}: A
	  \code{\linkS4class{SpatialPolygons}} representation of \code{bufferRect} via the
	  \code{\link{sp}} package. }
  }
}

\section{Extends}{
Class \code{"\linkS4class{Tract}"}, directly.
Class \code{"\linkS4class{RasterLayer}"}, by class "Tract", distance 2.
Class \code{"\linkS4class{Raster}"}, by class "Tract", distance 3.
Class \code{"\linkS4class{BasicRaster}"}, by class "Tract", distance 4.
}

\section{Methods}{
  \describe{
    \item{downLogs}{\code{signature(object = "numeric", container = "bufferedTract")}: ... }
    \item{plot}{\code{signature(x = "bufferedTract", y = "missing")}: ... }
    \item{show}{\code{signature(object = "bufferedTract")}: ... }
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
\code{\linkS4class{Tract}}
}

\examples{
showClass("bufferedTract")
}
\keyword{classes}
