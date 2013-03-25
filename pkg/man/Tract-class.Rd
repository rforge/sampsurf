\name{Tract-class}
\Rdversion{1.1}
\docType{class}
\alias{Tract-class}
\alias{bbox,Tract-method}
%\alias{bufferedTract,numeric,Tract-method}
%\alias{heapIZ,InclusionZoneGrid,Tract-method}
%\alias{izGridCSFull,InclusionZoneGrid,Tract-method}
%\alias{izGrid,chainSawIZ,Tract-method}
%\alias{izGrid,matrix,Tract-method}
%\alias{izGrid,sausageIZ,Tract-method}
%\alias{izGrid,standUpIZ,Tract-method}
%\alias{perimeter,Tract-method}
%\alias{plot,Tract,missing-method}
%\alias{plot3D,Tract-method}
%\alias{sampSurf,downLogIZs,Tract-method}
%\alias{sampSurf,numeric,Tract-method}
\alias{show,Tract-method}
\alias{summary,Tract-method}

\title{Class "Tract"}
\description{
  The "Tract" class is the base class for creating objects corresponding
  to raster grids for use in samplng surface simulation. The class is a
  subclass of \code{"\linkS4class{RasterLayer}"} and so incorporates all
  of the slots from that class as well as the ones defined below.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Tract", ...)};
however, due to the complexity of the class, this is not
recommended. Instead, one can use on of the class constructor methods of
the same name, \code{\link{Tract}}, to create and object. There are
several of these available, giving a variety of ways to create a raster
grid for use in sampling surface simulation.

There is currently one subclass, \code{"\linkS4class{bufferedTract}"},
which allows an internal buffer.
}

\section{Slots}{
  In addition to the slots found in the
  \code{"\linkS4class{RasterLayer}"} class, the following slots are
  defined\ldots 
  \describe{
    \item{\code{description}:}{Object of class \code{"character"}: A
	  character vector describing the tract if desired. }
    \item{\code{units}:}{Object of class \code{"character"}: The units of
	  measure used, must be either "English" or "metric".  }
    \item{\code{area}:}{Object of class \code{"numeric"}: The area of
	  the tract in appropriate units. }
  }
}
\section{Extends}{
Class \code{"\linkS4class{RasterLayer}"}, directly.
Class \code{"\linkS4class{Raster}"}, by class "RasterLayer", distance 2.
Class \code{"\linkS4class{BasicRaster}"}, by class "RasterLayer", distance 3.
}

\section{Methods}{
  \describe{
    \item{bbox}{\code{signature(obj = "Tract")}: Return object bounding
	  box }
	
    \item{bufferedTract}{\code{signature(bufferWidth = "numeric", tract
		= "Tract")}: Object constructor }
	
    \item{heapIZ}{\code{signature(izgObject = "InclusionZoneGrid", tract
		= "Tract")}: Heap into a sampling surface }
	
    \item{izGridCSFull}{\code{signature(izGridSausage =
      "InclusionZoneGrid", tract = "Tract")}: Full chainsaw
      InclusionZoneGrid }

    \item{izGrid}{\code{signature(izObject = "chainSawIZ", tract =
		"Tract")}: \dQuote{InclusionZoneGrid} generic constructor }
	
    \item{izGrid}{\code{signature(izObject = "matrix", tract =
		"Tract")}: ... }
	
    \item{izGrid}{\code{signature(izObject = "sausageIZ", tract =
		"Tract")}: ... }
	
    \item{izGrid}{\code{signature(izObject = "standUpIZ", tract =
		"Tract")}: ... }
	
    \item{perimeter}{\code{signature(object = "Tract")}: Returns object perimeter }
	
    \item{plot}{\code{signature(x = "Tract", y = "missing")}: Plot the
      object }
	
    \item{sampSurf}{\code{signature(object = "downLogIZs", tract =
		"Tract")}: Generate a sampling surface }
	
    \item{sampSurf}{\code{signature(object = "numeric", tract =
		"Tract")}: ... }
	
    \item{show}{\code{signature(object = "Tract")}: Show/print the object }
	
    \item{summary}{\code{signature(object = "Tract")}: Object summary }
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
\code{\linkS4class{bufferedTract}}
}

\examples{
showClass("Tract")
}
\keyword{classes}
