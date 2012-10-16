\name{circularPlot-class}
\Rdversion{1.1}
\docType{class}
\alias{circularPlot-class}
\alias{bbox,circularPlot-method}
%\alias{perimeter,circularPlot-method}
%\alias{plot,circularPlot,missing-method}
\alias{summary,circularPlot-method}

\title{Class "circularPlot": Fixed-area Circular Plots}

\description{
A subclass of "\code{\linkS4class{ArealSampling}}" that can be used to
create fixed-area circular plot objects for use in sampling surface
simulations. 
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("circularPlot",
  ...)}; however, this is not recommended due to the necessity to have
the spatial representation correct. Preferably, one should use the
\code{\link{circularPlot}} constructor for this class.
}

\section{Slots}{
  In addition to those slots in class "ArealSampling," the following are defined\ldots
  \describe{
	
    \item{\code{radius}:}{Object of class \code{"numeric"}: The
      fixed-plot radius in the correct units. For "English" units, this
      would be feet, for "metric" units, meters.}
	
    \item{\code{area}:}{Object of class \code{"numeric"}: The area of
      the plot in the correct units.  }
	
    \item{\code{perimeter}:}{Object of class \code{"SpatialPolygons"}:
      The \code{"SpatialPolygons"} object corresponding to the perimeter of the
      fixed-radius plot.}
	
    \item{\code{location}:}{Object of class \code{"SpatialPoints"}: This
      is a \dQuote{SpatialPoints} representation of the location of the
      object. In the "circularPlot" class, this is the fixed-radius plot
      center. It will often correspond to the location slot in the
      "\code{\linkS4class{Stem}}" object under sampling surface
      simulations. But there are exceptions: for example, under the
      \sQuote{standup} method, it will be at the large-end of the log,
      while under the \sQuote{chainsaw} method, it will be some point
      within the \dQuote{sausage} shaped inclusion zone for protocol 1
      in (Gove and Van Deusen, 2011).  }

    \item{\code{spID}:}{Object of class \code{"character"}: A unique
      identifier that is used in the \dQuote{SpatialPolygons}
      representation of the object.  }
	
    \item{\code{spUnits}:}{Object of class \code{"CRS"}: A valid string
      of class \dQuote{CRS} denoting the spatial units coordinate system (?CRS
      for more information) as in package \pkg{sp}.  }

  }
}

\section{Extends}{
Class \code{"\linkS4class{ArealSampling}"}, directly.
}

\section{Methods}{
  \describe{
    \item{bbox}{\code{signature(obj = "circularPlot")}: Return the
      bounding box. }
    \item{perimeter}{\code{signature(object = "circularPlot")}: Return
      the perimeter object. }
    \item{plot}{\code{signature(x = "circularPlot", y = "missing")}:
      Graphical display of the object. }
    \item{summary}{\code{signature(object = "circularPlot")}: A summary
      of the object. }
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
  The "\code{\linkS4class{ArealSampling}}" class.
}
\examples{
showClass("circularPlot")
}
\keyword{classes}
