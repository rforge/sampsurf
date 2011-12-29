\name{StemContainer-class}
\Rdversion{1.1}
\docType{class}
\alias{StemContainer-class}
\alias{bbox,StemContainer-method}
%\alias{perimeter,StemContainer-method}
%\alias{plot,StemContainer,missing-method}
\alias{show,StemContainer-method}
\alias{summary,StemContainer-method}

\title{Class \code{"StemContainer"} for collections of tree and log objects}

\description{ This is a virtual class that provides the minimal shared
  structure for the actual usable subclasses such as
  \dQuote{standingTrees} and \dQuote{downLogs}. In general, the idea for
  container objects is similar to what is found in some other languages
  such as \code{C++} or \code{Java}. The container object just hold a
  collection of objects of the appropriate subclass, and has methods
  that work on this collection as a whole, like plotting the collection.
  It is a simplified view of traditional container classes in that it
  does not currently allow for deletion or addition of objects.}

\section{Objects from the Class}{A virtual Class: No objects may be
  created from it.}

\section{Slots}{
  \describe{
    \item{\code{units}:}{Object of class \code{"character"}:  The units of
	  measure used, must be either "English" or "metric".  }
	
    \item{\code{bbox}:}{Object of class \code{"matrix"}: The overall
	  bounding box extents for the collection. }
	
    \item{\code{stats}:}{Object of class \code{"data.frame"}: A summary of
      simple statistics on the collection; e.g., the mean and variance
      for volume etc. }
	
    \item{\code{description}:}{Object of class \code{"character"}: A
      helpful comment about the collection. }
  }
}

\section{Methods}{
  \describe{
    \item{bbox}{\code{signature(obj = "StemContainer")}: Calculates the
	  overall bounding box matrix.}
	
    \item{perimeter}{\code{signature(object = "StemContainer")}: Extract
      the perimeter of the collection corresponding to the bounding box
      as a "\code{\link{SpatialPolygons}}" object for possible
      plotting.}
	
    \item{plot}{\code{signature(x = "StemContainer", y = "missing")}:
      Plots the collection. }
	
    \item{show}{\code{signature(object = "StemContainer")}: Show
      component summary for the base slots in the collection. }
	
    \item{summary}{\code{signature(object = "StemContainer")}: Same as show. }
	 }
}
   

\references{
  \dQuote{The Stem Class} vignette in this package.
}

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Subclasses: \code{\linkS4class{downLogs}} and  \code{\linkS4class{standingTrees}}.
}

\examples{
showClass("StemContainer")
}
\keyword{classes}
