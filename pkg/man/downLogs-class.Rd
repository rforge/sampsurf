\name{downLogs-class}
\Rdversion{1.1}
\docType{class}
\alias{downLogs-class}
\alias{bbox,downLogs-method}
%\alias{perimeter,downLogs-method}
%\alias{plot,downLogs,missing-method}
\alias{show,downLogs-method}
\alias{summary,downLogs-method}

\title{Class "downLogs"}
\description{
  
To borrow a phrase from other object oriented languages, the
\dQuote{downLogs} class is a simplified container class that can contain multiple
objects of class \dQuote{\code{\linkS4class{downLog}}}. Its specific
purpose is to hold a population of down logs that are either generated
synthetically as part of a simulation, or a collection from field
measurements. The constructor of the same name has several different
forms corresponding to possible argument signatures. It is a simplified
view of traditional container classes in that is does not currently allow for
deletion or addition of objects.
}

\section{Objects from the Class}{
Objects can be created by calls of the form
\code{new("downLogs", ...)}; however, as in the other classes within
this package, constructors have been written to simplify the
process. The \code{\link{downLogs}} constructor should therefore be used
in preference to \code{new}. 
}

\section{Slots}{
  \describe{
    \item{\code{logs}:}{Object of class \code{"list"}: This holds the
      collection of \dQuote{\code{\link{downLog}}} objects.  }
	
    \item{\code{units}:}{Object of class \code{"character"}: The units
      for all of the logs in the collection; note that they must all be
      measured in the same units. }
	
    \item{\code{bbox}:}{Object of class \code{"matrix"}: The overall
      bounding box for the collection used in plotting. }
	
    \item{\code{stats}:}{Object of class \code{"data.frame"}: A summary of
      simple statistics on the collection; e.g., the mean and variance
      for volume and length. }
  }

Please note that at the present time this class only partially meets the requirements of a
true \dQuote{container class} in object oriented programming. This is because it does not as
yet have methods for object deletion, editing, or addition to the list of down logs. Because the
statistics and bounding box are tied to he collection, a caution is in order regarding
changing in any way the objects within your \R code. The best way to handle this is to
simply extract the list from the object, do whatever editing has to be done to it, then
use the constructor below to make a new object. Then everything will be correctly
represented within the object. Eventually I'll get to routines to do the editing, but it
is low on the priority list right now.

}



\section{Methods}{
  \describe{
    \item{bbox}{\code{signature(obj = "downLogs")}: Extract the bounding
      box}
    \item{perimeter}{\code{signature(object = "downLogs")}: Extract the
      perimeter of the collection corresponding to the bounding box as a
      "\code{\link{SpatialPolygons}}" object for possible plotting. }
    \item{plot}{\code{signature(x = "downLogs", y = "missing")}: Plot
      the collection. }
    \item{show}{\code{signature(object = "downLogs")}: Show some
      information about the object. }
    \item{summary}{\code{signature(object = "downLogs")}: Same as
      \code{show} currently. }
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
\code{\link{sampleLogs}},  "\code{\linkS4class{downLog}}", "\code{\linkS4class{Stem}}"

}

\examples{
showClass("downLogs")
buff = matrix(c(0,100,0,100), nrow=2, byrow=TRUE,
              dimnames=list(c('x','y'),c('min','max')))
sl = sampleLogs(10, buttDiam = c(10,25), sampleRect = buff)
dls = downLogs(sl)
summary(dls)
}
\keyword{classes}
