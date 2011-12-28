\name{downLogs-class}
\Rdversion{1.1}
\docType{class}
\alias{downLogs-class}
\alias{hist,downLogs-method}
%\alias{bbox,downLogs-method}
%\alias{perimeter,downLogs-method}
%\alias{plot,downLogs,missing-method}
%\alias{show,downLogs-method}
\alias{summary,downLogs-method}

\title{Class "downLogs"}

\description{ The \dQuote{downLogs} class is a simplified container
class that can hold multiple objects of class
\dQuote{\code{\linkS4class{downLog}}}. Its specific purpose is to hold a
population of down logs that are either generated synthetically as part
of a simulation, or a collection from field measurements. The
constructor of the same name has several different forms corresponding
to possible argument signatures.  }

\section{Objects from the Class}{ Objects can be created by calls of the
form \code{new("downLogs", ...)}; however, as in the other classes
within this package, constructors have been written to simplify the
process. The \code{\link{downLogs}} constructor should therefore be used
in preference to \code{new}.  }

\section{Slots}{

  Please see the virtual base class,
  \dQuote{\code{\linkS4class{StemContainer}}}, for additional slots
  definitions. 
  
  \describe{
    \item{\code{logs}:}{Object of class \code{"list"}: This holds the
      collection of \dQuote{\code{\link{downLog}}} objects.  }
	
  }

Please note that at the present time this class only partially meets the requirements of a
true \dQuote{container class} in object oriented programming. This is because it does not as
yet have methods for object deletion, editing, or addition to the list of down logs. Because the
statistics and bounding box are tied to the collection, a caution is in order regarding
changing in any way the objects within your \R code. The best way to handle this is to
simply extract the list from the object, do whatever editing has to be done to it, then
use the constructor below to make a new object. Then everything will be correctly
represented within the object. 

}

\section{Extends}{
Class \code{"\linkS4class{StemContainer}"}, directly.
}

\section{Methods}{
  \describe{
	
    \item{hist}{\code{signature(x = "downLogs")}: Displays a histogram
      of different variables in the collection. }
	
    \item{plot}{\code{signature(x = "downLogs", y = "missing")}: Plot
      the collection. }
	
    \item{summary}{\code{signature(object = "downLogs")}: Same as
      \code{show} currently. }
	 }
}


\author{
Jeffrey H. Gove
}

\seealso{
\code{\link{sampleLogs}},  "\code{\linkS4class{downLog}}", "\code{\linkS4class{Stem}}",
"\code{\linkS4class{StemContainer}}"
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
