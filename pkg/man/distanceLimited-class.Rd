\name{distanceLimited-class}
\Rdversion{1.1}
\docType{class}
\alias{distanceLimited-class}
%\alias{distanceLimitedMCIZ,downLog,distanceLimited-method}
\alias{summary,distanceLimited-method}

\title{Class \code{"distanceLimited"}: Distance Limited Sampling}

\description{A subclass of "\code{\linkS4class{ArealSampling}}" that can be used to
create objects that encapsulate all the parameters necessary for any
variant of distance limited sampling of down woody debris.
}

\section{Objects from the Class}{ Objects can be created by calls of the
form \code{new("distanceLimited", ...)}, and while this is reasonable
with this class because there are no graphical slots, it is still not
recommended. The preferred method for creating new objects is via the
\code{\link{distanceLimited}} constructor.  }

\section{Slots}{
  In addition to those slots in class "ArealSampling," the following are defined\ldots
  \describe{
    \item{\code{distanceLimit}:}{Object of class \code{"numeric"}: This
  is simply the distance limit in either feet or meters, depending upon
  the \code{units} chosen. }
  }
}

\section{Extends}{
Class \code{"\linkS4class{ArealSampling}"}, directly.
%Class \code{"\linkS4class{dlsNumeric}"}, directly.
}

\section{Methods}{
  \describe{
    %\item{distanceLimitedMCIZ}{\code{signature(downLog = "downLog", dls = "distanceLimited")}:  }
    \item{summary}{\code{signature(object = "distanceLimited")}: prints a
      summary of the object }
	 }
}

%\references{
%%  ~~put references to the literature/web site here~~
%}

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}

\seealso{
The "\code{\linkS4class{ArealSampling}}" class.
}
\examples{
showClass("distanceLimited")
}
\keyword{classes}
