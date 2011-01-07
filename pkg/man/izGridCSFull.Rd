\name{izGridCSFull}
\alias{izGridCSFull}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Generate Objects of Class "\code{\linkS4class{csFullInclusionZoneGrid}}"
}

\description{ This is the generic definition for generating objects of
class "csFullInclusionZoneGrid." There is one method corresponding to
this generic that may be found in \code{\link{izGridCSFull-methods}}. Most
users will not require the use of this method, it is a helper method in
that it is necessary to build the final sampling surface. More details
and examples are found in \emph{"The InclusionZoneGrid Class"} vignette.
}

\usage{
izGridCSFull(izGridSausage, tract, ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{izGridSausage}{ Signature object of a class
	"\code{\linkS4class{InclusionZoneGrid}}" with \code{iz} slot
	containing an object of class "\code{\linkS4class{sausageIZ}}". }
  
  \item{tract}{ Signature object of class "\code{\linkS4class{Tract}}".}

  \item{\dots}{ See methods.}

}

\details{ The method for this constructor may take quite a while to run
  because it has to apply the chainsaw method to every grid cell within the overall log sausage
  inclusion zone. For populations of many logs under this (i.e.,
  developing a sampling surface of many logs), one has to allow for the
  time to create these objects. Once created, visual display in either
  two- or three-dimensions is often interesting.
}

\value{
A valid object of class "\code{\linkS4class{csFullInclusionZoneGrid}}."
}


\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}


\seealso{
\code{\linkS4class{InclusionZoneGrid}}, \code{\linkS4class{InclusionZone}}, \code{\linkS4class{Stem}},
\code{\linkS4class{Tract}}, \code{\linkS4class{sampSurf}}
}

\examples{
\dontrun{
tr = Tract(c(x=20,y=20), cellSize=0.5)
btr = bufferedTract(5, tr)
dlogs = downLogs(1, btr@bufferRect, logLen=c(4,6))
iz.sa = sausageIZ(dlogs@logs$log.1, 2)
izg.sa = izGrid(iz.sa, btr)
izgFCS = izGridCSFull(izg.sa, btr)
plot(izgFCS, axes=TRUE)
summary(izgFCS)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
