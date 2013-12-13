\name{izGrid}
\alias{izGrid}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Generate Objects of Class "\code{\linkS4class{InclusionZoneGrid}}"
}

\description{ This is the generic definition for generating objects of
class "InclusionZoneGrid." There are several methods corresponding to
this generic that may be found in \code{\link{izGrid-methods}}. Most
users will not require the use of these methods in a primary use sense,
they are more helper methods in that they are necessary to build the
final sampling surface. More details and examples are found in
\emph{"The InclusionZoneGrid Class"} vignette.  }

\usage{
izGrid(izObject, tract, ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{izObject}{ Signature object of a subclass of
	"\code{\linkS4class{InclusionZone}}".}

  \item{tract}{ Signature object of class "\code{\linkS4class{Tract}}".}

  \item{\dots}{ See methods.}

}

\details{ There are any number of details concerning this class that
are described in the vignette mentioned above. It provides the basic
idea behind the class and the use of the methods for each specific
sampling protocol. Because these methods will not necessarily be useful
to the casual user, we leave the details to that document.  }

\value{
A valid object of class "\code{\linkS4class{InclusionZoneGrid}}."
}


\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}


\seealso{
\code{\linkS4class{InclusionZone}}, \code{\linkS4class{Stem}},
\code{\linkS4class{Tract}}, \code{\linkS4class{sampSurf}},
\code{\link{izGridConstruct}}, \code{\link{izGrid-methods}}
}

\examples{
tr = Tract(c(x=20,y=20), cellSize=0.5)
btr = bufferedTract(5, tr)
dlogs = downLogs(1, btr@bufferRect)
sup = standUpIZ(dlogs@logs$log.1, 2)
izgSU = izGrid(sup, btr)
plot(izgSU, axes=TRUE)
izgSU
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
