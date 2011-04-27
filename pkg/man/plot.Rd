\name{plot}
\alias{plot}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{ Plot objects within package `sampSurf'}

\description{ Each different class of object in this package has a plot
  method associated with it that is based on \R's graphics::\code{plot}
  generic. These all have the same form for the first signature
  arguments as shown below, but many have different arguments beyond
  those normmaly found in \code{plot} to add some flexibility to the
  rendering of each  object. These are all given in
  \code{\link{plot-methods}}. 
}


%
% the \usage below will create a warning in running a check on the
% package because the generic is not here, but in base, so it says there
% is a usage in Rd that is not in the code. I tried a number of things,
% but settled on leaving it for now...
%
\usage{
plot(x, y, ...)
}

\arguments{
  \item{x}{  Signature object, which differs for each method.}
  \item{y}{  Signature object, which differs for each method.}
  \item{\dots}{  See methods.}
}

\details{ The different classes within the package are unique enough
  that there is not much functionality in common with regard to shared
  arguments. Two notable exceptions are that often the individual plot
  methods for a given object do not show axes and assume that a new plot
  is desired. Two common arguments that can be used to override these
  assumptions are \code{axes=TRUE} and \code{add=TRUE}. An example is
  shown below.
}

\value{
The methods do not, in general, return any values.
}

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}


\seealso{
  \code{\link{plot-methods}},  graphics::\code{\link[graphics]{plot}},
  raster::\code{\link[raster]{plot}}  
}

\examples{
tr = Tract(c(x=25,y=25), cellSize=0.5)
btr = bufferedTract(5, tr)
dlogs = downLogs(20, btr, buttDiams=c(25,35),logLens=c(3,10))
plot(btr, axes=TRUE, bufferColor='lightblue')
plot(dlogs, add=TRUE, showLogCenter=TRUE, logCenterColor='grey30', cex=2)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
