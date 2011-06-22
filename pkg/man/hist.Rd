\name{hist}
\alias{hist}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{ Histogram of Various Object Attributes in Package \pkg{sampSurf} }

\description{ Several different classes have methods for generating
  histograms of one or more of their class attributes. The different
  methods are described in \code{\link{hist-methods}}. They all extend
  \R's graphics::\code{\link[graphics]{hist}} generic, and so allow all of the arguments
  that it does. In addition, some of the methods have other new
  arguments as well.  }

%
% the \usage below will create a warning in running a check on the
% package because the generic is not here, but in base, so it says there
% is a usage in Rd that is not in the code. I tried a number of things,
% but settled on leaving it for now...
%
%\usage{
%hist(x, ...)
%}
%- maybe also 'usage' for other objects documented here.
%\arguments{
%  \item{x}{ Signature object, which differs for each method.}
%  \item{\dots}{ See methods and graphics::\code{\link[graphics]{hist}} for details.}
%}

\details{ The different attributes of each class that are plotted are
  described in \code{\link{hist-methods}}. Not all classes have a
  \code{hist} method, so it is also advisable to check which ones do,
  and what extra arguments might be available. For example, one can
  render a distribution of any number of different components of
  \code{\linkS4class{downLog}} attributes in a
  \code{\linkS4class{downLogs}} container collection. 
}

\value{
  The same as is returned in the graphics::\code{\link[graphics]{hist}} generic.
}

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}

\seealso{
 See methods and graphics::\code{\link[graphics]{hist}} for details.
}
\examples{
dlogs = downLogs(20)
hist(dlogs, logAttr='logVol')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
