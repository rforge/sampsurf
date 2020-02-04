\name{lineSegment}
\alias{lineSegment}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
  Generate Objects of Class "\code{\linkS4class{lineSegment}}"
}

\description{This generic function has only one method
(\code{\link{lineSegment-methods}}) used as a constructor function for
objects that are of class "\code{\linkS4class{lineSegment}}". This
method should be used in preference to \code{\link{new}} to insure a
valid object.  }

\usage{
lineSegment(length, orientation, ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{length}{ The length of the line segment in the appropriate units
	(feet for "English" or meters for "metric").
}

  \item{orientation}{ The line orientation clockwise from \emph{north} as an
	azimuth in degrees.
}

  \item{\dots}{  Arguments that are defined in \code{\link{lineSegment-methods}}
}

}

\details{
Only one method currently exists for object generation. Its arguments
are documented in \code{\link{lineSegment-methods}}.
}

\value{
A valid object of class "\code{\linkS4class{lineSegment}}"
}

%\references{
%% ~put references to the literature/web site here ~
%}

\author{
Jeffrey H. Gove 
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{lineSegment-methods}}
}

\examples{
# a one chain segment...
ls = lineSegment(length=66, orientation=45, centerPoint=c(x=100, y=80), units='English')
summary(ls)
plot(ls, showLineCenter=TRUE, cex=2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{ ~kwd1 }
%% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
