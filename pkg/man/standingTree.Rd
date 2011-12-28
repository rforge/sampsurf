\name{standingTree}
\alias{standingTree}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Generate Objects of Class "\code{\linkS4class{standingTree}}"
}

\description{ This generic function has methods based on the signature
formal argument \code{object}. It is used as a constructor function for
objects that are of class "\code{\linkS4class{standingTree}}". There are two
methods that should be used in preference to \code{\link{new}} to insure
a valid object.
}

\usage{
standingTree(object, ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{ This is the signature formal argument, see the methods
	for more details. }

  \item{\dots}{ Formal arguments that are different for each method, see
	those for details. }
}

\details{
\code{standingTree} is defined completely with respect to the
signature of its methods \code{\link{standingTree-methods}}. Methods
are available for \code{\link{data.frame}},  and \code{\link{missing}}
signatures.  Other methods can obviously be added for other signatures
as necessary.
}

\value{
A valid object of class "\code{\linkS4class{standingTree}}."
}

\references{
  \dQuote{The Stem Class} vignette in this package.
}

\author{
Jeffrey H. Gove
}


\seealso{
\code{\link{standingTree-methods}}
}

\examples{
#
#create a standingTree object and show it...
#
st = standingTree(dbh=20, solidType=3)
summary(st)
plot(st)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
