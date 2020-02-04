\name{standingTrees}
\alias{standingTrees}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Generate Objects of Class "\code{\linkS4class{standingTrees}}"
}

\description{
This generic function has methods based on the signature formal
arguments \code{object} and \code{container}. It is used as a
constructor function for objects that are of class
"\code{\linkS4class{standingTrees}}". This method should be used in preference
to \code{\link{new}} to insure a valid object.
}

\usage{
standingTrees(object, container, ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{ Signature formal argument to key on whether trees are
	passed or generated. }

  \item{container}{ Relevant if the trees are to be contained within some
	physical boundary/area like a \dQuote{\code{\linkS4class{Tract}}}
	object. }

  \item{\dots}{ Formal arguments that are different for each method, see
	those for details. }
}

\details{
The methods that can be used to generate objects of class
\dQuote{\code{\linkS4class{standingTrees}}} are detailed here:
\code{\link{standingTrees-methods}}. As mentioned, each has a different
signature and supporting formal arguments allowing you to generate a
collection in various ways. Please see the above link for more details.
}

\value{
A valid object of class "\code{\linkS4class{standingTrees}}."
}

\references{
  \dQuote{The Stem Class} vignette in this package.
}

\author{
Jeffrey H. Gove
}


\seealso{
\code{\link{standingTree}}, and classes: "\code{\linkS4class{standingTree}}", "\code{\linkS4class{StemContainer}}"
}

\examples{
showMethods("standingTrees")
strees = standingTrees(15, xlim=c(0,20), ylim=c(10,40), dbhs=c(10,25))
summary(strees)
plot(strees, axes=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{ ~kwd1 }
%% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
