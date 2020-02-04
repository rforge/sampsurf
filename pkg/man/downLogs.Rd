\name{downLogs}
\alias{downLogs}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Generate Objects of Class "\code{\linkS4class{downLogs}}"
}

\description{  
This generic function has methods based on the signature formal
arguments \code{object} and \code{container}. It is used as a
constructor function for objects that are of class
"\code{\linkS4class{downLogs}}". This method should be used in preference
to \code{\link{new}} to insure a valid object.
}

\usage{
downLogs(object, container, ...)
}

\arguments{
  \item{object}{ Signature formal argument to key on whether logs are
	passed or generated.}
  
  \item{container}{ Relevant if the logs are to be contained within some
	physical boundary/area like a \dQuote{\code{\linkS4class{Tract}}}
	object.}
  
  \item{\dots}{ Formal arguments that are different for each method, see
	those for details. }
}

\details{
The methods that can be used to generate objects of class
\dQuote{\code{\linkS4class{downLogs}}} are detailed here:
\code{\link{downLogs-methods}}. As mentioned, each has a different
signature and supporting formal arguments allowing you to generate a
collection in various ways. Please see the above link for more details.
}

\value{
A valid object of class "\code{\linkS4class{downLogs}}"
}

\references{
  \dQuote{The Stem Class} vignette in this package.
}

\author{
Jeffrey H. Gove
}


\seealso{
  \code{\link{downLog}}, "\code{\linkS4class{downLog}}", "\code{\linkS4class{StemContainer}}"
}

\examples{
showMethods("downLogs")
dlogs = downLogs(15, xlim=c(0,20), ylim=c(10,40), buttDiams=c(10,35))
summary(dlogs)
plot(dlogs, axes=TRUE)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{ ~kwd1 }
%% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
