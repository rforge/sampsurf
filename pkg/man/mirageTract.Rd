\name{mirageTract}
\alias{mirageTract}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Generate Objects of Class "\code{\linkS4class{mirageTract}}"
}

\description{
This generic will allow the construction of tract objects
within \pkg{sampSurf} that will use the mirage method for
boundary overlap correction  on any inclusion zones where
slopover is encountered. This constructor method should be used in
preference to \code{\link{new}} to insure a valid object.  }

\usage{
mirageTract(tract, ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tract}{ An object of class "\code{\linkS4class{Tract}}". }

  \item{\dots}{ Items to be gobbled for now. }

}

\details{
It is very simple to create an object of this class, all one requires is
an existing \dQuote{Tract} object as shown in the example below and the
methods: \code{\link{mirageTract-methods}}.
}

\value{
A valid object of class "\code{\linkS4class{mirageTract}}."
}

\references{
See the \dQuote{Mirage Method} vignette for more explanation.
}

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}


\seealso{
  \code{\link{Tract}}
}

\examples{
tract = Tract(c(x=20,y=20), cellSize=1)    #one-meter resolution
(mtract = mirageTract(tract))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{ ~kwd1 }
%% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
