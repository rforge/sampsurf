\name{getID}
\alias{getID}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{ Retrieve Object  IDs }

\description{ This generic's methods will get the ID that is used to
  identify the object. Sometimes this can be embedded down within the
  object's spatial slot components, so this function just makes it
  simpler to find this information.
}

\usage{
getID(object, ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  
  \item{object}{ An object for a class supported by the methods.
}

  \item{\dots}{ Not used at present. }

}


\details{
  Currently, methods exist for objects of classes described in \code{\link{getID-methods}}.
}

\value{  The return is a vector of IDs for the object (e.g., container classes
  will normally have multiple \dQuote{Stem} objects).
}

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}

\seealso{"\code{\linkS4class{downLog}}",
  "\code{\linkS4class{downLogs}}", "\code{\linkS4class{standingTree}}",
  and "\code{\linkS4class{standingTrees}}" class definitions.
}

\examples{
st = standingTree()
getID(st)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{ ~kwd1 }
%% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
