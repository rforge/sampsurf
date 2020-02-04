\name{izContainer}
\alias{izContainer}
\alias{izContainer,list-method}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
  Common Setup for Subclass Object Creation
}

\description{
  This function simply does some background work that each of
  "\code{\linkS4class{downLogIZs}}" and
  "\code{\linkS4class{standingTreeIZs}}" subclasses have in common for
  object creation. It is \emph{not} meant to be called as a general
  function by the casual user.
}

\usage{
izContainer(object, ...)
}

\arguments{
  \item{object}{ the one signature object, a \code{list}. }
  \item{\dots}{ Just gobbled for now. }
}

\details{ See the returned value below. Note that this is pre-object
  creation, so it does not make sense to have a \code{bbox} method to
  calculate the overall bounding box. A \code{bbox} method based on a
  \code{list} object is also out of the question because the list could
  contain anything; in this routine the list passed must only contain
  valid objects of class "\code{\linkS4class{InclusionZone}}" (i.e., one
  of its subclasses).  }

\value{
Currently returns just the overall bounding box of all the objects in
the \code{list}. However, it may acquire more functionality later on.
}

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}

\seealso{"\code{\linkS4class{izContainer}}","\code{\link{downLogIZs}}", "\code{\link{standingTreeIZs}}"
}

%\examples{
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{ ~kwd1 }
%% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
