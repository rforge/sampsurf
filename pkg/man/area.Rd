\name{area}
\alias{area}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{ Area of objects in package \pkg{sampSurf} }

\description{ A number of the different classes of objects in
  \pkg{sampSurf} have some kind of areal representation, often just part
  of the class definition as one of the slots. This function simply
  returns the area as stored within the object as given in
  \code{\link{area-methods}}.  }

%
% the \usage below will create a warning in running a check on the
% package because the generic is not here, but in base, so it says there
% is a usage in Rd that is not in the code. I tried a number of things,
% but settled on leaving it for now...
%
%\usage{
%area(x, ...)
%}
%\arguments{
%  \item{x}{ Signature object, which differs with each method. }
%  \item{\dots}{ See methods. }
%}

\details{ The area that is stored and returned is in the native units of
  the object. In most cases, this will be either square feet or square
  meters. The \code{\link{area-methods}} provide individual detail, as
  do the individual class definitions for the objects passed.
}

\value{
Area in the correct units.
}

\author{
  Jeffrey H. Gove %, \email{jhgove@unh.edu}
}

\seealso{
\code{\link{area-methods}}
}
\examples{
tract = Tract(c(x=20,y=35),cellSize=0.25, units='metric')
area(tract)/.StemEnv$smpHectare  #in hectares
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{ ~kwd1 }
%% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
