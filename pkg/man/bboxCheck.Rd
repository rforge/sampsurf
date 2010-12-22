\name{bboxCheck}
\alias{bboxCheck}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{ Function to Check Spatial Bounding Boxes }

\description{ A \code{bbox} is not a formal class, but is used often
  enough in package \pkg{sampSurf} that a little routine to check for a
  valid one is useful. It could have been made a class, but it was not
  in the more basic \pkg{sp} and \pkg{raster} packages, and so was not
  introduced here.
}

\usage{
bboxCheck(bbox)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bbox}{ A matrix to be checked.}
}

\details{
  A valid \code{bbox} matrix is \code{2x2}, all numeric, and has row
  names \code{c('x','y')} and column names \code{c('min','max')} in
  package \pkg{sampSurf}. A check is also made for minima-maxima extents
  in the correct sense. 
}

\value{ The function returns true if the matrix passes all the checks,
  otherwise it stops with an error.
}

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}



\seealso{ \code{\link{bbox}} }

\examples{
tract = Tract(c(x=20,y=20), cellSize=0.5)
bb = bbox(tract)
bboxCheck(bb)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
