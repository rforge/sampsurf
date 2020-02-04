\name{bboxSum}
\alias{bboxSum}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{ Function to Compute an Overall Bounding Box (bbox) }

\description{ Takes an array of \code{\link{bbox}} matrices and forms the overall
  encapsulating bounding box from these. }



\usage{
bboxSum(arr.bbox, ...)
}
\arguments{
  \item{arr.bbox}{ This is a 3 dimensional array of \code{bbox} matrices with the rows and
	columns of each matrix as the first two dimensions, and each matrix
	indexed by the 3rd dimension of the array. Note that each
	\code{bbox} will be checked for validity such that row names must be
	\code{c("x", "y")} and column names must be \code{c("min",
	  "max")}. Usually it is most convenient to use the
	\code{\link{bbox}} method for retrieving valid individual
	\code{bbox} matrices to put into the array.
  }
  
  \item{\dots}{ Not presently used.
}
}

\details{ The details are all presented above.
}

\value{
  A valid \code{bbox} matrix for the minimal bounding box of the set.
}

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{bbox}}, \code{\link{bboxToPoly}}, \code{\link{bboxCheck}}
}
\examples{
dlogs = downLogs(3)
bboxArray = array(dim=c(2,2,3))
bboxArray[,,1] = bbox(perimeter(dlogs@logs$log.1))
bboxArray[,,2] = bbox(perimeter(dlogs@logs$log.2))
bboxArray[,,3] = bbox(perimeter(dlogs@logs$log.3))
dimnames(bboxArray) = dimnames(bbox(dlogs@logs$log.1)) #page dim doesn't matter
bbox = bboxSum(bboxArray)    
\dontrun{
plot(dlogs, axes=TRUE)
plot(bboxToPoly(bbox), add=TRUE)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{ ~kwd1 }
%% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
