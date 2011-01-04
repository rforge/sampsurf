\name{Tract}
\alias{Tract}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate Objects of Class "\code{\linkS4class{Tract}}"
}

\description{ This generic function has methods based on the signature
formal argument \code{object}. It is used as a constructor function for
objects that are of class "\code{\linkS4class{Tract}}". This method
should be used in preference to \code{\link{new}} to insure a valid
object.}

\usage{
Tract(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{ This is the signature formal argument, see the methods
	for more details.}
  \item{\dots}{ Formal arguments that are different for each method, see
	those for details. }
}

\details{There is no default generic function for \code{Tract} per
se. It is defined completely with respect to the signature of its
methods \code{\link{Tract-methods}}. Methods are available for several
useful signatures, such as objects of class
\code{"\linkS4class{RasterLayer}"}, allowing for a large variety of ways
to generate objects of this class. Other methods can obviously be added
for other signatures as necessary.  }

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A valid object of class "\code{\linkS4class{Tract}}."
}

%\references{
%% ~put references to the literature/web site here ~
%}

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{Tract-methods}}
}
\examples{
tract = Tract(c(x=20,y=20), cellSize=0.25)    #extents constructor
tr = Tract(,0.5, c(x=20,y=20),c(0.25,0.25),units='metric') #missing constructor
tr2 = Tract(cellSize=0.5, cellDims=c(x=20,y=20), cellCenter=c(0.25,0.25),
            units='metric') #same constructor as the previous
tr.ext = Tract(c(x=10,y=15), cellSize=0.5) #maximal extent constructor
#bounding box constructor...
bb = bbox(tr.ext)
bb[,1] = c(5,4)
tr3 = Tract(bb, 0.5)
#RasterLayer constructor...
ex = extent(tr3)
rl = raster(ex, nrow=10, ncols=10, crs=NA)
\dontrun{
tr4 = Tract(rl)  #error: non-square grid cells
}
rl = raster(ex, nrow=22, ncol=10, crs=NA)
tr4 = Tract(rl)  #okay, square cells
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
