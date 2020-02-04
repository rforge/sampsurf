\name{circularPlot}
\alias{circularPlot}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Generate Objects of Class "\code{\linkS4class{circularPlot}}"
}

\description{ This generic function has only one method
(\code{\link{circularPlot-methods}}) used as a constructor function for
objects that are of class "\code{\linkS4class{circularPlot}}". This
method should be used in preference to \code{\link{new}} to insure a
valid object. }

\usage{
circularPlot(radius, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  
  \item{radius}{ This is the signature argument: The fixed-plot
      radius in the correct units}
	
  \item{\dots}{ Arguments that are defined in \code{\link{circularPlot-methods}}
  }
  
}

\details{
Only one method currently exists for object generation. Its arguments
are documented in \code{\link{circularPlot-methods}}.
}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A valid object of class "\code{\linkS4class{circularPlot}}"
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
\code{\link{circularPlot-methods}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
cp = circularPlot(37.237, units='English', center=c(x=10,y=3))
summary(cp)
plot(cp, axes=TRUE, showPlotCenter=TRUE, cex=2.5)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{ ~kwd1 }
%% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
