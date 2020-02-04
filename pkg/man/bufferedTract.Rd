\name{bufferedTract}
\alias{bufferedTract}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Generate Objects of Class "\code{\linkS4class{bufferedTract}}"
}

\description{
This creates a "\code{\linkS4class{bufferedTract}}" object with an internal
buffer. There is only one method for object construction at present.
}

\usage{
bufferedTract(bufferWidth, tract, ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bufferWidth}{ Signature argument: buffer width.}
  
  \item{tract}{ Signature argument: a "Tract" object. }
  
  \item{\dots}{ See methods for details.}
}

\details{ This generic is defined completely with respect to the signature of its
method in \code{\link{bufferedTract-methods}}, which should be consulted for details.}


\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A valid object of class "\code{\linkS4class{bufferedTract}}"
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
\code{\link{bufferedTract-methods}}, \code{\link{Tract-methods}}, \code{\link{Tract}}
}

\examples{
tract = Tract(c(x=20,y=20), cellSize=0.25) 
buffTr = bufferedTract(4, tract)
summary(buffTr)
buffTr
plot(buffTr, axes=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{ ~kwd1 }
%% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
