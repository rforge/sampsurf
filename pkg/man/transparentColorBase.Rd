\name{transparentColorBase}
\alias{transparentColorBase}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{Set Transparancey in Base Graphics}

\description{Setting transparency in base graphics is not as easy as in
  \code{Lattice}, so here's a little functon to help.}

\usage{
transparentColorBase(color, alphaTrans = alphaTrans)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{color}{The color, or a vector of colors from
	\code{\link{colors}()}.}
  
  \item{alphaTrans}{The alpha transparency value between [0,1] with 0
	opaque and 1 fully transparent.
}
}

\details{As above.}

\value{The rgb value(s), which can be passed to any base graphics
  routine to get transparency.}

\author{Jeffrey H. Gove}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{col2rgb}}, \code{\link{rgb}} }
\examples{
\dontrun{
cols = transparentColorBase('red', alphaTrans=c(0.3,0.6,0.9))
symbols(c(1,1.5,2), c(1,1.5,2), circles=rep(1,3), bg=cols, xlim=c(0,4), ylim=c(0,4))
}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
