\name{getProxy}
\alias{getProxy}
\alias{cmcProxy}
\alias{gvProxy}
\alias{wbProxy}
%- Also NEED an '\alias' for EACH other topic documented here.


\title{ Proxy functions for Monte Carlo sampling methods in
  \pkg{sampSurf} }

\description{
  These functions encapsulate the proxy capabilities under Monte Carlo
  sampling within "\code{\linkS4class{Stem}}" subclass objects for the
  estimation of volume.
}

\usage{
getProxy(proxy = c("cmcProxy", "gvProxy", "wbProxy"), ...) 

cmcProxy(stem, u.s, segBnds, ...)

gvProxy(stem, u.s, segBnds, ...)

wbProxy(stem, u.s, segBnds, solidTypeProxy = 3, truncateProxyStem = TRUE,
        wbProxySolve = c('uniroot', 'nlminb'), warningsOn = FALSE, ...)

}


%- maybe also 'usage' for other objects documented here.
\arguments{
  
  \item{proxy}{ A character argument specifying the name of the proxy function to
	retrieve. }

  \item{stem}{ An object that is a subclass of
    "\code{\linkS4class{Stem}}", i.e., a "\code{\linkS4class{downLog}}"
    or "\code{\linkS4class{standingTree}}" object. }

  \item{u.s}{ The uniform (0,1) random number vector for Monte Carlo sampling
    to determine the sampled heights for the respective method. }

  \item{segBnds}{ A vector of length two giving the lower and upper
    height/length bounds for volume estimation within the bole.  These
    bounds correspond to the limits of integration along the bole. }

  \item{solidTypeProxy}{ The applicable values for this and what they do
    depend on whether the default \pkg{sampSurf} taper equation was used
    to construct the \code{stem} object passed. In general, specifying
    \code{NA} will use the exact taper for the \code{stem}, so there
    should be no estimation error. Otherwise, this specifies a value for
    the shape parameter to be used in the proxy function in the default
    taper system. Specifying a value between zero and one will yield a
    proxy stem with shape parameter that is this proportion of the true
    shape parameter for the \code{stem} passed (i.e., a value of 0.9
    gives a proxy that is pretty close to the actual \code{stem}). If
    user-defined taper model was used to construct \code{stem}, then
    this this range is not applicable and the default vaule of 3 is
    used. This has been a very terse explanation of this argument, please
    consult the vignette refernece below for details on its use. }

  \item{truncateProxyStem}{ \code{TRUE} enlarge the tip diameter if it
    is zero so inflation of the estimate is not a concern if
    cross-sectional areas close to zero are sampled in importance
    sampling. \code{FALSE} the proxy will taper to the tip if the
    \code{stem} passed does. Again, more details are given in the
    vignette. }

  \item{wbProxySolve}{ The two options specify which method is used to
    numerically solve for the proxy heights. The default should normally
    be used, the \code{'nlminb'} option is slow. }

  \item{warningsOn}{ \code{TRUE}: print warnings if necessary;
  \code{FALSE}: runs silently (default).} 

  \item{\dots}{ Used to ignore extraneious arguments passed at present. }
}

\details{

  The first function, \code{getProxy}, returns one of the built-in proxy
  functions from the \pkg{sampSurf} namespace so that it can be used
  within the respective Monte Carlo sampling method. In the case of a
  user proxy, it will also look for the proxy within the user's
  workspace. The function does a number of checks for the minimal format
  of \dQuote{proxy} functions that are described in the vignette
  reference below. Therefore, it will catch problems with arguments and
  return lists in user-defined proxies. This is necessary to make sure
  all user-written proxy functions accept and return the standard set of
  arguments and list components.

  The built-in proxy functions are discussed in detail in the
  vignette. The built-in proxies range from very simple to fairly
  complex. They include\ldots

  \code{cmcProxy}: This is the default proxy used for crude Monte Carlo
  sampling in the \code{\link{crudeMonteCarlo}} object constructor. It
  is also used in control variate sampling in the
  \code{\link{controlVariate}} constructor for selecting the
  height/length sample points.

  \code{gvProxy}: This is a simple proxy that is proportional to
  cross-sectional area at a given sampled height. It is the default
  proxy used in \code{\link{importanceSampling}} and
  \code{\link{controlVariate}} (as the proxy cross-sectional area model)
  methods for creation of their respective objects. Details on its uses
  and potential concerns are found in the vignette.

  \code{wbProxy}: This proxy uses the default taper equation in
  \pkg{sampSurf}. This is a much more complicated proxy than the other
  two, and can take more arguments. It can be used in both \code{\link{importanceSampling}} and
  \code{\link{controlVariate}} methods for determination of the sample
  heights and cross-sectional areas. Please see the vignette below for
  details on this function and its use.

  
}

\value{
  
  \code{getProxy} returns the proxy function. The other functions return a
  list of the form\ldots

  \item{g}{ The function (closure) that actually determines the
    cross-sectional area at a given height.}

  \item{G}{ The volume integral for the stem segment. }

  \item{hgt.s}{ The sampled heights along the bole segment at which
    \code{g} will be evaluated. }

  Please see the vignette for more details.
}

\references{
\describe{
%
\item{}{Gove, J. H. 2013. Monte Carlo sampling methods in
  \pkg{sampSurf}. Package vignette.}
}
}

\author{
  Jeffrey H. Gove %, \email{jhgove@unh.edu}
}


\seealso{
  \code{\linkS4class{MonteCarloSampling}},
  \code{\linkS4class{crudeMonteCarlo}},
  \code{\linkS4class{importanceSampling}},
  \code{\linkS4class{controlVariate}},
  \code{\linkS4class{antitheticSampling}}. 
}

\examples{
#
# retrieve a built-in proxy and use it...
#
sTree = standingTree(dbh = 40, topDiam = 0, height = 20, solidType = 2.8)
cmcFun = getProxy('cmcProxy')
( cmcFun(sTree, runif(4), c(0, sTree@height)) )
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{ ~kwd1 }
%% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
