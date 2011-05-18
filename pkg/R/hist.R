#---------------------------------------------------------------------------
#
#   These methods will plot a histogram of objects of different classes as
#   e.g.,
#
#   1. "izContainer" -- inclusion zone areas
#   2. "sampSurf" -- the surface estimate at each grid cell
#
#Author...									Date: 10-May-2011
#	Jeffrey H. Gove
#	USDA Forest Service
#	Northern Research Station
#	271 Mast Road
#	Durham, NH 03824
#	jhgove@unh.edu
#	phone: 603-868-7667	fax: 603-868-7604
#---------------------------------------------------------------------------
#



#================================================================================
#  1. method for class izContainer...
#
setMethod('hist',
          signature(x = 'izContainer'),
function(x,
         xlab = 'Inclusion Zone Area',
         main = NA,
         col = 'gray90',
         ...
        )
{
#------------------------------------------------------------------------------
#   this just plots the histo of inclusion zone areas...
#------------------------------------------------------------------------------
#

    izAreas = sapply(x@iZones, area)
    hg = hist(izAreas, xlab=xlab, main=main, col=col, ...)

    return(invisible(hg))

}    #plot for 'InclusionZone'
) #setMethod
     


#================================================================================
#  2. method for class sampSurf...
#
setMethod('hist',
          signature(x = 'sampSurf'),
function(x,
         zeroTrunc = TRUE,              #exclude zeros?
         xlab = x@estimate,
         main = NA,
         col = 'gray90',
         ...
        )
{
#------------------------------------------------------------------------------
#   this just plots the sampling distribution histogram w/ or w/o zeros
#------------------------------------------------------------------------------
#
    values = getValues(x@tract)
    if(zeroTrunc) 
      values = values[values>0]
    hg = hist(values, xlab=xlab, main=main, col=col, ...)
    if(zeroTrunc)
      cat('\nHistogram is zero-truncated:',count(x@tract, 0),'zeros excluded.\n')

    return(invisible(hg))

}    #plot for 'sampSurf'
) #setMethod
     


#================================================================================
#  3. method for class "downLogs" container...
#
setMethod('hist',
          signature(x = 'downLogs'),
function(x,
         logAttr = c('logLen','buttDiam','topDiam','logAngle','solidType','logVol',
                     'surfaceArea','coverageArea','biomass','carbon'),
         xlab = logAttr,
         main = NA,
         col = 'gray90',
         ...
        )
{
#------------------------------------------------------------------------------
#   this just plots one of the desired metrics for the logs...
#------------------------------------------------------------------------------
#
    logAttr = match.arg(logAttr)

    vals = sapply( x@logs, function(z) slot(z, logAttr) )
    #check for no carbon or biomass conversions in the collection...
    if(all(is.na(vals)))
      stop('All values for ',logAttr,' are NA!')

#
#   convert diameters to usual units...
#
    if(logAttr == 'buttDiam' || logAttr == 'topDiam')
       if(x@units == .StemEnv$msrUnits$metric) 
         vals = vals*.StemEnv$m2cm
       else
         vals = vals*.StemEnv$ft2in

#
#   and angles too...
#
    if(logAttr == 'logAngle')
      vals = sapply(vals, .StemEnv$rad2Deg)    #not vectorized at present V0.52

    
    hg = hist(vals, main=main, xlab=xlab, col=col, ...)

    return(invisible(hg))

}    #plot for 'sampSurf'
) #setMethod
     
