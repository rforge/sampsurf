#---------------------------------------------------------------------------
#
#   This file holds the S4 definition for the constructor methods of the
#   sampSurf class...
#
#   The methods include signatures for...
#     Signature: "object", "tract"
#     1. "izContainer", "Tract": Takes a collection of inclusion
#        zones already generated for the tract argument.
#     2. "numeric", "Tract": This will allow generating a sampling surface
#        from scratch. The object is the number of logs/trees, you can specify any
#        argument for generating logs/trees that will be passed on to downLogs or
#        standingTrees in the "..." argument list, similar for other methods.
#
#   Revamped to handle standing tree methods 5-Dec-2011, JHG.
#
#Author...									Date: 1-Oct-2010
#	Jeffrey H. Gove
#	USDA Forest Service
#	Northern Research Station
#	271 Mast Road
#	Durham, NH 03824
#	jhgove@unh.edu
#	phone: 603-868-7667	fax: 603-868-7604
#---------------------------------------------------------------------------
#   generic definition...
#
setGeneric('sampSurf',  
           function(object, tract, ...) standardGeneric('sampSurf'),
             signature = c('object', 'tract')
            )




          
#================================================================================
#
#   Takes a collection of Stem inclusion zones and a "Tract" object...
#
setMethod('sampSurf',
          signature(object = 'izContainer', tract='Tract'), 
function(object, 
         tract,
         estimate = unlist(.StemEnv$puaEstimates),
         wantChainSaw = FALSE,
         description = 'sampling surface object',
         runQuiet = FALSE,
         ...
        )
{
#---------------------------------------------------------------------------
#
#   This is the main routine for calculating stats on the surface etc, any
#   other constructors should eventually call this one.
#
#   Arguments...
#     wantChainSaw = TRUE: calculate the full chainSaw inclusion zone by
#                    using the sausageIZ zones for each log--note that
#                    iZone must be 'sausageIZ' for this to work; FALSE: one 
#                    of the other methods.
#
#---------------------------------------------------------------------------
#
#   chainsaw is always the problem child...
#
    if(wantChainSaw && !is(object@iZones[[1]], 'sausageIZ'))
      stop('If you want the full chainsaw estimate, you must also specify sausage inclusion zones!')
    if(is(object@iZones[[1]], 'chainSawIZ'))                 #this request does not make sense
      stop('You must use \"sausageIZ\" zones for the chainSaw method!')

#
#   throw a warning if any of the inclusion zones land outside the tract boundary...
#
    bb.iz = bbox(object)
    bb.tr = bbox(tract)
    if(any(bb.iz[,'min']<bb.tr[,'min']) || any(bb.iz[,'max']>bb.tr[,'max']))
      warning('Some object inclusion zones lie outside the tract--this will impart a bias!!')

#
#   let's see what we are dealing with here...
#
    estimate = match.arg(estimate)
    if(is(object, 'downLogIZs')) {
      isLogs = TRUE
      stemName = 'log'
      if(!estimate %in% .StemEnv$validEstimates$downLogs)
        stop(paste(estimate,'is not a valid attribute for downLogs'))
    }
    else {
      isLogs = FALSE
      stemName = 'tree'
      if(!estimate %in% .StemEnv$validEstimates$standingTrees)
        stop(paste(estimate,'is not a valid attribute for standingTrees'))
    }

    
#
#   heap each inclusion zone in the collection...
#
    nStems = length(object@iZones)
    if(!runQuiet) {
      cat('\nNumber of ',stemName,'s in collection = ', nStems, sep='')
      cat('\nHeaping ',stemName,': ',sep='')
    }
    for(i in seq_len(nStems)) {
      if(!runQuiet)
        cat(i,',',sep='')
      if(wantChainSaw && is(object@iZones[[1]], 'sausageIZ')) {   #need sausage inclusion zones for chainsaw
        izg.sa = izGrid(object@iZones[[i]], tract)                #first: InclusionZoneGrid for sausage
        izg = izGridCSFull(izg.sa, tract)                         #then: full chainsaw iz grid
        if(i==1)
          description = paste(description,'(Full chainsaw inclusion zone)',sep='\n')
      }
      else   
        izg = izGrid(object@iZones[[i]], tract, ...)              #InclusionZoneGrid
      tract = heapIZ(izg, tract, estimate = estimate, ...)        #heap it up
    }


#
#   get the true population attribute value for the collection...
#   note that it is possible for biomass and carbon to have logs with
#   no estimates (NA), so we must account for that since these quantities are
#   optional...
#
    if(isLogs)
      Stems = as(object, 'downLogs')
    else
      Stems = as(object, 'standingTrees')

    truth = switch(estimate,
                   volume =  Stems@stats['total','volume'],
                   Density = nStems,
                   Length =  Stems@stats['total','length'],
                   surfaceArea = Stems@stats['total','surfaceArea'],
                   coverageArea = Stems@stats['total','coverageArea'],
                   basalArea = Stems@stats['total','basalArea'],
                   biomass = Stems@stats['total','biomass'],
                   carbon = Stems@stats['total','carbon'],
                   NA
                  )

#
#   we must adjust the area of the tract in case it is different from one hectare
#   or one acre...
#
    unitArea = ifelse(object@units==.StemEnv$msrUnits$English, .StemEnv$sfpAcre, .StemEnv$smpHectare) 
    areaAdjust = tract@area/unitArea   
    
#
#   some surface stats using raster...
#
    surfStats = list( mean = cellStats(tract, mean)*areaAdjust,
                      sum = cellStats(tract,sum)*areaAdjust,
                      var = cellStats(tract, var)*areaAdjust^2,
                      nc = ncell(tract),
                      max = maxValue(tract)*areaAdjust
                    )
    surfStats$stDev = sqrt(surfStats$var)
    surfStats$se = surfStats$stDev/sqrt(surfStats$nc)
    surfStats$bias = surfStats$mean - truth
    surfStats$popTotal = truth
             

#
#   create the object...
#
    ss = new('sampSurf',
             description = description,
             izContainer = object,
             tract = tract,
             estimate = estimate,
             surfStats = surfStats
            )

    if(!runQuiet)
      cat('\n')

    return(ss)
}   #sampSurf for "izContainer"
)   #setMethod






          
#================================================================================
#
#   Takes the number of stems and a "Tract" object, other arguments for, e.g.,
#   downLogs, can be passed via "..."
#
#
setMethod('sampSurf',
          signature(object = 'numeric', tract='Tract'),
function(object, 
         tract,
         iZone,
         estimate = unlist(.StemEnv$puaEstimates),
         wantChainSaw = FALSE,               #always the exception
         description = 'sampling surface object',
         runQuiet = FALSE,
         ...
        )
{
#---------------------------------------------------------------------------
#
#   Arguments...
#     wantChainSaw = TRUE: calculate the full chainSaw inclusion zone by
#                    using the sausageIZ zones for each log--note that
#                    iZone must be 'sausageIZ' for this to work; FALSE: one 
#                    of the other methods.
#
#   a few checks...
#
    nStems = round(object)
    if(nStems<1)
      stop('You must specify a positive number of stems in "object"!')

#
#   make sure the inclusion zone constructor is a valid available type...
#
    if(extends(iZone, 'downLogIZ')) {
      isLogs = TRUE                                 #proper English
      papa = getClass('downLogIZ')
    }
    else if(extends(iZone, 'standingTreeIZ')) {
      isLogs = FALSE
      papa = getClass('standingTreeIZ')
    }
    else                                  #catch non-InclusionZone subclass values...
      stop('Invalid inclusion zone constructor name supplied: iZone = ',iZone)
    #above test is not quite enough, iZone must actually be a subclass, not the parent itself...     
    validNames = names(papa@subclasses)
    if(is.na(match(iZone, validNames)))
      stop('Invalid inclusion zone constructor name supplied: iZone = ',iZone)
    if(iZone=='chainSawIZ')                          #catch this error too
      stop('You must use \"sausageIZ\" zones for the chainSaw method!')
 

#
#   get the logs/trees and the inclusion zones...
#
    if(isLogs) {
      dlogs = downLogs(nStems, tract, ...)
      izs = downLogIZs(lapply(dlogs@logs, iZone, ...))
    }
    else {
      strees = standingTrees(nStems, tract, ...)
      izs = standingTreeIZs(lapply(strees@trees, iZone, ...))
    }

#
#   just apply the default constructor now...
#
    ss = sampSurf(izs, tract, estimate=estimate, wantChainSaw=wantChainSaw,
                  description=description, runQuiet=runQuiet, ...)

    return(ss)
}   #sampSurf for "numeric"
)   #setMethod
    
