#---------------------------------------------------------------------------
#
#   This file holds the S4 definition for the constructor methods of the
#   sampSurf class...
#
#   The methods include signatures for...
#     Signature: "object", "tract"
#     1. "downLogIZs", "Tract": Takes a collection of down log inclusion
#        zone already generated for the tract argument.
#     2. "numeric", "Tract": This will allow generating a sampling surface
#        from scratch. The object is the number of logs, you can specify any
#        argument for generating logs that will be passed on to downLogs in
#        the ... argument list, similar for other methods.
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
#   Takes a collection of log inclusion zones and a "Tract" object...
#
setMethod('sampSurf',
          signature(object = 'downLogIZs', tract='Tract'),
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
#   create the surface--chainsaw is always the problem child...
#
    if(wantChainSaw && !is(object@iZones[[1]], 'sausageIZ'))
      stop('If you want the full chainsaw estimate, you must also specify sausage inclusion zones!')

#
#   throw a warning if any of the inclusion zones land outside the tract boundary...
#
    bb.iz = bbox(object)
    bb.tr = bbox(tract)
    if(any(bb.iz[,'min']<bb.tr[,'min']) || any(bb.iz[,'max']>bb.tr[,'max']))
      warning('Some object incusion zones lie outside the tract--this will impart a bias!!')
    
#
#   heap each inclusion zone in the collection...
#
    estimate = match.arg(estimate)
    nLogs = length(object@iZones)
    if(!runQuiet) {
      cat('\nLogs in collection =', nLogs)
      cat('\nHeaping log: ')
    }
    for(i in seq_len(nLogs)) {
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
#   we must adjust the area of the tract in case it is different from one hectare
#   or one acre...
#
    unitArea = ifelse(object@units==.StemEnv$msrUnits$English, .StemEnv$sfpAcre, .StemEnv$smpHectare) 
    areaAdjust = tract@area/unitArea     
    
#
#   some surface stats--truth value is from the raw log quantities...
#   note that it is possible for biomass and carbon to have logs with
#   no estimates (NA), so we must account for that since these quantities are
#   optional...
#
    surfStats = list( mean=cellStats(tract, mean)*areaAdjust,
                      sum=cellStats(tract,sum)*areaAdjust,
                      var=cellStats(tract, var)*areaAdjust^2,
                      nc=ncell(tract)
                    )
    surfStats$stDev = sqrt(surfStats$var)
    surfStats$se = surfStats$stDev/sqrt(surfStats$nc)
    truth = switch(estimate,
                   volume = sum(sapply(object@iZones,function(x){x@downLog@logVol})),
                   Density = nLogs,
                   Length = sum(sapply(object@iZones,function(x){x@downLog@logLen})),
                   surfaceArea = sum(sapply(object@iZones,function(x){x@downLog@surfaceArea})),
                   coverageArea = sum(sapply(object@iZones,function(x){x@downLog@coverageArea})),
                   biomass = sum(sapply(object@iZones,function(x){x@downLog@biomass}), na.rm=TRUE), #optional
                   carbon = sum(sapply(object@iZones,function(x){x@downLog@carbon}), na.rm=TRUE),   #optional
                   NA
                  )
    surfStats$bias = surfStats$mean - truth     
             

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
}   #sampSurf for "downLogIZs"
)   #setMethod






          
#================================================================================
#
#   Takes the number of logs and a "Tract" object, other arguments for, e.g.,
#   downLogs, can be passed via "..."
#
#   Eventually, if standingTrees are added, we will have to change this to
#   reflect generating these rather than downLog objects--could key off the kind
#   of subclass passed in iZone below if we start with InclusionZone rather
#   than downLog in the check below
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
    nLogs = object
    if(nLogs<1)
      stop('You must specify a positive number of logs in "object"!')

#
#   make sure the inclusion zone constructor is a valid available type...
#
    papa = getClass('downLogIZ')
    validNames = names(papa@subclasses)
    if(is.na(match(iZone, validNames)))
      stop('Invalid inclusion zone constructor name supplied: iZone = ',iZone)
       

#
#   get the logs and the inclusion zones...
#
    dlogs = downLogs(nLogs, tract, ...)
    izs = downLogIZs(lapply(dlogs@logs, iZone, ...))

#
#   just apply the default constructor now...
#
    ss = sampSurf(izs, tract, estimate=estimate, wantChainSaw=wantChainSaw,
                  description=description, runQuiet=runQuiet, ...)

    return(ss)
}   #sampSurf for "numeric"
)   #setMethod
    
