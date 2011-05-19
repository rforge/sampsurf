#---------------------------------------------------------------------------
#
#   Methods for generic summary() for sampSurf class...
#     (1) This will have to be changed somewhat once variables other than
#         volume and density become available. 
#
#Author...									Date: 5-Oct-2010
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
#  method for class Tract & subclasses...
#
setMethod('summary',
          signature(object = 'sampSurf'),
function(object,
         ...
        )
{
#------------------------------------------------------------------------------
#   just a simple summary of common items...
#------------------------------------------------------------------------------
    cat('\nObject of class:', class(object))
    .StemEnv$underLine(60)
    if(!is.na(object@description))
      cat(object@description, fill=60)
    .StemEnv$underLine(60, prologue='')

    if(object@tract@units == .StemEnv$msrUnits$metric) {
      unitLen = 'meters'
      unitVol = 'cubic meters'
      unitSA = 'square meters'
    }
    else {
      unitLen = 'feet'
      unitVol = 'cubic feet'
      unitSA = 'square feet'
    }
    
    cat('\nInclusion zone objects:', class(object@izContainer@iZones[[1]]) )
    if(.hasSlot(object@izContainer@iZones[[1]], 'pdsType'))
       cat(' (with PP to: ',object@izContainer@iZones[[1]]@pdsType,')',sep='')
    cat('\nMeasurement units =', object@tract@units)
    numLogs = length(object@izContainer@iZones)
    cat('\nNumber of logs =', numLogs)
    logs = vector('list', numLogs)
    for(i in seq_len(numLogs))
      logs[[i]] = object@izContainer@iZones[[i]]@downLog
    dls = downLogs(logs)
    cat('\nTrue log volume =', dls@stats['total','volume'],unitVol)
    cat('\nTrue log length =', dls@stats['total','length'],unitLen)
    cat('\nTrue log surface area =', dls@stats['total','surfaceArea'],unitSA)
    cat('\nTrue log coverage area =', dls@stats['total','coverageArea'],unitSA)
    cat('\nTrue log biomass =', dls@stats['total','biomass'])
    cat('\nTrue log carbon =', dls@stats['total','carbon'])
    cat('\n\nEstimate attribute:', object@estimate)

    cat('\nSurface statistics...')
    #cat('\n  attribute =', object@estimate)
    cat('\n  mean =', object@surfStats$mean)
    cat('\n  bias =', object@surfStats$bias )

    truth = switch(object@estimate,
                   volume =  dls@stats['total','volume'],
                   Density = numLogs,
                   Length =  dls@stats['total','length'],
                   surfaceArea = dls@stats['total','surfaceArea'],
                   coverageArea = dls@stats['total','coverageArea'],
                   biomass = dls@stats['total','biomass'],
                   carbon = dls@stats['total','carbon'],
                   NA
                  )
    #if(object@estimate == 'volume')
    #  truth = dls@stats['total','volume']
    #else
    #  truth = numLogs
    cat('\n  bias percent =', object@surfStats$bias/truth*100)
    cat('\n  sum =', object@surfStats$sum)
    cat('\n  var =', object@surfStats$var)
    cat('\n  st. dev. =', object@surfStats$stDev)
    cat('\n  cv % =', 100*object@surfStats$stDev/object@surfStats$mean)
    cat('\n  surface max =', object@surfStats$max)
    #cat('\n  st. error =', object@surfStats$se)
    cat('\n  total # grid cells =', object@surfStats$nc)
    cat('\n  grid cell resolution (x & y) =', xres(object@tract), unitLen)
    ncellZero = count(object@tract, 0) #zero cells
    cat('\n  # of background cells (zero) =', ncellZero)
    cat('\n  # of inclusion zone cells =', object@surfStats$nc - ncellZero)
    cat('\n')


    
    #summary(object@tract)
    cat('\n')
    
    return(invisible())
}   #summary for 'sampSurf'
) #setMethod

