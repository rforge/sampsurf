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

    cat('\nInclusion zone objects:', class(object@izContainer@iZones[[1]]) )
    cat('\nEstimate attribute:', object@estimate)
    cat('\nMeasurement units =', object@tract@units)
    numLogs = length(object@izContainer@iZones)
    cat('\nNumber of Logs =', numLogs)
    logs = vector('list', numLogs)
    for(i in seq_len(numLogs))
      logs[[i]] = object@izContainer@iZones[[i]]@downLog
    dls = downLogs(logs)
    cat('\nTrue Log volume =', dls@stats['total','volume'])

    cat('\nSurface statistics...')
    #cat('\n  attribute =', object@estimate)
    cat('\n  mean =', object@surfStats$mean)
    cat('\n  bias =', object@surfStats$bias )
    cat('\n  bias percent =', object@surfStats$bias/dls@stats['total','volume']*100)
    cat('\n  sum =', object@surfStats$sum)
    cat('\n  var =', object@surfStats$var)
    cat('\n  st. dev. =', object@surfStats$stDev)
    cat('\n  st. error =', object@surfStats$se)
    cat('\n  total # grid cells =', object@surfStats$nc)
    ncellZero = count(object@tract, 0) #zero cells
    cat('\n  # of background cells (zero) =', ncellZero)
    cat('\n  # of inclusion zone cells =', object@surfStats$nc - ncellZero)
    cat('\n')


    
    #summary(object@tract)
    cat('\n')
    
    return(invisible())
}   #summary for 'sampSurf'
) #setMethod

