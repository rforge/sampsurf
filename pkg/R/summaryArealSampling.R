#---------------------------------------------------------------------------
#
#   Methods for generic summary() for ArealSampling class...
#     (1) circularPlot class
#     (2) pointRelascope class
#     (3) perpendicularDistance class
#     (4) distanceLimited class
#     (5) angleGauge class
#
#Author...									Date: 20-Aug-2010
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
#  method for virtual class ArealSampling...
#
setMethod('summary',
          signature(object = 'ArealSampling'),
function(object,
         ...
        )
{
#------------------------------------------------------------------------------
#   just a simple summary of common items from virtual class...
#------------------------------------------------------------------------------
    cat('\nObject of class:', class(object))
    .StemEnv$underLine(60)
    if(!is.na(object@description))
      cat(object@description, fill=60)
    .StemEnv$underLine(60, prologue='')

    cat('\nArealSampling...')
    cat('\n  units of measurement: ', object@units)

    cat('\n')
    
    return(invisible())
}   #summary for 'ArealSampling'
) #setMethod




#================================================================================
# method for class "circularPlot"...
#
setMethod('summary',
          signature(object = 'circularPlot'),
function(object,
         ...
        )
{
#------------------------------------------------------------------------------
#   add a little to 'ArealSampling' method for 'circularPlot'...
#------------------------------------------------------------------------------
    callNextMethod()

    cat('\ncircularPlot...')
    if(object@units == .StemEnv$msrUnits$metric) {
      cat('\n  radius = ', object@radius, ' meters',sep='')
      cat('\n  area = ', object@area, ' square meters', sep='')
      cat(' (', format(object@area/.StemEnv$smpHectare, digits=4), ' hectares)')
    }
    else {
      cat('\n  radius = ', object@radius, ' feet',sep='')
      cat('\n  area = ', object@area, ' square feet', sep='')
      cat(' (', format(object@area/.StemEnv$sfpAcre, digits=4), ' acres)', sep='')
    }
    cat('\n  spatial units: ', object@spUnits@projargs)
    cat('\n  spatial ID:', object@spID)
    cat('\n  location (plot center)...')
    cat('\n    x coord: ', coordinates(object@location)[,'x'])
    cat('\n    y coord: ', coordinates(object@location)[,'y'])
#    cat('\n    (Above coordinates are for plot center)')

#
#   important check to see if any valid SpatialPolygon exists for the object...
#
    if(length(object@perimeter@polygons) != 0)  #check for object made with new()
      cat('\n  Number of perimeter points:',
          dim(object@perimeter@polygons$pgsCircPlot@Polygons$circPlot@coords)[1], #yeck
          '(closed polygon)')
    else
      cat('\n\n***No perimeter "SpatialPolygons" -- please use circularPlot constructor!\n')

    cat('\n')
        
    return(invisible())
}   #summary for 'circularPlot'
) #setMethod








#================================================================================
# method for class "pointRelascope"...
#
setMethod('summary',
          signature(object = 'pointRelascope'),
function(object,
         ...
        )
{
#------------------------------------------------------------------------------
#   add a little to 'ArealSampling' method for 'pointRelascope'...
#------------------------------------------------------------------------------
    callNextMethod()

    if(object@units == .StemEnv$msrUnits$metric) 
      slUnits = 'square meters per hectare'
    else
      slUnits = 'square feet per acre'
    
    cat('\npointRelascope...')
    cat('\n  Angle (nu) in degrees =', object@angleDegrees)
    cat('\n  Angle (nu) in radians =', object@angleRadians)
    cat('\n  PRS area factor (phi) =', object@phi)
    cat('\n  PRS squared-length factor (L) =', object@slFactor, slUnits)
    cat('\n  This angle has a ',format(object@rwFactor,digits=3),':1 reach:width factor',sep='')

    cat('\n')
        
    return(invisible())
}   #summary for 'pointRelascope'
) #setMethod

    





#================================================================================
# method for class "perpendicularDistance"...
#
setMethod('summary',
          signature(object = 'perpendicularDistance'),
function(object,
         ...
        )
{
#------------------------------------------------------------------------------
#   add a little to 'ArealSampling' method for 'perpendicularDistance'...
#------------------------------------------------------------------------------
    callNextMethod()

    if(object@units == .StemEnv$msrUnits$metric) {
      factorUnits = 'cubic meters [square meters] per hectare'
      kUnits = 'per meter [or dimensionless]'
    }
    else {
      factorUnits = 'cubic feet [square feet] per acre'
      kUnits = 'per foot [dimensionless]'
    }
    
    cat('\nperpendicularDistance...')
    cat('\n  kPDS factor =', object@kpds, kUnits, 'for volume [surface/coverage area]')
    cat('\n  volume [surface/coverage area] factor =', object@factor, factorUnits)

    cat('\n')
        
    return(invisible())
}   #summary for 'perpendicularDistance'
) #setMethod






#================================================================================
# method for class "distanceLimited"...
#
setMethod('summary',
          signature(object = 'distanceLimited'),
function(object,
         ...
        )
{
#------------------------------------------------------------------------------
#   add a little to 'ArealSampling' method for 'distanceLimited'...
#------------------------------------------------------------------------------
    callNextMethod()

    cat('\ndistanceLimited...')
    if(object@units == .StemEnv$msrUnits$metric) 
      dlUnits = 'meters'
    else 
      dlUnits = 'feet'

    cat('\n  Distance limit =', object@distanceLimit, dlUnits)

    cat('\n')
        
    return(invisible())
}   #summary for 'distanceLimited'
) #setMethod
    





#================================================================================
# method for class "angleGauge"...
#
setMethod('summary',
          signature(object = 'angleGauge'),
function(object,
         ...
        )
{
#------------------------------------------------------------------------------
#   add a little to 'ArealSampling' method for 'angleGauge'...
#------------------------------------------------------------------------------
    callNextMethod()

    if(object@units == .StemEnv$msrUnits$metric) { 
      saUnits = ' square meters per hectare'
      prfUnits = ' meters per cm'
      PRFUnits = ' meters per meter'
    }
    else {
      saUnits = ' square feet per acre'
      prfUnits = ' feet per inch'
      PRFUnits = ' feet per foot'
    }
    
    cat('\nangleGauge...')
    cat('\n  Angle (nu) in degrees = ', object@angleDegrees, sep='')
    cat('\n  Angle (nu) in radians = ', object@angleRadians, sep='')
    cat('\n  Basal area factor (baf) = ', object@baf, saUnits, sep='')
    cat('\n  Plot radius factor (prf) = ', object@prf, prfUnits,' (',object@PRF, PRFUnits,')',sep='')
    cat('\n  This angle has a proportionality factor (alpha) = ',format(object@alpha,digits=3),
        PRFUnits, sep='')

    cat('\n')
        
    return(invisible())
}   #summary for 'angleGauge'
) #setMethod
