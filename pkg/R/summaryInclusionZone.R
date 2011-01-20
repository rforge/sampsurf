#---------------------------------------------------------------------------
#
#   Methods for generic summary() for InclusionZone class...
#     (1) InclusionZone base class
#     (2) downLog component class--on a per unit area basis
#     (3) standUpIZ class
#     (4) chainSawIZ class
#     (5) sausageIZ class
#     (6) downLogIZs (plural) class
#
#Author...									Date: 24-Aug-2010
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
#  method for virtual class InclusionZone...
#
setMethod('summary',
          signature(object = 'InclusionZone'),
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

    cat('\nInclusionZone...')
    cat('\n  units of measurement: ', object@units)    
    cat('\n  Per unit area blowup factor:', object@puaBlowup)
    cat(ifelse(object@units == .StemEnv$msrUnits$metric, ' per hectare', ' per acre'))
    cat('\n\n  Object bounding box...\n');print(object@bbox)

    if(!is.null(object@userExtra))
      cat('\n  "Note: userExtra" slot is non-NULL')

    cat('\n')
    
    return(invisible())
}   #summary for 'InclusionZone'
) #setMethod





#================================================================================
#  method for class "downLogIZ"...
#
setMethod('summary',
          signature(object = 'downLogIZ'),
function(object,
         ...
        )
{
#------------------------------------------------------------------------------
#   add a little to 'InclusionZone' method for 'downLogIZ'...
#------------------------------------------------------------------------------
    callNextMethod()

    cat('\ndownLog component...')
    cat('\n  Spatial ID:', object@downLog@spLog@polygons$pgsLog@ID)
    if(object@units == .StemEnv$msrUnits$metric) {
      cat('\n  Volume in cubic meters:', object@puaEstimates$cubicVolume, 'per hectare')
      cat('\n  Number of logs:', object@puaEstimates$Density, 'per hectare')
    }
    else {
      cat('\n  Volume in cubic feet:', object@puaEstimates$cubicVolume, 'per acre')
      cat('\n  Number of logs:', object@puaEstimates$Density, 'per acre')
    }

    cat('\n')
    
    return(invisible())
}   #summary for 'downLogIZ'
) #setMethod


    



#================================================================================
#  method for class "standUpIZ"...
#
setMethod('summary',
          signature(object = 'standUpIZ'),
function(object,
         ...
        )
{
#------------------------------------------------------------------------------
#   add a little to 'InclusionZone' & 'downLogIZ' methods for 'standUpIZ'...
#------------------------------------------------------------------------------
    callNextMethod()
    
    cat('\nstandUpIZ...')
    cat('\n  use \"summary\" on the circularPlot slot for details')
    #summary(object@circularPlot)

    cat('\n')
    
    return(invisible())
}   #summary for 'standUpIZ'
) #setMethod

    



#================================================================================
#  method for class "chainSawIZ"...
#
setMethod('summary',
          signature(object = 'chainSawIZ'),
function(object,
         ...
        )
{
#------------------------------------------------------------------------------
#   add a little to 'InclusionZone' & 'downLogIZ' methods for 'chainSawIZ'...
#------------------------------------------------------------------------------
    callNextMethod()
    cat('  The above estimates are based on the expanded sliver portion.\n')
    cat('  The following are unexpanded...')
    if(object@units == .StemEnv$msrUnits$metric)
      cu = 'cubic meters'
    else
      cu = 'cubic feet'
    cat('\n    Total log volume:', object@downLog@logVol, cu)
    cat('\n    Bounding bolt volume:',object@bolt$boltVol, cu)
    cat('\n    Sliver volume:', object@bolt$sectVol, cu)
    cat('\n    Sliver area is',object@bolt$area['propArea'],'of bounding bolt\n')
    
    cat('\nchainSawIZ...')
    cat('\n  use \"summary\" on the circularPlot slot for sample plot details')
    #summary(object@circularPlot)

    cat('\n')
    
    return(invisible())
}   #summary for 'chainSawIZ'
) #setMethod






#================================================================================
#  method for class "sausageIZ"...
#
setMethod('summary',
          signature(object = 'sausageIZ'),
function(object,
         ...
        )
{
#------------------------------------------------------------------------------
#   add a little to 'InclusionZone' & 'downLogIZ' methods for 'sausageIZ'...
#------------------------------------------------------------------------------
    callNextMethod()
    
    cat('\nsausageIZ...')
    cat('\n  Spatial ID:', object@perimeter@polygons$pgsSausage@ID)
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
    cat('\n  Number of perimeter points:', dim(object@sausage)[1], '(closed polygon)')


    cat('\n')
    
    return(invisible())
}   #summary for 'sausageIZ'
) #setMethod






#================================================================================
#  method for class "pointRelascopeIZ"...
#
setMethod('summary',
          signature(object = 'pointRelascopeIZ'),
function(object,
         ...
        )
{
#------------------------------------------------------------------------------
#   add a little to 'InclusionZone' & 'downLogIZ' methods for 'pointRelascopeIZ'...
#------------------------------------------------------------------------------
    callNextMethod()
    
    cat('\npointRelascopeIZ...')
    cat('\n  Spatial ID:', object@perimeter@polygons$pgsPRS@ID)
    if(object@units == .StemEnv$msrUnits$metric) {
      cat('\n  dual circle radius = ', object@radius, ' meters',sep='')
      cat('\n  area = ', object@area, ' square meters', sep='')
      cat(' (', format(object@area/.StemEnv$smpHectare, digits=4), ' hectares)')
    }
    else {
      cat('\n  dual circle radius = ', object@radius, ' feet',sep='')
      cat('\n  area = ', object@area, ' square feet', sep='')
      cat(' (', format(object@area/.StemEnv$sfpAcre, digits=4), ' acres)', sep='')
    }
    cat('\n  Number of perimeter points:', dim(object@dualCircle)[1], '(closed polygon)')


    cat('\n')
    
    return(invisible())
}   #summary for 'pointRelascopeIZ'
) #setMethod










#================================================================================
#  method for data frames and class "downLogIZs" (plural!)...
#
setMethod('summary',
          signature(object = 'downLogIZs'),
function(object,
         ...
        )
{
#------------------------------------------------------------------------------
#   just a simple summary of items in the "downLogIZs" object...
#------------------------------------------------------------------------------
    cat('\nObject of class:', class(object))
    .StemEnv$underLine(60)
    cat(object@description, fill=60)
    .StemEnv$underLine(60, prologue='')
    cat('Container class object...')
    numIZs = length(object@iZones)
    cat('\n  There are',numIZs,'inclusion zones in the population')
    cat('\n  Inclusion zones are of class:', class(object@iZones[[1]]))
    cat('\n  Units of measurement: ', object@units)

    cat('\n\n  Encapulating bounding box...\n')
    print(object@bbox)

    cat('\n')
    return(invisible())
}   #summary for 'downLogIZs'
) #setMethod


#showMethods('summary')
