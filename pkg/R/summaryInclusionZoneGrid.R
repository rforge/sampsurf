#---------------------------------------------------------------------------
#
#   Methods for generic summary() for InclusionZoneGrid class...
#     (1)  inclusionZoneGrid class
#
#Author...									Date: 23-sept-2010
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
#  method for class InclusionZoneGrid...
#
setMethod('summary',
          signature(object = 'InclusionZoneGrid'),
function(object,
         ...
        )
{
#------------------------------------------------------------------------------
    cat('\nObject of class:', class(object))
    .StemEnv$underLine(60)
    if(!is.na(object@description))
      cat(object@description, fill=60)
    .StemEnv$underLine(60, prologue='')

    cat('\nInclusionZone class:', class(object@iz))
    cat('\n  units of measurement: ', object@iz@units)    

    grid = object@grid
    cat('\n\nTract class:', class(grid))
    cat('\nGrid cell values*...\n')
    gridValues = getValues(grid)
    gvt = data.frame(table(gridValues, useNA='ifany'))
    print(gvt)
    cat('\nNumber of grid cells =', ncell(grid))
    cat('\nCell dimensions: (nrows=',nrow(grid), ', ncol=',ncol(grid),')', sep='')

    cat('\nPer unit area estimates in data slot...\n')
    df = data.frame(na.omit(cbind(gridValues, object@data)))[,-1]   #omit cells outside the inclusion zone
    print(apply(df, 2, summary))                                    #only vary under full sausage-chainsaw

    cat('\n\n  Encapulating bounding box...\n')
    print(object@bbox)

    cat('\n*Note: data slot values get swapped with zero-valued grid cells as necessary.')
    
    cat('\n')
    
    return(invisible())
}   #summary for 'InclusionZoneGrid'
) #setMethod


