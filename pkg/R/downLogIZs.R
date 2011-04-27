#---------------------------------------------------------------------------
#
#   This file holds the S4 definition for the constructors of downLogIZs
#   generic and methods. This is used for a collection or population of
#   "downLogIZ"s, it has several methods with different signatures for...
#
#   Constructors include...
#     1. passing a list of extant "downLogIZ" objects
#     2. NA: random generation of "downLogIZ" objects
#     3. NA: a list of "downLogs" objects with a character specifying
#        the "downLogIZ" class to be constructed. signature will need
#        to be modified for all, with missing used in 1&2 for 2nd argument
#
#   Note that 2 & 3 are not implemented yet & probably won't be.
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
#   generic definition...
#
if(!isGeneric("downLogIZs")) 
  setGeneric('downLogIZs',  
             function(object, ...) standardGeneric('downLogIZs'),
             signature = c('object')
            )



          
#================================================================================
#  method for function for a previously made collection of log IZs, most often
#  from a sample by a list...
#
setMethod('downLogIZs',
          signature(object = 'list'),
function(object,
         description = '',
         ...
        )
{
#------------------------------------------------------------------------------
#
#   note that this code allows mixing of different objects within class "downLogIZ"
#   but currently, the validation routines will subsequently flag this as an error,
#   so we'll let it go here and deal with it later...
#
    numIZs = length(object)
    if(numIZs < 1)
      stop('error in "object": must be at least one inclusion zone in the list')

#
#   group all polygons into one SpatialPolygons to get the overall bbox; note
#   that this code allows mixing of different objects within class "downLogIZ"
#   but currently, the validation routines will subsequently flag this as an error,
#   so we'll let it go here and deal with it later...
#
#    sp = vector('list', numIZs)
#    for(i in seq_len(numIZs)) {
#      if(!is(object[[i]], 'downLogIZ'))     #catch objects that may not be in the correct form
#        stop('All list elements must be a subclass of "downLogIZ"!')
#      class = class(object[[i]])
#      if(class == 'standUpIZ')
#        sp[[i]] = object[[i]]@circularPlot@perimeter@polygons$pgsCircPlot
#      else if(class == 'sausageIZ')
#        sp[[i]] = object[[i]]@perimeter@polygons$pgsSausage
#      else if(class == 'pointRelascopeIZ')
#        sp[[i]] = object[[i]]@perimeter@polygons$pgsPRS
#      else                                  #all others should now be standard as...
#        sp[[i]] = object[[i]]@perimeter@polygons$pgs
#      else if(class == 'perpendicularDistanceIZ')
#        sp[[i]] = object[[i]]@perimeter@polygons$pgs
#      else if(class == 'omnibusPDSIZ')
#        sp[[i]] = object[[i]]@perimeter@polygons$pgs
#      else if(class == 'distanceLimitedPDSIZ')
#        sp[[i]] = object[[i]]@perimeter@polygons$pgs
#      else if(class == 'omnibusDLPDSIZ')
#        sp[[i]] = object[[i]]@perimeter@polygons$pgs
#      else if(class == 'distanceLimitedMCIZ')
#        sp[[i]] = object[[i]]@perimeter@polygons$pgs
#      else
#        if(!is(object[[i]], 'downLogIZ'))     #catch objects that may not be in the correct form
#          stop('All list elements must be a subclass of "downLogIZ"!')
#    }
   
#    sps = SpatialPolygons(sp)
#    bbox = bbox(sps)

#
#   make an array of the perimeter bbox matrices for each IZ object, then determine their
#   new overall extent...
#
    bboxArray = array(dim=c(2,2,numIZs))
    for(i in seq_len(numIZs)) {
      if(!is(object[[i]], 'downLogIZ'))     #catch objects that may not be in the correct form
        stop('All list elements must be a subclass of "downLogIZ"!')
      bboxArray[,,i] = bbox(perimeter(object[[i]]))
    }
    dimnames(bboxArray) = dimnames(bbox(object[[1]])) #page dim doesn't matter
    bbox = bboxSum(bboxArray)                         #extend the bboxes to overall

      
#
#   create the object...
#    
    dliz = new('downLogIZs', iZones = object, bbox = bbox,
               units = object[[1]]@units, description = description)
    
    return(dliz)
}   #downLogIZs method for list
)   #setMethod



#showMethods('downLogIZs')
