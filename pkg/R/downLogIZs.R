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
    sp = vector('list', numIZs)
    for(i in seq_len(numIZs)) {
      class = class(object[[i]])
      if(class == 'standUpIZ')
        sp[[i]] = object[[i]]@circularPlot@perimeter@polygons$pgsCircPlot
      else if(class == 'sausageIZ')
        sp[[i]] = object[[i]]@perimeter@polygons$pgsSausage
      else
        if(!is(object[[i]], 'downLogIZ'))     #catch objects that may not be in the correct form
          stop('All list elements must be a subclass of "downLogIZ"!')
    }
   
    sps = SpatialPolygons(sp)
    bbox = bbox(sps)

#
#   create the object...
#    
    dliz = new('downLogIZs', iZones = object, bbox = bbox,
               units = object[[1]]@units, description = description)
    
    return(dliz)
}   #downLogIZs method for list
)   #setMethod



#********************************************************************************
#** the following really breaks oop because it calls for a character name of
#   a constructor, so for each new subclass added, we have to remember to change
#   this routine as well--bad form.
#================================================================================
#  method for function for a previously made collection of 'downLogs', with a
#  valid 'downLogIZ' subclass for object construction on each log...
#
#setMethod('downLogIZs',
#          signature(object = 'downLogs', izClass='character'),
#function(object,
#         izClass = 'sausageIZ',
#         plotRadius = 5,  #as more sampling methods are added, this may need to be more general
#         description = '',
#         ...
#        )
#{
#------------------------------------------------------------------------------
#
#   we need the sp package for plotting...
#
#    if(!require(sp)) 
#      stop('***>sp package must be loaded to make a valid "downLog" object!')

#
#   check on the object passed is redundant because of the signature, but
#   a check on the character id class for the inclusion zone is necessary...
#
#    if(!(izClass %in% names(getClass('downLogIZ')@subclasses)) )
#      stop('izClass must be a valid subclass of "downLogIZ"')

#
#   make a list and then use the list constructor for the object...
#
#    numLogs = length(object@logs)
#    dliz = vector('list', numLogs)
#    for(i in seq_len(numLogs))
#      if(izClass == 'standUpIZ')
#        dliz[[i]] = standUpIZ(object@logs[[i]], plotRadius)
#      else if(izClass == 'sausageIZ')
#        dliz[[i]] = sausageIZ(object@logs[[i]], plotRadius)

    
 #   dliz = downLogIZs(dliz, description=description)
    
#    return(dliz)
#}   #downLogIZs method for 'downLogs'
#)   #setMethod




#showMethods('downLogIZs')
