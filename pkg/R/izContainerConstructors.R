#---------------------------------------------------------------------------
#
#   This file holds the S4 definition for the constructors of izContainer
#   subclasses. This is used for a collection or population of
#   any "InclusionZone" subclass, it has different methods for downLogIZ and
#   standingTreeIZ related inclusion zone collections...
#
#   Constructors include...
#     1. izContainer: base method functionality
#     2. downLogIZs: for collections of "downLogIZ" subclass objects
#     3. standingTreeIZs: for collections of standingTreeIZ subclass objects
#
#   This was revamped from the original "downLogIZs" code, which no
#   longer exists. The changes were to move all of the common functionality
#   to the izContainer method, and then just create the objects in the
#   sublass methods. 1-Dec-2011, JHG.
#
#   Note that because "object" is always a list in the signature (albeit 
#   containing different class objects for the two subclasses), we can not 
#   make the subclass constructors methods of the base generic. So they 
#   all must be generics.
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
#   generic definitions...
#
if(!isGeneric("izContainer")) 
  setGeneric('izContainer',  
             function(object, ...) standardGeneric('izContainer'),
             signature = c('object')
            )

if(!isGeneric("downLogIZs")) 
  setGeneric('downLogIZs',  
             function(object, ...) standardGeneric('downLogIZs'),
             signature = c('object')
            )

if(!isGeneric("standingTreeIZs")) 
  setGeneric('standingTreeIZs',  
             function(object, ...) standardGeneric('standingTreeIZs'),
             signature = c('object')
            )



          
#================================================================================
#  1. base method for a previously made collection of IZs in a list...
#
setMethod('izContainer',
          signature(object = 'list'),
function(object,
         ...
        )
{
#------------------------------------------------------------------------------
#
#   this handles the calculation of the overall bbox for each of the
#   subclasses...
#
    numIZs = length(object)
    if(numIZs < 1)
      stop('error in "object": must be at least one inclusion zone in the list')


#
#   make an array of the perimeter bbox matrices for each IZ object, then determine their
#   new overall extent...
#
    bboxArray = array(dim=c(2,2,numIZs))
    for(i in seq_len(numIZs)) {
      if(!is(object[[i]], 'InclusionZone'))     #catch objects that may not be in the correct form
        stop('All list elements must be a subclass of "InclusionZone"!')
      bboxArray[,,i] = bbox(perimeter(object[[i]]))
    }
    dimnames(bboxArray) = dimnames(bbox(object[[1]])) #page dim doesn't matter
    bbox = bboxSum(bboxArray)                         #extend the bboxes to overall

    
    return(bbox)
}   #izContainer method for list
)   #setMethod




          
#================================================================================
#  2. method previously made collection of downLog IZs in a list...
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
#   get the overall bbox for the items in the list from the class method...
#
    bbox = izContainer(object, ...)
  
#
#   create the object...
#    
    dl.iz = new('downLogIZs', iZones = object, bbox = bbox,
               units = object[[1]]@units, description = description)
    
    return(dl.iz)
}   #downLogIZs method for list
)   #setMethod



          
#================================================================================
#  3. method for  a previously made collection of standingTree IZs in a list...
#
setMethod('standingTreeIZs',
          signature(object = 'list'),
function(object,
         description = '',
         ...
        )
{
#------------------------------------------------------------------------------
#
#   get the overall bbox for the items in the list from the class method...
#
    bbox = izContainer(object, ...)
  
#
#   create the object...
#    
    st.iz = new('standingTreeIZs', iZones = object, bbox = bbox,
               units = object[[1]]@units, description = description)
    
    return(st.iz)
}   #standingTreeIZs method for list
)   #setMethod
