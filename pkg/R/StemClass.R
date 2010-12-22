#---------------------------------------------------------------------------
#
#   This file holds the S4 class definitions for the stem-related classes.
#
#   Classes...
#   1. Stem: virtual -- parent of all stem objects
#   2. downLog -- for "normal" down logs
#   3. downLogs -- container class for collections of downLog(s)
#
#Author...									Date: 6-Aug-2010
#	Jeffrey H. Gove
#	USDA Forest Service
#	Northern Research Station
#	271 Mast Road
#	Durham, NH 03824
#	jhgove@unh.edu
#	phone: 603-868-7667	fax: 603-868-7604
#---------------------------------------------------------------------------
#





#=================================================================================================
#
#  1.  define the virtual Stem class...
#
setClass('Stem',
         
#
#  slots for the class and its subclasses...
#
    representation(species = 'character',          #species of piece                   
                   units = 'character',            #English or metric units
                   location = 'SpatialPoints',     #object "central" location
                   spUnits = 'CRS',                #sp units, character will change
#                   spUnits = 'missCRS',            #sp units, character will change
#                  other...                   
                   description = 'character',      #descriptive comment
                   userExtra = 'ANY'               #anything else the user wants to include--no checks
                  ),
    prototype = list(species = '',               #some defaults for validity checking
                     units = 'metric',
                     location = SpatialPoints(matrix(c(0,0), nrow=1, dimnames=list('1',c('x','y'))) ),
                     spUnits = CRS(projargs=as.character(NA)),
#                     spUnits = NULL,
                     description = '',
                     userExtra = NULL
                    ),
    contains = 'VIRTUAL',
    validity = function(object) {
                 if(!(object@units %in% .StemEnv$msrUnits))
                   return('units of measure must be "English" or "metric"')
                 
                 return(TRUE)
               } #validity check
) #class Stem






#=================================================================================================
#
#  2. the downed log class is just a direct descendant of 'Stem'...
#
#  inherited slots interpretation...
#    location = the center of the log longitudinally and radially
#
setClass('downLog',
    representation(buttDiam = 'numeric',          #diameter at the large end in same units as length
                   topDiam = 'numeric',           #diameter at the small end in same units as length
                   logLen = 'numeric',            #log length
                   logAngle = 'numeric',          #log lie angle from top pointing due East **RADIANS**
                   solidType = 'numericNULL',     #used in generic taper function
                   #solidType = 'numeric',         #used in generic taper function
                   logVol = 'numeric',            #this would be total volume in cubic units
                   taper = 'data.frame',          #dbh and length values
                   profile = 'data.frame',        #log profile of both sides, lying North, not rotated
                   rotLog = 'matrix',             #same as profile, but rotated and translated as desired
                   spLog = 'SpatialPolygons',     #rotated by logAngle and translated as desired
                   slNeedleAxis = 'SpatialLines'  #the central needle axis translated & rotated correctly
                  ),
    prototype = list(buttDiam = -1,               #these are dummy values to trigger initialize
                     topDiam = -1,
                     logLen = -1,
                     logAngle = 0,               #this is a valid angle
                     solidType = -1,
                     logVol = -1
                    ),
    contains='Stem',                         #a subclass of the virtual 'Stem' class
    validity = function(object) {
                 if(object@topDiam > object@buttDiam)
                   return('butt diameter must be at least as large as top diameter!')
                 if(object@buttDiam <=0 || object@topDiam <0 || object@logLen <=0)
                   return('logLen, butt and top diameters must have some size!')
#                 if(object@logAngle < 0 || object@logAngle > 360)
#                   return('log angle must be 0 <= logAngle <- 360 degrees')
                 if(object@logAngle <0 || object@logAngle > 2*pi)
                   return('log angle must be 0 <= logAngle <- 2*pi radians')
                 if(!is.null(object@solidType) && (object@solidType <1 || object@solidType > 10))
                   return('solidType must be 1<=solidType<=10')
                 taperNames = match(colnames(object@taper), c('diameter','length'))
                 if(any(is.na(taperNames)))
                   return('taper names bad--woops!')
                 if(nrow(object@taper) < 2 || ncol(object@taper) < 2)
                   return('taper data frame must have at least two rows and columns!')
                 if(object@logVol < 0)
                   reuturn('negative logVol not allowed!')
                 profileNames = match(colnames(object@profile), c('radius','length'))
                 if(any(is.na(profileNames)))
                   return('profile names bad--woops!')
                 rotNames = match(colnames(object@rotLog), c('x','y', 'hc'))
                 if(any(is.na(rotNames)))
                   return('rotLog names bad--woops!')

                 if(!is.na(object@spUnits@projargs) && object@spUnits@projargs == '+proj=longlat')
                   return(paste('spUnits must be commensurate with units,',
                                'please convert to non-geographic coordinate system!')
                         )


                 
                 return(TRUE)
               } #validity check
) #class downLog


#-------------------------------------------------------------------------------
# this is just for very simple initialization with new, for more complex
# logs with taper measurements, use downLog constructor methods to initialize...
#
# initialize is called after the prototype values are set, so we can use those to
# set flags for default initialization here, before validity checking...
#
# note that we can not call downLog from below because it calls new(), which
# would get us into infinite recursion
#-------------------------------------------------------------------------------
#
setMethod('initialize', 'downLog',
  function(.Object, ...) {
    if(.Object@units == .StemEnv$msrUnits$English) {  #initializer puts these in meters or feet...
      if(.Object@buttDiam < 0)
        .Object@buttDiam = runif(1, 0.4, 2.6)  #feet
      if(.Object@logLen < 0)
        .Object@logLen = runif(1, 1, 30) 
    }
    else{
      if(.Object@buttDiam < 0)
        .Object@buttDiam = runif(1, 0.1, 0.8)  #meters
      if(.Object@logLen < 0)
        .Object@logLen = runif(1, 1, 10)
    }
    if(.Object@topDiam < 0)
      .Object@topDiam = runif(1, 0, 0.9)*.Object@buttDiam
    if(.Object@solidType < 1 || .Object@solidType > 10)
      .Object@solidType = round( runif(1,1,10) )

    .Object@logAngle = with( .StemEnv, runif(1,  logAngles[1], logAngles[2]) )#

    taper = as.data.frame(matrix(c(.Object@buttDiam, .Object@topDiam, 0, .Object@logLen), nrow=2))
    colnames(taper) = c('diameter','length')
    .Object@taper = taper

    .Object@logVol = .StemEnv$wbVolume(.Object@buttDiam, .Object@topDiam, .Object@logLen, .Object@solidType)

               
    callNextMethod(.Object, ...)
 } #function
) #setMethod initialize








#=================================================================================================
#=================================================================================================
#
#  3. The downLogs class (plural) is a container class for any number of "downLog" objects...
#
setClass('downLogs',
    representation(logs = 'list',                    #the log objects as a list
                   units = 'character',              #English or metric units
                   bbox = 'matrix',
                   stats = 'data.frame'              #summary of volume, etc. of logs in collection
                   #numLogs = 'numeric'#,              #number of log objects in logs
                   #spLogs = 'SpatialPolygons'        #for simplicity in plotting
                  ),
    prototype = list(logs = list(),                  #empty, zero-length list
                     bbox = matrix(rep(0,4), nrow=2, dimnames=list(c('x','y'), c('min','max')))
                    ),
    validity = function(object) {
                 if(!(object@units %in% .StemEnv$msrUnits))
                   return('units of measure must be "English" or "metric"')
                 
                 numLogs = length(object@logs)
                 if(numLogs < 1)
                   return('no logs in collection!')

                 for(i in seq_len(numLogs))
                   validObject(object@logs[[i]])

                 for(i in seq_len(numLogs))
                   if(object@units != object@logs[[i]]@units)
                     return('At least one log has the wrong units!')

#                check on bbox matrix format...                 
                 if(!class(object@bbox) == 'matrix')
                   return('bbox slot must be a 2x2 matrix')
                 bboxNames = match(rownames(object@bbox), c('x','y'))
                 if(any(is.na(bboxNames)))
                   return('slot bbox rownames must be "x", "y"!')
                 bboxNames = match(colnames(object@bbox), c('min','max'))
                 if(any(is.na(bboxNames)))
                   return('slot bbox colnames must be "min", "max"!')
                     
#                consistent units check...
                 units = object@logs[[1]]@units
                 for(i in seq_len(numLogs))
                   if(object@logs[[i]]@units != units)
                     return('You can not mix measurement units within a population of logs!')
                 
                 return(TRUE)
               } #validity check
) #class downLogs 
