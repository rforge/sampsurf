#---------------------------------------------------------------------------
#
#   This file holds the S4 definition for the constructors of downLog
#   classes via the downLog generic and methods.
#
#   The methods key off the argument 'data', which can be one of...
#     1. a data frame: for observed taper
#     2. a function: so one could pass one's own taper and volume functions
#     3. numeric: the butt and tip diameters & length?????????
#   **2. & 3. are not implemented, and may not need to be since a user can
#     make a data frame themselves from taper equations
#
#   The log's canonical position is centered at (0,0) with tip pointing east.
#   Translation and rotation are both relative to this position. Note in
#   particular that the log base/butt is not at (0,0), its center in (x,y)
#   is at (0,0).
#
#   Note that the sp package should be loaded for the complete functionality. 
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
#   generic definition...
#
#if (!isGeneric("downLog")) 
  setGeneric('downLog',  
             function(object, ...) standardGeneric('downLog'),
             signature = c('object')
            )



          
#================================================================================
#  method for functions and class downLog...
#
setMethod('downLog',
          signature(object = 'missDF'),
function(object,
         buttDiam = 5,                 #cm
         topDiam = 0,                  #cm
         logLen = 5,                   #meters
         nSegs = 20,
         solidType = NULL,             #defaults to 3  
         logAngle = 0,                 #identity
         logVol = NULL,
         centerOffset = c(x=0, y=0),   #log center offset
         species = '',
         logID = paste('log',format(runif(1),digits=6),sep=':'),
         description = NULL,
         userExtra = NULL,
         units = 'metric',
         spUnits = CRS(projargs=as.character(NA)),
         #wantPlot = FALSE,
         runQuiet = TRUE,
         ...
        )
{
#------------------------------------------------------------------------------
#
#   some initial checks...
#
    if(nSegs < 1)
      stop('A log must have at least one segment!')
    if(any(is.na(match(c('x','y'),names(centerOffset)))))
      stop('Please use names x and y for centerOffset vector')
    if(length(centerOffset) < 2 || length(centerOffset) > 3)
      stop('Please supply one set of x,y[,z] coordinates for the object location.')
    if(is.na(match(units, .StemEnv$msrUnits)))
      stop('Illegal measurement units!')

#
#   description field...
#
    if(is.null(description) || is.na(description))
      description = ''
    else
      description = as.character(description)


#
#   convert diameters to meters or feet as appropriate...
#
    if(units == .StemEnv$msrUnits$English) {
      buttDiam = .StemEnv$in2ft * buttDiam
      topDiam = .StemEnv$in2ft * topDiam
    }
    else {
      buttDiam = .StemEnv$cm2m * buttDiam
      topDiam =  .StemEnv$cm2m * topDiam
    }

#
#   check to make sure but and top diameters are the same as in the taper data
#   frame if it was passed; same for length...
#
    if(!missing(object)) {
      nr = nrow(object)
      #identical is too strong here...
      if(!isTRUE(all.equal(buttDiam, object[1,'diameter'])) ||
         !isTRUE(all.equal(topDiam, object[nr,'diameter'])) )
        stop('buttDiam and topDiam must be equal to their respective measurements in the taper data!')
      if(!isTRUE(all.equal(logLen, object[nr,'length']-object[1,'length'])))
        stop('logLen must be equal to the total log length in taper data!')
    }

    
#
#   solid type is irrelevant (NULL) if both the taper and log volume are passed;
#   otherwise, one or both are estimated below, so we must have a valid
#   parameter value for the equations...
#
    if(!missing(object) && !is.null(logVol) && !is.na(logVol)) {
      solidType = NULL
      if(!runQuiet)
          cat('\nNote: taper and volume supplied, solidType set to', solidType)
    }
    else {
      if(is.null(solidType)) {
        solidType = 3   #default
        if(!runQuiet)
          cat('\nNote: either taper or volume are missing, solidType set to default value of', solidType)
      }
    }
  

#
#   first just check the simple variables with dummy variables for taper, etc...
#
    if(missing(object))
      taper = data.frame(diameter=rep(NA,2), length=rep(NA,2))
    else
      taper = object
    log = new('downLog', buttDiam=buttDiam, topDiam=topDiam, logLen=logLen,
              logAngle=logAngle, solidType=solidType, taper=taper, species=species
             )

#
#   okay, valid object to here, just get the taper in terms of diameter and height
#   if it is missing...
#   
    if(missing(object))
      taper = .StemEnv$wbTaper(buttDiam, topDiam, logLen, nSegs, solidType)
    if(is.null(logVol) || is.na(logVol))
      logVol = .StemEnv$wbVolume(buttDiam, topDiam, logLen, solidType)
    
    
#
#   now, get the full profile for the outline as if the tree were standing upright
#   along the positive y-axis with the diameters center on x=0;
#   keep the duplicate point at the tip for truncation...
#
    rad = taper$diameter/2
    profile = data.frame( rad = c(-rad, rev(rad)) )          #center at diameter=x=0 
    profile$length = with(taper, c(length, rev(length)) )    #duplicate lengths too
    profile = rbind(profile, profile[1,])                    #close the polygon
    colnames(profile) = c('radius','length')
    np = nrow(profile)

#    
#   put the log on the ground, aligned with tip towards positive x-axis...
#
    halfLen = logLen/2
    rotLog = profile
    rotLog$hc = rep(1, np)                         #homogeneous coordinates
    rotLog = as.matrix(rotLog)
    trMat = transfMatrix(offset = c(0,-halfLen))   #to center at (0,0)
    rotLog = rotLog %*% trMat                      #translate
    trMat = transfMatrix(angle = -pi/2)            #lay the log down due East centered at (0,0)
    rotLog = rotLog %*% trMat

#
#   now rotate and translate to the desired position...
#
    trMat = transfMatrix(logAngle, offset = centerOffset) 
    rotLog = rotLog %*% trMat
    dimnames(rotLog) = list(NULL, c('x','y','hc'))
    needleAxis = matrix(c(-halfLen, halfLen, 0, 0, 1, 1), nrow=2) #row-major order
    needleAxis = needleAxis %*% trMat
    dimnames(needleAxis) = list(NULL, c('x','y','hc'))    

#
#   and make a SpatialPolygons object if sp is available...
#
    pgLog = Polygon(rotLog[,-3])        #sans hc
    pgsLog = Polygons(list(log=pgLog), ID=logID)
    spLog = SpatialPolygons(list(pgsLog=pgsLog),
                            proj4string = spUnits                       
                           )     #takes a list of Polygons objects

    
#
#   make the needle axis and the center point for the log...
#
    naLine = Line(needleAxis[,-3])  #no hc
    naLines = Lines(list(naLine=naLine), ID=paste(logID,'Needle',sep=':'))
    slNeedleAxis = SpatialLines(list(naLines=naLines),
                                proj4string = spUnits                                                    
                               )

    loc = matrix(centerOffset, nrow=1)
    colnames(loc) = names(centerOffset)
    location = SpatialPoints(loc,
                             proj4string = spUnits,        
                             bbox = bbox(spLog)              #same bbox as the log for consistency
                            )

#
#   now create it for real, it should be fine by this point...
#
    log = new('downLog', buttDiam=buttDiam, topDiam=topDiam, logLen=logLen,
              logAngle=logAngle, solidType=solidType, logVol = logVol, 
              taper=taper, profile=profile, rotLog = rotLog,
              spLog = spLog, slNeedleAxis=slNeedleAxis,
              units = units, spUnits = spUnits,
              location = location,
              description=description, userExtra = userExtra, species = species
             )

#
#   plot it if desired; uses class-specific method for plot generic...
#
    #if(wantPlot) 
     # plot(log, ...)

    if(!runQuiet)
      cat('\n')
    
    return(log)
}   #downLog method for data.frames
)   #setMethod


#showMethods('downLog')
