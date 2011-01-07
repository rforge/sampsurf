#---------------------------------------------------------------------------
#
#   Hidden global environment for class 'Stem' stuff.
#
#   Note that this now holds constants/parameters, etc. for other classes
#   within sampSurf as well. JHG 16-Dec-2010.
#
#   To do all of the setup for the environment...
#     -- .defStemEnv
#   is run last thing in the file.
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





#---------------------------------------------------------------------------
#
#   Specifically for 'Stem' class and subclass objects and pertinent methods.
#
#   Creates a hidden environment for any global constants that might be
#   shared among different routines. Just run this to re-create. Add any
#   new assignments below.
#   
#---------------------------------------------------------------------------
#
#   create hidden environment to store things in within the package...
#
.StemEnv = new.env()



#
#   units of measure types, conversions and constants...
#
.StemEnv$msrUnits = list(English='English', metric='metric')
.StemEnv$cm2m = 1/100
.StemEnv$m2cm = 100
.StemEnv$in2ft = 1/12
.StemEnv$ft2in = 12

.StemEnv$smpHectare = 10000
.StemEnv$sfpAcre = 43560

#
# per unit area names for the list object slot in the InclusionZone class or subclasses;
# I have perhaps made this too difficult, but one can assign the slots based on the names
# of the list, while the values could someday change; e.g.
# eval(parse(text=paste('list(',.StemEnv$puaEstimates$volumeCubic,'=3)')))
# will do it...
#
.StemEnv$puaEstimates = list(cubicVolume = 'cubicVolume',          #cubic volume in correct units
                             bfVolume = 'bfVolume',                #board foot volume
                             Density = 'Density'                   #number of logs or trees
                            )
#.StemEnv$puaNames = list(English =
#                          list(cubicVolume = 'volume in cubic feet per acre',
#                               bfVolume = 'board foot volume per acre',
#                               logDensity = 'number of logs per acre'
#                              ),
#                          metric =
#                           list(cubicVolume = 'volume in cubic meters per hectare',
#                                bfVolume = NULL,
#                                logDensity = 'number of logs per hectare'
#                               )
#                         ) #puaNames

#
#   some plausible species codes/names--note that they can be any character string...
#
.StemEnv$species = c('wp','rm','sm','hemlock','Picea glauca','shagbark hickory','BW')


#
#   possible log lie angles & other angular constants...
#
.StemEnv$logAngles = c(0, 2*pi)
.StemEnv$deg2rad = pi/180
.StemEnv$rad2deg = 1/.StemEnv$deg2rad


#
#   some useful defaults for plotting consistently in different classes...
#
.StemEnv$alphaTrans = 0.5


#log colors, etc.
.StemEnv$logBorderColor = 'brown4'  #perimeter color of downLog objects
.StemEnv$logColor = transparentColorBase('sandybrown', .StemEnv$alphaTrans)  #internal shade for down logs
.StemEnv$logAttributeColor = transparentColorBase('tan3',              #needle & center color
                                    ifelse(1.5*.StemEnv$alphaTrans>1, 1, 1.5*.StemEnv$alphaTrans))

#inclusion zones or plot-related...
.StemEnv$izBorderColor = transparentColorBase('slategray',                      #plot perimeter color
                                    ifelse(1.5*.StemEnv$alphaTrans>1, 1, 1.5*.StemEnv$alphaTrans)) 
.StemEnv$izColor = transparentColorBase('gray95', .StemEnv$alphaTrans)  #interior color for sample plots
.StemEnv$izCenterColor = transparentColorBase('slategray', .StemEnv$alphaTrans)  #center point color


#zero=lightest
.StemEnv$blue.colors = colorRampPalette(c('lightsteelblue1','steelblue','steelblue4'))
.StemEnv$gray.colors = colorRampPalette(c('grey90','grey50'))
#blue.colors = colorRampPalette(c('lightsteelblue1','steelblue','steelblue4'))
#zero=darkest...
#.StemEnv$blue.colors = colorRampPalette(c('steelblue4','steelblue','lightsteelblue'))
#blue.colors = colorRampPalette(c('steelblue4','steelblue','lightsteelblue'))

.StemEnv$gridLineColor = transparentColorBase('slategray',.StemEnv$alphaTrans)
.StemEnv$gridCenterColor = transparentColorBase('firebrick4',.StemEnv$alphaTrans)


#
#   sampleLogs stuff...
#
.StemEnv$sampleLogsNames = c('species', 'buttDiam', 'topDiam', 'logLen', 'solidType',
                             'x', 'y', 'logAngle', 'logAngle.D')


#
#   taper/volume stuff...
#
.StemEnv$solidTypes = c(1,10)  #range for valid solidType in log taper/volume



#================================================================================
#
#  default taper function...
#
#  Note that the taper function must have diameters in the same units as length,
#  so if length is in meters, diameters must be as well...
#
#  Arguments...
#    botDiam = the diameter at the large end in height/length units
#    topDiam = the diameter at the small end in height/length units
#    logLen = the length of the log
#    nSegs = the number of segments desired
#    solidType = between 1 and 10 is legal
#    hgt = NULL to calculate sections here; otherwise a vector of hgt/length
#          section information
#    isLog = TRUE: a down log, so use "length" in taper; FALSE: a standing tree
#            so use "hgt" in taper
#
#   Note: little error checking has been added yet!!!!
#
wbTaper = function(botDiam, topDiam, logLen, nSegs=20, solidType, hgt=NULL, isLog=TRUE) {
    if(nSegs < 1)
      stop('Must have positive number of log segments for taper!')
    nSegs = nSegs + 1               #becomes the number of diameters required for taper
    if(is.null(solidType) || solidType < .StemEnv$solidTypes[1] || solidType > .StemEnv$solidTypes[2])
      stop('solidType=',solidType,' out of range, must be in: (',solidType[1],',',solidType[2],')')
    r = solidType
    
    if(is.null(hgt))
      hgt = seq(0, logLen, length.out=nSegs)
    diameter = topDiam + (botDiam - topDiam) * ((logLen - hgt)/logLen)^(2/r)
    taper = data.frame(diameter=diameter, hgt=hgt)
    if(isLog)
      colnames(taper) = c('diameter','length')
    return(taper)
}   #wbTaper
assign('wbTaper', wbTaper, envir=.StemEnv)            #move to .StemEnv
environment(.StemEnv$wbTaper) = .StemEnv              #assign its environment
rm(wbTaper)                                           #and remove from .GlobalEnv



#================================================================================
#
#  default volume function...
#
#  k is the conversion factor that takes diameter to radius and puts it into the
#  same units as length. But diameters should be in length units for downLogs,
#  so k just represents taking squared diameter to squared radius...
#
wbVolume = function(botDiam, topDiam, logLen, solidType, boltLen=NULL) {
    if(is.null(solidType) || solidType < .StemEnv$solidTypes[1] || solidType > .StemEnv$solidTypes[2])
      stop('solidType=',solidType,' out of range, must be in: (',solidType[1],',',solidType[2],')')
    r = solidType
    k = 1/4                              #diameter to radius; diam to length units conversion==1
    if(is.null(boltLen))
      h = logLen                         #any height/length is possible, default is for total volume
    else
      h = boltLen                        #some intermediate volume
                                         
    logVol = pi*k*topDiam^2*h +
             pi*k*logLen*(botDiam - topDiam)^2*r/(4+r)*(1-(1-h/logLen)^(4+r/r)) +
             2*pi*k*logLen*topDiam*(botDiam - topDiam)*r/(2+r)*(1-(1-h/logLen)^(2+r/r))
    #logVol = pi*k*logLen*((buttD-topD)^2*(r/(r+4)) + topD*(topD + 2*(buttD-topD)*(r/(r+2))))
    return(logVol)
}   #wbVolume
assign('wbVolume', wbVolume, envir=.StemEnv)               #move to .StemEnv
environment(.StemEnv$wbVolume) = .StemEnv                  #assign its environment
rm(wbVolume)                                               #and remove from .GlobalEnv




#================================================================================
#
#  Smalian's volume function for passed taper...
#
#  k is the conversion factor that takes diameter to radius and puts it into the
#  same units as length. But diameters should be in length units for downLogs,
#  so k just represents taking squared diameter to squared radius...
#
SmalianVolume = function(taper, isLog=TRUE) {
    k = 1/4                              #diameter to radius; diam to length units conversion==1
    nSegs = nrow(taper) - 1
    if(nSegs < 1)
      stop("Must have positive number of log segments for Smalian's!")
    if(isLog)
      hgtName = 'length'
    else
      hgtName = 'hgt'
    vol = 0
    diam = taper[,'diameter']
    csArea = diam^2
    length = taper[,hgtName]
    for(i in 1:nSegs) {
      sectLen = length[i+1] - length[i]
      if(isTRUE(all.equal(diam[i+1],0.0)))
        vol = vol + pi*k*csArea[i+1]*sectLen/3                 #cone for tip
      else
        vol = vol + pi*k*(csArea[i] + csArea[i+1])*sectLen/2   #Smalian's
    }
    return(vol)
}   #SmalianVolume
assign('SmalianVolume', SmalianVolume, envir=.StemEnv)     #move to .StemEnv
environment(.StemEnv$SmalianVolume) = .StemEnv             #assign its environment
rm(SmalianVolume)                                          #and remove from .GlobalEnv

  



#================================================================================
#
#  converts degrees to radians on [0,2pi]...
#
.deg2Rad = function(angle) {
    twoPi = 2.0*pi
    if(angle > 360) {
      fact = floor(angle/360)    
      angle = angle - fact*360
    }
    return(angle*twoPi/360)
} #.deg2Rad
assign('deg2Rad', .deg2Rad, envir=.StemEnv)               #move to .StemEnv
environment(.StemEnv$deg2Rad) = .StemEnv                  #assign its environment
rm(.deg2Rad)                                              #and remove from .GlobalEnv

#================================================================================
#
#  converts radians to degrees on [0,360]...
#

.rad2Deg = function(angle) {
    twoPi = 2.0*pi
    if(angle > twoPi) {
      fact = floor(angle/twoPi)    
      angle = angle - fact*twoPi
    }
    return(angle*360/twoPi)
} #.rad2Deg
assign('rad2Deg', .rad2Deg, envir=.StemEnv)               #move to .StemEnv
environment(.StemEnv$rad2Deg) = .StemEnv                  #assign its environment
rm(.rad2Deg)                                              #and remove from .GlobalEnv
  
  





#================================================================================
#
.underLine = function(lineLength = 0,   #length of line
                      lineChar = '-',   #character for line
                      prologue = '\n',  #some character (vector) to be output first
                      postfix = '\n'    #character (vector) to be output last
                     )
{
#------------------------------------------------------------------------------
#   just cat()'s an underline for nicer output...
#------------------------------------------------------------------------------
    if(lineLength>0 && lineLength<200)
      cat(prologue,rep(lineChar, lineLength),postfix,sep='')
    return(invisible())
}   #.underLine
assign('underLine', .underLine, envir=.StemEnv)           #move to .StemEnv
environment(.StemEnv$underLine) = .StemEnv                #assign its environment
rm(.underLine)                                            #and remove from .GlobalEnv




