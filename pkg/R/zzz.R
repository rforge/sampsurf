#
# attaching the packages below is for the user, as they will want to plot objects
# for example, that may be raster or sp based. it may not be required at all
# eventually, I will have to do some checking on this, they may not be necessary; from
# the namespace article in 2003 w/rt .onLoad and .onAttach...
# "Many packages will not need either hook function, since import directives take the place of
# require calls and useDynLib directives can replace
# direct calls to library.dynam."

.onLoad <- function(lib, pkg) {
	#require(gpclib)
    #require(sp)
	#require(raster)
    #.defStemEnv(moveMiscFunctions=TRUE)
  
#
#   lock the environment and its bindings, otherwise, someone can make 
#   changes to it since it is a reference object...
#
    lockEnvironment(.StemEnv, bindings=TRUE)
}


#.onAttach = function(lib, pkg) {
#    .defStemEnv(moveMiscFunctions=TRUE)
#}



