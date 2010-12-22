#---------------------------------------------------------------------------
#
#   This file holds the S4 class union definitions that are used in the 
#   sampSurf package classes.
#
#   classUnion...
#   1. missCharacter
#   2. numericNull
#
#Author...									Date: 26-Aug-2010
#	Jeffrey H. Gove
#	USDA Forest Service
#	Northern Research Station
#	271 Mast Road
#	Durham, NH 03824
#	jhgove@unh.edu
#	phone: 603-868-7667	fax: 603-868-7604
#---------------------------------------------------------------------------
# in general, the following is true for class unions: we use
# NULL to denote missing rather than NA because NULL is a class, but NA
# is of class logical, so we'd have to allow TRUE and FALSE if we added
# class logical to include NA; the missing sense here is NOT to
# be confused with missing() and 'missing' for a missing argument...

#
# general CRS class that allows NULL if not present (missing); 
# see above for reasoning on NULL vs NA; note that the 'sp'
# package must be available for this class union to be valid...
#
#setClassUnion('missCRS', c('CRS','NULL'))

#
# general character class that allows NULL if not present (missing); 
# see above for reasoning on NULL vs NA...
#
setClassUnion('missCharacter', c('character','NULL'))

setClassUnion('numericNULL', c('numeric', 'NULL'))

#setClassUnion('numericVector', c('numeric', 'vector'))



#
# general data frame class that allows 'missing' in function arguments... 
#
setClassUnion('missDF', c('data.frame','missing'))

