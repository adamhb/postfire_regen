facing <- function(slope,aspect,focal=180,unit='rad') {
  if (unit %in% c('rad','deg')) {
    if (unit=='deg') {
      slope <- d2r(slope)
      aspect <- d2r(aspect)
    }
    aspect <- d2r(focal) - aspect
    return(sin(slope) * cos(aspect))
  } else print('unit must be rad or deg')
}

eastness <- function(slope,aspect,unit='deg') facing(slope,aspect,focal=90,unit=unit)
northness <- function(slope,aspect,unit='deg') facing(slope,aspect,focal=0,unit=unit)
southness <- function(slope,aspect,unit='deg') facing(slope,aspect,focal=180,unit=unit)

thl <- function(L,A,S,unit='deg',fold=180) {
    folded <- function(x,f=fold) abs(180-abs(x-f))
    d2r <- function(x) 2*pi*x/360
    r2d <- function(x) 360*x/(2*pi)
    if (unit=='deg') {
        A <- folded(A,fold)
        L <- d2r(L)
        A <- d2r(A)
        S <- d2r(S)
    } else {
      atmp <- r2d(A)
      atmp <- folded(atmp,fold)
      A <- d2r(atmp)
    }
    # EQN 3, McCune and Keon 2002 JVS, latitude > 30‚ slope < 60°
    return(0.339+0.808*cos(L)*cos(S)-0.196*sin(L)*sin(S)-0.482*cos(A)*sin(S))
}
# L=37
# A = 180
# S = 20
# A=0:360
# plot(A,thl(L,A,S,fold=225))
# points(A,thl(L,A,S,fold=225))




