
#=============================================================================

# Example script loading data for Neighbourhood Scale Flow Regimes and Pollution Transport in Cities
# Dec 2020
# Ed Bannister 
# github.com/foresteddy
# [PATH] refers to the user's file path

#=============================================================================
# 1. DEFINING VARIABLES AND LOADING PACKAGES
#==========================

rm(list = ls(all=TRUE)) 
setwd("[PATH]")
library("fields")
library("magick")
south_north = 191
bottom_top = 79 
dy=10 # GRID RESOLUTION
zz=read.table("[PATH]/Z.txt",sep="",skip=1)
zz[,7] <- zz[,2]/21.69 # NORMALISES BY GRID INDENX HEIGHT
Lz=zz[,7]
Ly=(1:south_north)*dy/20 # NORMALISES BY THE HEIGHT OF THE CANOPY
kp=38
Lzrg=c(0,Lz[kp])
Lxrg=range(Ly)


#=============================================================================
# 2. IMPORTING THE SIMULATED DATA
#==========================

filein.uni="[PATH]/Uniform_case.txt"
filein.4h="[PATH]/4h_case.txt"
filein.6h="[PATH]/6h_case.txt"
filein.8h="[PATH]a/8h_case.txt"
filein.12h="[PATH]/12h_case.txt"
filein.16h="[PATH]/16h_case.txt"

# STATISTICS
# 1. mean
# 2. kinematic vertical turbulent flux
# 3. variance
# 4. sd*mean (not used in article)
# 5. mean vertical flux
# 6. skewness
# 7. kurtosis (absolute, not excess. i.e. kurt(normal distribution) = 3)
stats=c("(c)","wc","cc","c2C","WC","Skew_c","Kurt_c")

# VARIABLES
# velocity components and scalars released at various heights. Only Sclr2 (z=3m) was used in the analysis
vars=c('w','v','u','Sclr1','Sclr2','Sclr3', 'Sclr4', 'Sclr5', 'Sclr6', 'Sclr7', 'Sclr8')

nstats=length(stats)
nvars=length(vars)

#=== scan the data and produce 2D arrays

s2D.all.of.uni=array(scan(filein.uni),c(south_north,bottom_top,nstats,nvars))
s2D.all.of.4h=array(scan(filein.4h),c(south_north,bottom_top,nstats,nvars))
s2D.all.of.6h=array(scan(filein.6h),c(south_north,bottom_top,nstats,nvars))
s2D.all.of.8h=array(scan(filein.8h),c(south_north,bottom_top,nstats,nvars))
s2D.all.of.12h=array(scan(filein.12h),c(south_north,bottom_top,nstats,nvars))
s2D.all.of.16h=array(scan(filein.16h),c(south_north,bottom_top,nstats,nvars))

#=============================================================================
# 3. DEFINE TKE AND NORMALISING VARIABLES
#==========================

s2D.TKE.uni=0.5*(s2D.all.of.uni[,,3,1]+s2D.all.of.uni[,,3,2]+s2D.all.of.uni[,,3,3])
s2D.TKE.4h=0.5*(s2D.all.of.4h[,,3,1]+s2D.all.of.4h[,,3,2]+s2D.all.of.4h[,,3,3])
s2D.TKE.6h=0.5*(s2D.all.of.6h[,,3,1]+s2D.all.of.6h[,,3,2]+s2D.all.of.6h[,,3,3])
s2D.TKE.8h=0.5*(s2D.all.of.8h[,,3,1]+s2D.all.of.8h[,,3,2]+s2D.all.of.8h[,,3,3])
s2D.TKE.12h=0.5*(s2D.all.of.12h[,,3,1]+s2D.all.of.12h[,,3,2]+s2D.all.of.12h[,,3,3])
s2D.TKE.16h=0.5*(s2D.all.of.16h[,,3,1]+s2D.all.of.16h[,,3,2]+s2D.all.of.16h[,,3,3])

# defining reference wind speed for normalized plots
uref <- 0.5*(sqrt(mean(s2D.all.of.uni[,49,1,3]**2+s2D.all.of.uni[,49,1,2]**2))
             +sqrt(mean(s2D.all.of.uni[,50,1,3]**2+s2D.all.of.uni[,50,1,2]**2)))

# calculate ustar and cstar at canopy top
ustar <- mean(((s2D.all.of.uni[,21,2,2])^2+(s2D.all.of.uni[,21,2,3])^2)^0.25)

cstar <- mean((s2D.all.of.uni[,21,3,5])^0.5)

