############# Functions to calculate the indices in Table 1 #############
library(fdaoutlier)
library(approxOT)



###### Index 1. Iuq for uncertainty
# Purpose: Calculate the Iuq index for emulations 
# Inputs: haty    (R by |G_ARP| by T) emulation array
#         y       (R by |G_ARP| by T) training data array
# Output: a vector of length |G_ARP| consisting of Iuq index value for emulations at all grid points

central.region.area=function(y){      
  # Calculate the central region area
  yn=ncol(y)
  R=nrow(y)
  res=modified_band_depth(y)
  id.res=which(rank(-res,ties.method = "first")<=round(R/2))
  centralRegion=y[id.res,]
  return(sum(apply(centralRegion,2,max)-apply(centralRegion,2,min)))
}

get.IUQ=function(haty,y){
  loclen=dim(y)[2]
  Iuq=rep(0,loclen)
  for(i in 1:loclen){
    hatyi=haty[,i,]
    yi=y[,i,]
    Iuq[i]=central.region.area(hatyi)/central.region.area(yi)
  }
  return(Iuq)
}



###### Index 2. Ibc for bivariate correlation
# Purpose: Calculate the bivariate correlations for emulations and training data
# Inputs: hatyrsd.u    (R by |G_ARP| by T) detrend U-component emulation array
#         hatyrsd.v    (R by |G_ARP| by T) detrend V-component emulation array
#         yrsd.u       (R by |G_ARP| by T) detrend U-component training data array
#         yrsd.v       (R by |G_ARP| by T) detrend V-component training data array
# Output: a vector of length 2*|G_ARP|, the first |G_ARP| elements are bivariate correlations for emulations, 
#         the remaining elements are for training data
get.IBC=function(hatyrsd.u,hatyrsd.v,yrsd.u,yrsd.v){
  loclen=dim(yrsd.u)[2]
  bc.emu=bc.dat=rep(0,loclen)
  for(i in 1:loclen){
    hatyrsd.ui=hatyrsd.u[,i,]
    hatyrsd.vi=hatyrsd.v[,i,]
    yrsd.ui=yrsd.u[,i,]
    yrsd.vi=yrsd.v[,i,]
    bc.emu[i]=mean(hatyrsd.ui*hatyrsd.vi)/(sd(c(hatyrsd.ui))*sd(c(hatyrsd.vi)))
    bc.dat[i]=mean(yrsd.ui*yrsd.vi)/(sd(c(yrsd.ui))*sd(c(yrsd.vi)))
  }
  return(c(bc.emu,bc.dat))
}



###### Index 3. Itc1 and Itc2 for bivariate correlation
# Purpose: Calculate the temporal correlations at lag=1 for emulations and training data
# Inputs: hatyrsd    (R by |G_ARP| by T) detrend emulation array
#         yrsd       (R by |G_ARP| by T) detrend training data array
# Output: a vector of length 2*|G_ARP|, the first |G_ARP| elements are temporal correlations at lag=1 for emulations, 
#         the remaining elements are for training data
get.ITC1=function(hatyrsd,yrsd){
  loclen=dim(yrsd)[2]
  TT=dim(yrsd)[3]
  tc1.emu=tc1.dat=rep(0,loclen)
  for(i in 1:loclen){
    tc1.emu[i]=mean(hatyrsd[,i,-TT]*hatyrsd[,i,-1])/var(c(hatyrsd[,i,]))
    tc1.dat[i]=mean(yrsd[,i,-TT]*yrsd[,i,-1])/var(c(yrsd[,i,]))
  }
  return(c(tc1.emu,tc1.dat))
}


# Purpose: Calculate the temporal correlations at lag=2 for emulations and training data
# Inputs: hatyrsd    (R by |G_ARP| by T) detrend emulation array
#         yrsd       (R by |G_ARP| by T) detrend training data array
# Output: a vector of length 2*|G_ARP|, the first |G_ARP| elements are temporal correlations at lag=2 for emulations, 
#         the remaining elements are for training data
get.ITC2=function(hatyrsd,yrsd){
  loclen=dim(yrsd)[2]
  TT=dim(yrsd)[3]
  tc2.emu=tc2.dat=rep(0,loclen)
  for(i in 1:loclen){
    tc2.emu[i]=mean(hatyrsd[,i,-c(TT-1,TT)]*hatyrsd[,i,-c(1,2)])/var(c(hatyrsd[,i,]))
    tc2.dat[i]=mean(yrsd[,i,-c(TT-1,TT)]*yrsd[,i,-c(1,2)])/var(c(yrsd[,i,]))
  }
  return(c(tc2.emu,tc2.dat))
}



###### Index 4. Itp for temporal persistent
# Purpose: Calculate the number of consecutive wind power generation time points (NCPGT) for emulations and training data
# Inputs: y.u    (R by |G_ARP| by T) data array
#         y.v    (R by |G_ARP| by T) data array
# Output: 4 by |G_ARP| matrix for NCPGT(,[0,5)), NCPGT(,[5,10)), NCPGT(,[10,20)), NCPGT(,[20,\infty))

get.ITP=function(y.u,y.v){
  R=dim(y.u)[1]
  loclen=dim(y.u)[2]
  NO.ITP=matrix(0,4,loclen)
  for(j in 1:loclen){
    for(r in 1:R){
      x=sqrt((y.u[r,j,]^2+y.v[r,j,]^2))*(7.5)^(1/7)
      idx=which(x<=25 & x>3)
      x.dur=c(1)
      for(i in 2:length(idx)){
        if(idx[i]-idx[i-1]>1){
          x.dur=c(x.dur,1)
        }
        if(idx[i]-idx[i-1]==1){
          x.dur=c(x.dur[-length(x.dur)],x.dur[length(x.dur)]+1)
        }
      }
      NO.ITP[,j]=NO.ITP[,j]+c(length(which(x.dur<5)),length(which(x.dur>=5 & x.dur<10)),
                              length(which(x.dur>=10 & x.dur<20)),length(which(x.dur>=20)))
    }
  }
  return(NO.ITP/R)
}



###### Index 5. Iwdt and Iwds for marginal distributions
# Purpose: Calculate the Iwdt index for emulations
# Inputs: haty    (R by |G_ARP| by T) emulation array
#         y       (R by |G_ARP| by T) training data array
# Output: a vector of length |G_ARP| consisting of Iwdt index value for emulations at all grid points

get.IWDT=function(haty,y){
  loclen=dim(y)[2]
  Iwdt=rep(0,loclen)
  for(i in 1:loclen){
    hatyi=c(haty[,i,])
    yi=c(y[,i,])
    Iwdt[i]=wasserstein(X=hatyi,Y=yi,p=1,ground_p = 1,observation.orientation="colwise",method="univariate")
  }
  return(Iwdt)
}


# Purpose: Calculate the Iwds index for emulations
# Inputs: haty    (R by |G_ARP| by T) emulation array
#         y       (R by |G_ARP| by T) training data array
# Output: a vector of length T consisting of Iwds index value for emulations at all time points

get.IWDS=function(haty,y){
  TT=dim(y)[3]
  Iwds=rep(0,TT)
  for(t in 1:TT){
    hatyt=c(haty[,,t])
    yt=c(y[,,t])
    Iwds[t]=wasserstein(X=hatyt,Y=yt,p=1,ground_p = 1,observation.orientation="colwise",method="univariate")
  }
  return(Iwds)
}






