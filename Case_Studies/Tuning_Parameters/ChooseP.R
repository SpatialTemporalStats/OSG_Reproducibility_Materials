################################################################################
# This file includes all steps to reproduce Figure S4                          #
################################################################################
# Packages, functions, and data
# library(R.matlab)
# library(ggplot2)
# library(patchwork)
# source(here("Functions","InverseTH.R"))
# source(here("Data","Data_Treatment.R"))
Col=c("#3288BD","#66C2A5","#ABDDA4","#E6F598","#FEE08B","#FDAE61","#F46D43","#D53E4F")


# Slepian Bases with Q=181 on the ARP
Eig.arp=Re(readMat(here("Slepian_ARP","Basis_reg_eig_value_181_real.mat"))$Basis.reg.eig.value)
Rerank.id=rank(-Eig.arp,ties.method = "first")
Eig.arp.rerank=rep(0,length(Eig.arp))
for(i in 1:length(Eig.arp)){
  id=which(Rerank.id==i)
  Eig.arp.rerank[i]=Eig.arp[id]
}
Q.sl=length(which(Re(Eig.arp)>=0.01)) 
Basis.SLP.pre=readMat(here("Slepian_ARP","Slepian_spatial_181.mat"))$Slepian.spatial
Basis.SLP=matrix(0,nrow(Dat.loc),length(Eig.arp))
for(i in 1:length(Eig.arp)){
  id=which(Rerank.id==i)
  Basis.SLP[,i]=Basis.SLP.pre[,id]
}
Basis.SLP.pre=0



###### Part 1. Obtain the Gaussianized coefficients of the first year
# t1=proc.time()[[3]]
# Obtain data from the the first year
R=10
T0=365*8
Windu.ARP1=Windu.ARP[,,1:T0]
Windv.ARP1=Windv.ARP[,,1:T0]
# Detrend
Windu.EnMean1=apply(Windu.ARP1,c(2,3),mean)
Windv.EnMean1=apply(Windv.ARP1,c(2,3),mean)
Windu.rsd1=Windv.rsd1=array(0,c(R,length(id.ARP),T0))
for(r in 1:R){
  Windu.rsd1[r,,]=Windu.ARP1[r,,]-Windu.EnMean1
  Windv.rsd1[r,,]=Windv.ARP1[r,,]-Windv.EnMean1
}
# Translate to the Slepian domain
A=solve(t(Basis.SLP[id.ARP,1:Q.sl])%*%Basis.SLP[id.ARP,1:Q.sl],t(Basis.SLP[id.ARP,1:Q.sl]))
Windu.SLP1=Windv.SLP1=array(0,c(R,Q.sl,T0))
for(r in 1:R){
  Windu.SLP1[r,,]=A%*%Windu.rsd1[r,,]
  Windv.SLP1[r,,]=A%*%Windv.rsd1[r,,]
}
v2hat.u1=v2hat.v1=matrix(0,length(id.ARP),T0)
for(r in 1:R){
  v2hat.u1=v2hat.u1+(Basis.SLP[id.ARP,1:Q.sl]%*%Windu.SLP1[r,,]-Windu.rsd1[r,,])^2
  v2hat.v1=v2hat.v1+(Basis.SLP[id.ARP,1:Q.sl]%*%Windv.SLP1[r,,]-Windv.rsd1[r,,])^2
}
v2hat.u1=v2hat.u1/R
v2hat.v1=v2hat.v1/R
# Tukey h
Gamma.u1=Gamma.v1=matrix(0,Q.sl,2)
TGHpara.u1=TGHpara.v1=matrix(0,Q.sl,2)
Windu.SLP.TGH1=Windv.SLP.TGH1=array(0,c(R,Q.sl,T0))
for(i in 1:Q.sl){
  Gamma.u1[i,1]=mean((Windu.SLP1[,i,])^2)
  Gamma.v1[i,1]=mean((Windv.SLP1[,i,])^2)
  Gamma.u1[i,2]=max(mean((Windu.SLP1[,i,])^4)/(Gamma.u1[i,1])^2,162/66)
  Gamma.v1[i,2]=max(mean((Windv.SLP1[,i,])^4)/(Gamma.v1[i,1])^2,162/66)
  
  TGHpara.u1[i,2]=max(sqrt(66*Gamma.u1[i,2]-162)-6,0)/66
  TGHpara.v1[i,2]=max(sqrt(66*Gamma.v1[i,2]-162)-6,0)/66
  TGHpara.u1[i,1]=sqrt(Gamma.u1[i,1])*((1-2*TGHpara.u1[i,2])^(3/4))
  TGHpara.v1[i,1]=sqrt(Gamma.v1[i,1])*((1-2*TGHpara.v1[i,2])^(3/4))
  
  Windu.SLP.TGH1[,i,]=THinvfunc(Windu.SLP1[,i,],TGHpara.u1[i,1],TGHpara.u1[i,2])
  Windv.SLP.TGH1[,i,]=THinvfunc(Windv.SLP1[,i,],TGHpara.v1[i,1],TGHpara.v1[i,2])
}
# t2=proc.time()[[3]]
# t2-t1=151.736


###### Part 2. Calculate the PACF
# t1=proc.time()[[3]]
PACF.u=PACF.v=array(0,c(4,Q.sl,Q.sl))
for(i in 1:Q.sl){
  for(r in 1:R){
    PACF.u[,i,i]=PACF.u[,i,i]+pacf(Windu.SLP.TGH1[r,i,],lag.max = 4,plot=FALSE)$acf[,,1]
    PACF.v[,i,i]=PACF.v[,i,i]+pacf(Windv.SLP.TGH1[r,i,],lag.max = 4,plot=FALSE)$acf[,,1]
  }
  if((i+1)<=Q.sl){
    for(j in (i+1):Q.sl){
      for(r in 1:R){
        a=pacf(data.frame(u1=Windu.SLP.TGH1[r,i,],v1=Windu.SLP.TGH1[r,j,]),lag.max = 4,plot=FALSE)
        PACF.u[,i,j]=PACF.u[,i,j]+a$acf[,1,2]
        PACF.u[,j,i]=PACF.u[,j,i]+a$acf[,2,1]
        a=pacf(data.frame(u1=Windv.SLP.TGH1[r,i,],v1=Windv.SLP.TGH1[r,j,]),lag.max = 4,plot=FALSE)
        PACF.v[,i,j]=PACF.v[,i,j]+a$acf[,1,2]
        PACF.v[,j,i]=PACF.v[,j,i]+a$acf[,2,1]
      }
    }
  }
}
PACF.u=PACF.u/R
PACF.v=PACF.v/R
# t2=proc.time()[[3]]
# t2-t1=3946.942

# t1=proc.time()[[3]]
PACF.uv=PACF.vu=array(0,c(4,Q.sl,Q.sl))
for(i in 1:Q.sl){
  for(j in 1:Q.sl){
    a=pacf(data.frame(u1=Windu.SLP.TGH1[r,i,],v1=Windv.SLP.TGH1[r,j,]),lag.max = 4,plot=FALSE)
    PACF.uv[,i,j]=PACF.uv[,i,j]+a$acf[,1,2]
    PACF.vu[,i,j]=PACF.vu[,i,j]+a$acf[,2,1]
  }
}
PACF.uv=PACF.uv/R
PACF.vu=PACF.vu/R
# t2=proc.time()[[3]]
# t2-t1=404.646




###### Part 3. Reproduce Figure S4
# t1=proc.time()[[3]]
# Plot Figure S4(a)
dataF=data.frame(V1=rep(1:Q.sl,each=Q.sl),V2=rep(seq(-1,-Q.sl,by=-1),times=Q.sl),
                 V3=c(PACF.u[1,,]),V4=c(PACF.u[2,,]),
                 V5=c(PACF.u[3,,]),V6=c(PACF.u[4,,]))
PT1=ggplot()+xlab(" ")+ylab(" ")+
  geom_point(aes(x=V1,y=V2,colour=V3),size=0.1,data=dataF,shape=15)+
  scale_colour_gradient2(low = "#313695", mid = "white",high ="#A50026",
                         midpoint = 0,space = "Lab",na.value = "grey50",
                         guide = "colourbar",aesthetics = "colour",limits=c(-0.21,0.72))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_blank(),
                   axis.title = element_blank(),
                   legend.title = element_blank(),
                   legend.position = "none",
                   legend.key.width=unit(0.5,"line"),
                   legend.key.height=unit(0.8,"line"))
PT2=ggplot()+xlab(" ")+ylab(" ")+
  geom_point(aes(x=V1,y=V2,colour=V4),size=0.1,data=dataF,shape=15)+
  scale_colour_gradient2("#313695", mid = "white",high ="#A50026",
                         midpoint = 0,space = "Lab",na.value = "grey50",
                         guide = "colourbar",aesthetics = "colour",limits=c(-0.21,0.72))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_blank(),
                   axis.title = element_blank(),
                   legend.title = element_blank(),
                   legend.position = "none",
                   legend.key.width=unit(0.5,"line"),
                   legend.key.height=unit(0.8,"line"))
PT3=ggplot()+xlab(" ")+ylab(" ")+
  geom_point(aes(x=V1,y=V2,colour=V5),size=0.1,data=dataF,shape=15)+
  scale_colour_gradient2(low = "#313695", mid = "white",high ="#A50026",
                         midpoint = 0,space = "Lab",na.value = "grey50",
                         guide = "colourbar",aesthetics = "colour",limits=c(-0.21,0.72))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_blank(),
                   axis.title = element_blank(),
                   legend.title = element_blank(),
                   legend.position = "none",
                   legend.key.width=unit(0.5,"line"),
                   legend.key.height=unit(0.8,"line"))
PT4=ggplot()+xlab(" ")+ylab(" ")+
  geom_point(aes(x=V1,y=V2,colour=V6),size=0.1,data=dataF,shape=15)+
  scale_colour_gradient2(low = "#313695", mid = "white",high ="#A50026",
                         midpoint = 0,space = "Lab",na.value = "grey50",
                         guide = "colourbar",aesthetics = "colour",limits=c(-0.21,0.72))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_blank(),
                   axis.title = element_blank(),
                   legend.title = element_blank(),
                   legend.position = "right",
                   legend.key.width=unit(0.5,"line"),
                   legend.key.height=unit(0.8,"line"))
print(PT1+PT2+PT3+PT4+plot_layout(ncol=4))     # 9.00*2.00

# Plot Figure S4(b)
dataF=data.frame(V1=rep(1:Q.sl,each=Q.sl),V2=rep(seq(-1,-Q.sl,by=-1),times=Q.sl),
                 V3=c(PACF.v[1,,]),V4=c(PACF.v[2,,]),
                 V5=c(PACF.v[3,,]),V6=c(PACF.v[4,,]))
PT1=ggplot()+xlab(" ")+ylab(" ")+
  geom_point(aes(x=V1,y=V2,colour=V3),size=0.1,data=dataF,shape=15)+
  scale_colour_gradient2(low = "#313695", mid = "white",high ="#A50026",
                         midpoint = 0,space = "Lab",na.value = "grey50",
                         guide = "colourbar",aesthetics = "colour",limits=c(-0.25,0.75))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_blank(),
                   axis.title = element_blank(),
                   legend.title = element_blank(),
                   legend.position = "none",
                   legend.key.width=unit(0.5,"line"),
                   legend.key.height=unit(0.8,"line"))
PT2=ggplot()+xlab(" ")+ylab(" ")+
  geom_point(aes(x=V1,y=V2,colour=V4),size=0.1,data=dataF,shape=15)+
  scale_colour_gradient2("#313695", mid = "white",high ="#A50026",
                         midpoint = 0,space = "Lab",na.value = "grey50",
                         guide = "colourbar",aesthetics = "colour",limits=c(-0.25,0.75))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_blank(),
                   axis.title = element_blank(),
                   legend.title = element_blank(),
                   legend.position = "none",
                   legend.key.width=unit(0.5,"line"),
                   legend.key.height=unit(0.8,"line"))
PT3=ggplot()+xlab(" ")+ylab(" ")+
  geom_point(aes(x=V1,y=V2,colour=V5),size=0.1,data=dataF,shape=15)+
  scale_colour_gradient2(low = "#313695", mid = "white",high ="#A50026",
                         midpoint = 0,space = "Lab",na.value = "grey50",
                         guide = "colourbar",aesthetics = "colour",limits=c(-0.25,0.75))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_blank(),
                   axis.title = element_blank(),
                   legend.title = element_blank(),
                   legend.position = "none",
                   legend.key.width=unit(0.5,"line"),
                   legend.key.height=unit(0.8,"line"))
PT4=ggplot()+xlab(" ")+ylab(" ")+
  geom_point(aes(x=V1,y=V2,colour=V6),size=0.1,data=dataF,shape=15)+
  scale_colour_gradient2(low = "#313695", mid = "white",high ="#A50026",
                         midpoint = 0,space = "Lab",na.value = "grey50",
                         guide = "colourbar",aesthetics = "colour",limits=c(-0.25,0.75))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_blank(),
                   axis.title = element_blank(),
                   legend.title = element_blank(),
                   legend.position = "right",
                   legend.key.width=unit(0.5,"line"),
                   legend.key.height=unit(0.8,"line"))
print(PT1+PT2+PT3+PT4+plot_layout(ncol=4))   # 9.00*2.00

# Plot Figure S4(c)
dataF=data.frame(V1=rep(1:Q.sl,each=Q.sl),V2=rep(seq(-1,-Q.sl,by=-1),times=Q.sl),
                 V3=c(PACF.uv[1,,]),V4=c(PACF.uv[2,,]),
                 V5=c(PACF.uv[3,,]),V6=c(PACF.uv[4,,]))
PT1=ggplot()+xlab(" ")+ylab(" ")+
  geom_point(aes(x=V1,y=V2,colour=V3),size=0.1,data=dataF,shape=15)+
  scale_colour_gradient2(low = "#313695", mid = "white",high ="#A50026",
                         midpoint = 0,space = "Lab",na.value = "grey50",
                         guide = "colourbar",aesthetics = "colour",limits=c(-0.02,0.03))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_blank(),
                   axis.title = element_blank(),
                   legend.title = element_blank(),
                   legend.position = "none",
                   legend.key.width=unit(0.5,"line"),
                   legend.key.height=unit(0.8,"line"))
PT2=ggplot()+xlab(" ")+ylab(" ")+
  geom_point(aes(x=V1,y=V2,colour=V4),size=0.1,data=dataF,shape=15)+
  scale_colour_gradient2("#313695", mid = "white",high ="#A50026",
                         midpoint = 0,space = "Lab",na.value = "grey50",
                         guide = "colourbar",aesthetics = "colour",limits=c(-0.02,0.03))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_blank(),
                   axis.title = element_blank(),
                   legend.title = element_blank(),
                   legend.position = "none",
                   legend.key.width=unit(0.5,"line"),
                   legend.key.height=unit(0.8,"line"))
PT3=ggplot()+xlab(" ")+ylab(" ")+
  geom_point(aes(x=V1,y=V2,colour=V5),size=0.1,data=dataF,shape=15)+
  scale_colour_gradient2(low = "#313695", mid = "white",high ="#A50026",
                         midpoint = 0,space = "Lab",na.value = "grey50",
                         guide = "colourbar",aesthetics = "colour",limits=c(-0.02,0.03))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_blank(),
                   axis.title = element_blank(),
                   legend.title = element_blank(),
                   legend.position = "none",
                   legend.key.width=unit(0.5,"line"),
                   legend.key.height=unit(0.8,"line"))
PT4=ggplot()+xlab(" ")+ylab(" ")+
  geom_point(aes(x=V1,y=V2,colour=V6),size=0.1,data=dataF,shape=15)+
  scale_colour_gradient2(low = "#313695", mid = "white",high ="#A50026",
                         midpoint = 0,space = "Lab",na.value = "grey50",
                         guide = "colourbar",aesthetics = "colour",limits=c(-0.02,0.03))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_blank(),
                   axis.title = element_blank(),
                   legend.title = element_blank(),
                   legend.position = "right",
                   legend.key.width=unit(0.5,"line"),
                   legend.key.height=unit(0.8,"line"))
print(PT1+PT2+PT3+PT4+plot_layout(ncol=4))

# Plot Figure S4(d)
dataF=data.frame(V1=rep(1:Q.sl,each=Q.sl),V2=rep(seq(-1,-Q.sl,by=-1),times=Q.sl),
                 V3=c(PACF.vu[1,,]),V4=c(PACF.vu[2,,]),
                 V5=c(PACF.vu[3,,]),V6=c(PACF.vu[4,,]))
PT1=ggplot()+xlab(" ")+ylab(" ")+
  geom_point(aes(x=V1,y=V2,colour=V3),size=0.1,data=dataF,shape=15)+
  scale_colour_gradient2(low = "#313695", mid = "white",high ="#A50026",
                         midpoint = 0,space = "Lab",na.value = "grey50",
                         guide = "colourbar",aesthetics = "colour",limits=c(-0.03,0.02))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_blank(),
                   axis.title = element_blank(),
                   legend.title = element_blank(),
                   legend.position = "none",
                   legend.key.width=unit(0.5,"line"),
                   legend.key.height=unit(0.8,"line"))
PT2=ggplot()+xlab(" ")+ylab(" ")+
  geom_point(aes(x=V1,y=V2,colour=V4),size=0.1,data=dataF,shape=15)+
  scale_colour_gradient2("#313695", mid = "white",high ="#A50026",
                         midpoint = 0,space = "Lab",na.value = "grey50",
                         guide = "colourbar",aesthetics = "colour",limits=c(-0.03,0.02))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_blank(),
                   axis.title = element_blank(),
                   legend.title = element_blank(),
                   legend.position = "none",
                   legend.key.width=unit(0.5,"line"),
                   legend.key.height=unit(0.8,"line"))
PT3=ggplot()+xlab(" ")+ylab(" ")+
  geom_point(aes(x=V1,y=V2,colour=V5),size=0.1,data=dataF,shape=15)+
  scale_colour_gradient2(low = "#313695", mid = "white",high ="#A50026",
                         midpoint = 0,space = "Lab",na.value = "grey50",
                         guide = "colourbar",aesthetics = "colour",limits=c(-0.03,0.02))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_blank(),
                   axis.title = element_blank(),
                   legend.title = element_blank(),
                   legend.position = "none",
                   legend.key.width=unit(0.5,"line"),
                   legend.key.height=unit(0.8,"line"))
PT4=ggplot()+xlab(" ")+ylab(" ")+
  geom_point(aes(x=V1,y=V2,colour=V6),size=0.1,data=dataF,shape=15)+
  scale_colour_gradient2(low = "#313695", mid = "white",high ="#A50026",
                         midpoint = 0,space = "Lab",na.value = "grey50",
                         guide = "colourbar",aesthetics = "colour",limits=c(-0.03,0.02))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_blank(),
                   axis.title = element_blank(),
                   legend.title = element_blank(),
                   legend.position = "right",
                   legend.key.width=unit(0.5,"line"),
                   legend.key.height=unit(0.8,"line"))
print(PT1+PT2+PT3+PT4+plot_layout(ncol=4))
# t2=proc.time()[[3]]
# t2-t1=16.182



