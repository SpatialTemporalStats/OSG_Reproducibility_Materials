################################################################################
# This file includes all steps to reproduce Figure S2                          #
################################################################################
# Necessary packages 
library(geoR)
cbPalette=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Get data on five random time points to illustrate the variogram
set.seed(1)
t.choose=sample(1:29200,5)
Windu.Enmean=apply(Windu.ARP[,,t.choose],c(2,3),mean)
Windu.Rsd=sweep(Windu.ARP[,,t.choose],c(2,3),Windu.Enmean,"-")
ReDiv=which(Dat.loc.arp[,1]<46.5)   # Split the ARP into western and eastern subregions


# Variogram (Direction = 0)
vario.left.dir0.lag=variog(coords=Dat.loc.arp[ReDiv,],data=Windu.Rsd[1,ReDiv,1],max.dist=15,direction=0)$u
vario.right.dir0.lag=variog(coords=Dat.loc.arp[-ReDiv,],data=Windu.Rsd[1,-ReDiv,1],max.dist=15,direction=0)$u
vario.left.dir0=matrix(0,length(vario.left.dir0.lag),5)
vario.right.dir0=matrix(0,length(vario.right.dir0.lag),5)
for(t in 1:5){
  for(r in 1:R){
    vario.left.dir0[,t]=vario.left.dir0[,t]+variog(coords=Dat.loc.arp[ReDiv,],data=Windu.Rsd[r,ReDiv,t],max.dist=15,direction=0)$v
    vario.right.dir0[,t]=vario.right.dir0[,t]+variog(coords=Dat.loc.arp[-ReDiv,],data=Windu.Rsd[r,-ReDiv,t],max.dist=15,direction=0)$v
  }
}
vario.left.dir0=vario.left.dir0/R
vario.right.dir0=vario.right.dir0/R

dataF=data.frame(lag=rep(vario.left.dir0.lag,times=10),
                 variog.left=c(vario.left.dir0),
                 variog.right=c(vario.right.dir0),
                 type=as.factor(rep(c("17401","24388","4775","26753","13218"),each=length(vario.left.dir0.lag))))
PT=ggplot()+
  geom_point(mapping=aes(lag,variog.left,color=type),data=dataF)+
  geom_line(mapping=aes(lag,variog.left,color=type),data=dataF)+
  scale_y_continuous(limits=c(0,0.25))+
  scale_color_manual(values=cbPalette[1:5])+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position ="none",
                   legend.title = element_text(),
                   legend.key.width=unit(0.5,"line"),
                   legend.key.height=unit(0.7,"line"))+
  labs(color="t=")+ylab("Variogram")+xlab("Lag")
print(PT)
PT=ggplot()+
  geom_point(mapping=aes(lag,variog.right,color=type),data=dataF)+
  geom_line(mapping=aes(lag,variog.right,color=type),data=dataF)+
  scale_y_continuous(limits=c(0,0.25))+
  scale_color_manual(values=cbPalette[1:5])+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,1),
                   legend.position = c(0.01,0.99),
                   legend.title = element_text(),
                   legend.key.width=unit(0.5,"line"),
                   legend.key.height=unit(0.7,"line"))+
  labs(color="t=")+ylab("Variogram")+xlab("Lag")
print(PT)


# Variogram (Direction = 45)
vario.left.dir0.lag=variog(coords=Dat.loc.arp[ReDiv,],data=Windu.Rsd[1,ReDiv,1],max.dist=15,direction=pi/4)$u
vario.right.dir0.lag=variog(coords=Dat.loc.arp[-ReDiv,],data=Windu.Rsd[1,-ReDiv,1],max.dist=15,direction=pi/4)$u
vario.left.dir0=matrix(0,length(vario.left.dir0.lag),5)
vario.right.dir0=matrix(0,length(vario.right.dir0.lag),5)
for(t in 1:5){
  for(r in 1:R){
    vario.left.dir0[,t]=vario.left.dir0[,t]+variog(coords=Dat.loc.arp[ReDiv,],data=Windu.Rsd[r,ReDiv,t],max.dist=15,direction=pi/4)$v
    vario.right.dir0[,t]=vario.right.dir0[,t]+variog(coords=Dat.loc.arp[-ReDiv,],data=Windu.Rsd[r,-ReDiv,t],max.dist=15,direction=pi/4)$v
  }
}
vario.left.dir0=vario.left.dir0/R
vario.right.dir0=vario.right.dir0/R

dataF=data.frame(lag=rep(vario.left.dir0.lag,times=10),
                 variog.left=c(vario.left.dir0),
                 variog.right=c(vario.right.dir0),
                 type=as.factor(rep(c("17401","24388","4775","26753","13218"),each=length(vario.left.dir0.lag))))
PT=ggplot()+
  geom_point(mapping=aes(lag,variog.left,color=type),data=dataF)+
  geom_line(mapping=aes(lag,variog.left,color=type),data=dataF)+
  scale_y_continuous(limits=c(0,0.25))+
  scale_color_manual(values=cbPalette[1:5])+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position ="none",
                   legend.title = element_text(),
                   legend.key.width=unit(0.5,"line"),
                   legend.key.height=unit(0.7,"line"))+
  labs(color="t=")+ylab("Variogram")+xlab("Lag")
print(PT)
PT=ggplot()+
  geom_point(mapping=aes(lag,variog.right,color=type),data=dataF)+
  geom_line(mapping=aes(lag,variog.right,color=type),data=dataF)+
  scale_y_continuous(limits=c(0,0.25))+
  scale_color_manual(values=cbPalette[1:5])+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,1),
                   legend.position = "none",
                   legend.title = element_text(),
                   legend.key.width=unit(0.5,"line"),
                   legend.key.height=unit(0.7,"line"))+
  labs(color="t=")+ylab("Variogram")+xlab("Lag")
print(PT)


# Variogram (Direction = 90)
vario.left.dir0.lag=variog(coords=Dat.loc.arp[ReDiv,],data=Windu.Rsd[1,ReDiv,1],max.dist=15,direction=pi/2)$u
vario.right.dir0.lag=variog(coords=Dat.loc.arp[-ReDiv,],data=Windu.Rsd[1,-ReDiv,1],max.dist=15,direction=pi/2)$u
vario.left.dir0=matrix(0,length(vario.left.dir0.lag),5)
vario.right.dir0=matrix(0,length(vario.right.dir0.lag),5)
for(t in 1:5){
  for(r in 1:R){
    vario.left.dir0[,t]=vario.left.dir0[,t]+variog(coords=Dat.loc.arp[ReDiv,],data=Windu.Rsd[r,ReDiv,t],max.dist=15,direction=pi/2)$v
    vario.right.dir0[,t]=vario.right.dir0[,t]+variog(coords=Dat.loc.arp[-ReDiv,],data=Windu.Rsd[r,-ReDiv,t],max.dist=15,direction=pi/2)$v
  }
}
vario.left.dir0=vario.left.dir0/R
vario.right.dir0=vario.right.dir0/R

dataF=data.frame(lag=rep(vario.left.dir0.lag,times=10),
                 variog.left=c(vario.left.dir0),
                 variog.right=c(vario.right.dir0),
                 type=as.factor(rep(c("17401","24388","4775","26753","13218"),each=length(vario.left.dir0.lag))))
PT=ggplot()+
  geom_point(mapping=aes(lag,variog.left,color=type),data=dataF)+
  geom_line(mapping=aes(lag,variog.left,color=type),data=dataF)+
  scale_y_continuous(limits=c(0,0.25))+
  scale_color_manual(values=cbPalette[1:5])+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position ="none",
                   legend.title = element_text(),
                   legend.key.width=unit(0.5,"line"),
                   legend.key.height=unit(0.7,"line"))+
  labs(color="t=")+ylab("Variogram")+xlab("Lag")
print(PT)
PT=ggplot()+
  geom_point(mapping=aes(lag,variog.right,color=type),data=dataF)+
  geom_line(mapping=aes(lag,variog.right,color=type),data=dataF)+
  scale_y_continuous(limits=c(0,0.25))+
  scale_color_manual(values=cbPalette[1:5])+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,1),
                   legend.position = "none",
                   legend.title = element_text(),
                   legend.key.width=unit(0.5,"line"),
                   legend.key.height=unit(0.7,"line"))+
  labs(color="t=")+ylab("Variogram")+xlab("Lag")
print(PT)


# Variogram (Direction = 135)
vario.left.dir0.lag=variog(coords=Dat.loc.arp[ReDiv,],data=Windu.Rsd[1,ReDiv,1],max.dist=15,direction=pi/4*3)$u
vario.right.dir0.lag=variog(coords=Dat.loc.arp[-ReDiv,],data=Windu.Rsd[1,-ReDiv,1],max.dist=15,direction=pi/4*3)$u
vario.left.dir0=matrix(0,length(vario.left.dir0.lag),5)
vario.right.dir0=matrix(0,length(vario.right.dir0.lag),5)
for(t in 1:5){
  for(r in 1:R){
    vario.left.dir0[,t]=vario.left.dir0[,t]+variog(coords=Dat.loc.arp[ReDiv,],data=Windu.Rsd[r,ReDiv,t],max.dist=15,direction=pi/4*3)$v
    vario.right.dir0[,t]=vario.right.dir0[,t]+variog(coords=Dat.loc.arp[-ReDiv,],data=Windu.Rsd[r,-ReDiv,t],max.dist=15,direction=pi/4*3)$v
  }
}
vario.left.dir0=vario.left.dir0/R
vario.right.dir0=vario.right.dir0/R

dataF=data.frame(lag=rep(vario.left.dir0.lag,times=10),
                 variog.left=c(vario.left.dir0),
                 variog.right=c(vario.right.dir0),
                 type=as.factor(rep(c("17401","24388","4775","26753","13218"),each=length(vario.left.dir0.lag))))
PT=ggplot()+
  geom_point(mapping=aes(lag,variog.left,color=type),data=dataF)+
  geom_line(mapping=aes(lag,variog.left,color=type),data=dataF)+
  scale_y_continuous(limits=c(0,0.25))+
  scale_color_manual(values=cbPalette[1:5])+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position ="none",
                   legend.title = element_text(),
                   legend.key.width=unit(0.5,"line"),
                   legend.key.height=unit(0.7,"line"))+
  labs(color="t=")+ylab("Variogram")+xlab("Lag")
print(PT)
PT=ggplot()+
  geom_point(mapping=aes(lag,variog.right,color=type),data=dataF)+
  geom_line(mapping=aes(lag,variog.right,color=type),data=dataF)+
  scale_y_continuous(limits=c(0,0.25))+
  scale_color_manual(values=cbPalette[1:5])+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,1),
                   legend.position = "none",
                   legend.title = element_text(),
                   legend.key.width=unit(0.5,"line"),
                   legend.key.height=unit(0.7,"line"))+
  labs(color="t=")+ylab("Variogram")+xlab("Lag")
print(PT)

