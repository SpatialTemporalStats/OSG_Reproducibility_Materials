################################################################################
# This file includes all steps to reproduce Figure 2                           #
################################################################################
# ! Note 1. Before running the code, please download the required Slepian bases and their eigenvalues.
#           First, please download "Slepian_ARP.zip" from https://zenodo.org/records/16655516.
#           Then, please extract the contents of "Slepian_ARP.zip" and save all .m files in the sub-repository "Slepian_ARP".
# ! Note 2. On the same website, we provide Matlab code in "Slepian_Code.zip" for
#           generating these Slepian bases or adapting the approach for related applications.
#           The code applies the method proposed by [Bates et al. (2017)](https://dl.acm.org/doi/abs/10.1109/TSP.2017.2712122) to ARP.
 
# Necessary packages
library(R.matlab)
library(ggplot2)



# Necessary ARP information
load(here("Data","IPCC-WGI-reference-regions-v4_R.rda"))
ARP=IPCC_WGI_reference_regions_v4@polygons[['ARP']]
ARP.border=ARP@Polygons[[1]]@coords
dataARPb=data.frame(lon=ARP.border[,1],lat=ARP.border[,2])



###### Part 1. Reproduce Figure 2(a)
# t1=proc.time()[[3]]
# Load Slepian bases with Q=181 and their eigenvalues
Eig.arp=Re(readMat(here("Slepian_ARP","Basis_reg_eig_value_181_real.mat"))$Basis.reg.eig.value) 
Rerank.id=rank(-Eig.arp,ties.method = "first")
Eig.arp.rerank=rep(0,length(Eig.arp))              # Rerank the eigenvalues \lambda_{\alpha}
for(i in 1:length(Eig.arp)){
  id=which(Rerank.id==i)
  Eig.arp.rerank[i]=Eig.arp[id]
}
Q.sl=length(which(Re(Eig.arp)>=0.01))              # Choose A such that \lambda_A is about 0.01
Basis.SLP.pre=readMat(here("Slepian_ARP","Slepian_spatial_181.mat"))$Slepian.spatial
Basis.SLP=matrix(0,nrow(Dat.loc),length(Eig.arp))  # Reorder the Slepian bases 
for(i in 1:length(Eig.arp)){
  id=which(Rerank.id==i)
  Basis.SLP[,i]=Basis.SLP.pre[,id]
}
Basis.SLP.pre=0
# Plot Figure 2(a)
dataF=data.frame(lon=rep(Dat.loc[,1],times=9),lat=rep(Dat.loc[,2],times=9),
                 Degree=c(Basis.SLP[,1],Basis.SLP[,10],Basis.SLP[,20],
                          Basis.SLP[,52],Basis.SLP[,122],Basis.SLP[,192],
                          Basis.SLP[,235],Basis.SLP[,300],Basis.SLP[,435]),
                 group=as.factor(rep(c("\u03BB\u0020\u0020\u2081 = 1.0000000","\u03BB\u0020\u2081\u2080 = 1.0000000","\u03BB\u0020\u2082\u2080 = 1.0000000",
                                       "\u03BB\u0020\u2085\u2082 = 1.0000000","\u03BB\u2081\u2082\u2082 = 0.9991339","\u03BB\u2081\u2089\u2082 = 0.8132422",
                                       "\u03BB\u2082\u2083\u2085 = 0.2873075","\u03BB\u2083\u2080\u2080 = 0.0102389","\u03BB\u2084\u2083\u2085 = 0.0000000"),each=2035)))
PT.1=ggplot() + facet_wrap(~ group,ncol = 3, dir="h")+
  geom_raster(mapping=aes(lon, lat, fill = Degree),data=dataF)+
  geom_path(mapping=aes(x = lon, y = lat), data=dataARPb,size=0.6,linetype=2,color="darkgray")+
  scale_fill_gradient2(low = "#3288BD",mid="white",high = "#E41A1C",midpoint=0,limits=range(dataF$Degree))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=9),
                   axis.title = element_text(size=9),
                   legend.position ="right",
                   #legend.justification = c(0,0),
                   legend.title = element_blank(),
                   legend.key.width=unit(0.5,"line"),
                   legend.key.height=unit(2,"line"))+
  ylab("Latitude")+xlab("Longitude")
print(PT.1)                         # Save to 6.00*4.64 Slepian_Bases_A300.pdf    
# t2=proc.time()[[3]]
# t2-t1=1.859 



###### Part 2. Reproduce Figure 2(b)
# t1=proc.time()[[3]]
# Load eigenvalues for Slepian bases with Q=96(A=100)
Eig.arp.96=Re(readMat(here("Slepian_ARP","Basis_reg_eig_value_96_real.mat"))$Basis.reg.eig.value)
Rerank.id.96=rank(-Eig.arp.96,ties.method = "first")
Eig.arp.rerank.96=rep(0,length(Eig.arp.96))
for(i in 1:length(Eig.arp.96)){
  id=which(Rerank.id.96==i)
  Eig.arp.rerank.96[i]=Eig.arp.96[id]
}
Q.sl.96=length(which(Re(Eig.arp.96)>=0.01)) 
# Load eigenvalues for Slepian bases with Q=144(A=200)
Eig.arp.144=Re(readMat(here("Slepian_ARP","Basis_reg_eig_value_144_real.mat"))$Basis.reg.eig.value)
Rerank.id.144=rank(-Eig.arp.144,ties.method = "first")
Eig.arp.rerank.144=rep(0,length(Eig.arp.144))
for(i in 1:length(Eig.arp.144)){
  id=which(Rerank.id.144==i)
  Eig.arp.rerank.144[i]=Eig.arp.144[id]
}
Q.sl.144=length(which(Re(Eig.arp.144)>=0.01))-1
# Load eigenvalues for Slepian bases with Q=163(A=250)
Eig.arp.163=Re(readMat(here("Slepian_ARP","Basis_reg_eig_value_163_real.mat"))$Basis.reg.eig.value)
Rerank.id.163=rank(-Eig.arp.163,ties.method = "first")
Eig.arp.rerank.163=rep(0,length(Eig.arp.163))
for(i in 1:length(Eig.arp.163)){
  id=which(Rerank.id.163==i)
  Eig.arp.rerank.163[i]=Eig.arp.163[id]
}
Q.sl.163=length(which(Re(Eig.arp.163)>=0.01))
# Load eigenvalues for Slepian bases with Q=181(A=300)
Eig.arp.181=Re(readMat(here("Slepian_ARP","Basis_reg_eig_value_181_real.mat"))$Basis.reg.eig.value)
Rerank.id.181=rank(-Eig.arp.181,ties.method = "first")
Eig.arp.rerank.181=rep(0,length(Eig.arp.181))
for(i in 1:length(Eig.arp.181)){
  id=which(Rerank.id.181==i)
  Eig.arp.rerank.181[i]=Eig.arp.181[id]
}
Q.sl.181=length(which(Re(Eig.arp.181)>=0.01))
# Load eigenvalues for Slepian bases with Q=213(A=400)
Eig.arp.213=Re(readMat(here("Slepian_ARP","Basis_reg_eig_value_213_real.mat"))$Basis.reg.eig.value)
Rerank.id.213=rank(-Eig.arp.213,ties.method = "first")
Eig.arp.rerank.213=rep(0,length(Eig.arp.213))
for(i in 1:length(Eig.arp.213)){
  id=which(Rerank.id.213==i)
  Eig.arp.rerank.213[i]=Eig.arp.213[id]
}
Q.sl.213=length(which(Re(Eig.arp.213)>=0.01))-1
# Plot Figure 2(b)
dataF=data.frame(aA=c(1:length(Eig.arp.rerank.96),1:length(Eig.arp.rerank.144),rep(1:500,times=3)),
                 Eigvalue=c(Eig.arp.rerank.96,Eig.arp.rerank.144,Eig.arp.rerank.163[1:500],
                            Eig.arp.rerank.181[1:500],Eig.arp.rerank.213[1:500]),
                 group=as.factor(c(rep("Q= 96, A=100",each=length(Eig.arp.rerank.96)),
                                   rep("Q=144, A=200",each=length(Eig.arp.rerank.144)),
                                   rep("Q=163, A=250",each=500),rep("Q=181, A=300",each=500),
                                   rep("Q=213, A=400",each=500))))
dataP=data.frame(aA=c(100,200,250,300,400),Eigvalue=rep(0.01,5))
PT.2=ggplot() +
  geom_point(mapping=aes(aA, Eigvalue, shape=group, color=group),size=0.4,data=dataF)+
  geom_point(mapping=aes(aA, Eigvalue),color="#FCBBA1",shape=8,data=dataP[1,])+
  geom_point(mapping=aes(aA, Eigvalue),color="#FC9272",shape=8,data=dataP[2,])+
  geom_point(mapping=aes(aA, Eigvalue),color="#FB6A4A",shape=8,data=dataP[3,])+
  geom_point(mapping=aes(aA, Eigvalue),color="#EF3B2C",shape=8,data=dataP[4,])+
  geom_point(mapping=aes(aA, Eigvalue),color="#CB181D",shape=8,data=dataP[5,])+
  geom_point(mapping=aes(aA, Eigvalue),color="#99000D",shape=8,data=dataP[6,])+
  scale_color_manual(values=c("gray70","gray60","gray50","gray40","gray30","gray20"))+
  scale_shape_manual(values=c(1:6))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.position =c(0.99,0.99),
                   legend.justification = c(1,1),
                   legend.title = element_blank(),
                   legend.key.width=unit(2,"line"),
                   legend.key.height=unit(1,"line"))+
  geom_hline(yintercept = 0.01, size=0.5, linetype=2, color="#E41A1C")+
  xlab(expression(alpha))+ylab(expression(lambda[alpha]))
print(PT.2)       # Size 6.70*2.70
# t2=proc.time()[[3]]
# t2-t1=0.917 



