#######################################################################################
# This file reproduces Figures 4-5, S6, and Table S3                                  #
#######################################################################################
# Please ensure that you have run the Rscripts "FSG.R" and "Scenario1_OSG_Long.R" and saved all outputs.
Col=c("#3288BD","#66C2A5","#ABDDA4","#E6F598","#FEE08B","#FDAE61","#F46D43","#D53E4F")

# R packages
# library(R.matlab)
# library(ggplot2)
# library(patchwork)

# OSG-Long information
B=9
R=10
Tt=365*8
P=2



###### Part 1. Reproduce Figure 4
# Plot Figure 4(a) and (b)
TGHpara.u.online.1=readMat(here("Case_Studies/Scenario1_OSG_Long","TGHpara_u_all_1.mat"))$TGHparauall
TGHpara.v.online.1=readMat(here("Case_Studies/Scenario1_OSG_Long","TGHpara_v_all_1.mat"))$TGHparavall
dataF=data.frame(aseq=rep(1:Q.sl,times=4),
                 omega=c(TGHpara.u.online.1[1,,1],TGHpara.v.online.1[1,,1],TGHpara.u.online.1[B+1,,1],TGHpara.v.online.1[B+1,,1]),
                 h=c(TGHpara.u.online.1[1,,2],TGHpara.v.online.1[1,,2],TGHpara.u.online.1[B+1,,2],TGHpara.v.online.1[B+1,,2]),
                 Type=as.factor(rep(c("U","V","U","V"),each=Q.sl)),
                 b=as.factor(rep(c("b=0","b=B"),each=2*Q.sl)))
PT=ggplot()+facet_wrap(~ Type, nrow = 2)+
  geom_line(aes(x=aseq,y=omega,color=b),data=dataF)+
  scale_color_manual(values = Col[c(1,8)])+
  scale_y_continuous(limits = c(0,0.02))+
  xlab(expression(alpha))+ylab(expression(omega))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=14),
                   legend.justification = c(1,1),
                   legend.position =c(0.8,0.99),
                   legend.title = element_blank(),
                   legend.key.width=unit(1,"line"),
                   legend.key.height=unit(0.9,"line"))
print(PT)         # 4.15*3.10

PT=ggplot()+facet_wrap(~ Type, nrow = 2)+
  geom_line(aes(x=aseq,y=h,color=b),data=dataF)+
  scale_color_manual(values = Col[c(1,8)])+
  scale_y_continuous(limits = c(0,0.2))+
  xlab(expression(alpha))+ylab(expression(h))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=14),
                   legend.justification = c(1,1),
                   legend.position ="none",
                   legend.title = element_blank(),
                   legend.key.width=unit(1,"line"),
                   legend.key.height=unit(0.9,"line"))
print(PT)


# Plot Figure 4(c) and (d)
TGHpara.u.full=read.csv(here("Case_Studies/FSG","TGHparaufull.csv"))[,-1]
TGHpara.v.full=read.csv(here("Case_Studies/FSG","TGHparavfull.csv"))[,-1]
dataF=data.frame(aseq=rep(0:B,times=4),
                 omega=c(TGHpara.u.online.1[,1,1],TGHpara.v.online.1[,1,1],TGHpara.u.online.1[,151,1],TGHpara.v.online.1[,151,1]),
                 h=c(TGHpara.u.online.1[,1,2],TGHpara.v.online.1[,1,2],TGHpara.u.online.1[,151,2],TGHpara.v.online.1[,151,2]),
                 Type=as.factor(rep(c("U","V","U","V"),each=B+1)),
                 b=as.factor(rep(c("\U03B1=1","\U03B1=151"),each=2*(B+1))))
data.full=data.frame(b=as.factor(c("\U03B1=1","\U03B1=151","\U03B1=1","\U03B1=151")),
                     omega=c(TGHpara.u.full[1,1],TGHpara.u.full[151,1],TGHpara.v.full[1,1],TGHpara.v.full[151,1]),
                     h=c(TGHpara.u.full[1,2],TGHpara.u.full[151,2],TGHpara.v.full[1,2],TGHpara.v.full[151,2]),
                     Type=as.factor(c("U","U","V","V")))
PT=ggplot()+facet_wrap(~ b, nrow = 2,scales="free")+
  geom_line(aes(x=aseq,y=omega,color=Type),data=dataF)+
  geom_point(aes(x=aseq,y=omega,color=Type),data=dataF)+
  scale_color_manual(values = Col[c(1,8)])+
  scale_x_continuous(breaks = c(0,3,6,9))+
  xlab(expression(b))+ylab(expression(omega))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=14),
                   legend.justification = c(0,1),
                   legend.position ="none",
                   legend.title = element_blank(),
                   legend.key.width=unit(1,"line"),
                   legend.key.height=unit(0.9,"line"))+
  geom_hline(data=data.full,aes(yintercept = omega,color=Type),linetype=2)
print(PT)

PT=ggplot()+facet_wrap(~ b, nrow = 2,scales="free")+
  geom_line(aes(x=aseq,y=h,color=Type),data=dataF)+
  geom_point(aes(x=aseq,y=h,color=Type),data=dataF)+
  scale_color_manual(values = Col[c(1,8)])+
  scale_x_continuous(breaks = c(0,3,6,9))+
  xlab(expression(b))+ylab(expression(h))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=14),
                   legend.justification = c(1,0),
                   legend.position =c(0.99,0.64),
                   legend.title = element_blank(),
                   legend.key.width=unit(1,"line"),
                   legend.key.height=unit(0.9,"line"))+
  geom_hline(data=data.full,aes(yintercept = h,color=Type),linetype=2)
print(PT)



###### Part 2. Reproduce Figures 5 and S6
# Plot Figure 5(a) 
Phi.hat.full=readMat(here("Case_Studies/FSG","Phihatfull.mat"))$Phihatfull
Phi.hat.online.1=readMat(here("Case_Studies/Scenario1_OSG_Long","Phihat_online_1.mat"))$Phihatonline
dataF=data.frame(V1=rep(1:(2*Q.sl),each=2*Q.sl),V2=rep(seq(-1,-2*Q.sl,by=-1),times=2*Q.sl),
                 Vfull1=c(Phi.hat.full[1:(2*Q.sl),]),Vfull2=c(Phi.hat.full[-(1:(2*Q.sl)),]),
                 Vb01=c(Phi.hat.online.1[1,1:(2*Q.sl),]),Vb02=c(Phi.hat.online.1[1,-(1:(2*Q.sl)),]),
                 VbB1=c(Phi.hat.online.1[2,1:(2*Q.sl),]),VbB2=c(Phi.hat.online.1[2,-(1:(2*Q.sl)),]))
PT.0=ggplot()+xlab(" ")+ylab(" ")+
  # geom_point(aes(x=V1,y=V2,colour=sign(Vb01)*(abs(Vb01))^(1/2)),size=0.1,data=dataF,shape=15)+
  geom_raster(aes(x=V1,y=V2,fill=sign(Vb01)*(abs(Vb01))^(1/2)),data=dataF)+
  scale_fill_gradient2(low = "#0072B2", mid = "white",high ="#E41A1C",
                       midpoint = 0,space = "Lab",na.value = "grey50",
                       guide = "colourbar",aesthetics = "fill",limits=c(-0.61,1))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_blank(),
                   axis.title = element_blank(),
                   legend.title = element_blank(),
                   legend.position = "none",
                   legend.key.width=unit(1,"line"),
                   legend.key.height=unit(1,"line"),
                   plot.title = element_text(hjust=0.5,size=16))+
  ggtitle("b=0")
PT.B=ggplot()+xlab(" ")+ylab(" ")+
  # geom_point(aes(x=V1,y=V2,colour=sign(VbB1)*(abs(VbB1))^(1/2)),size=0.1,data=dataF,shape=15)+
  geom_raster(aes(x=V1,y=V2,fill=sign(VbB1)*(abs(VbB1))^(1/2)),data=dataF)+
  scale_fill_gradient2(low = "#0072B2", mid = "white",high ="#E41A1C",
                       midpoint = 0,space = "Lab",na.value = "grey50",
                       guide = "colourbar",aesthetics = "fill",limits=c(-0.61,1))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_blank(),
                   axis.title = element_blank(),
                   legend.title = element_blank(),
                   legend.position = "none",
                   legend.key.width=unit(1,"line"),
                   legend.key.height=unit(1,"line"),
                   plot.title = element_text(hjust=0.5,size=16))+
  ggtitle("b=B")
PT.full=ggplot()+xlab(" ")+ylab(" ")+
  # geom_point(aes(x=V1,y=V2,colour=sign(Vfull1)*(abs(Vfull1))^(1/2)),size=0.1,data=dataF,shape=15)+
  geom_raster(aes(x=V1,y=V2,fill=sign(Vfull1)*(abs(Vfull1))^(1/2)),data=dataF)+
  scale_fill_gradient2(low = "#0072B2", mid = "white",high ="#E41A1C",
                       midpoint = 0,space = "Lab",na.value = "grey50",
                       guide = "colourbar",aesthetics = "fill",limits=c(-0.61,1))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_blank(),
                   axis.title = element_blank(),
                   legend.title = element_blank(),
                   legend.position = "right",
                   legend.key.width=unit(0.5,"line"),
                   legend.key.height=unit(1.5,"line"),
                   plot.title = element_text(hjust=0.5,size=16))+
  ggtitle("Full")
print(PT.0+PT.B+PT.full)  # 6.46*2.33 


# Plot Figure S6
PT.0=ggplot()+xlab(" ")+ylab(" ")+
  geom_raster(aes(x=V1,y=V2,fill=sign(Vb02)*(abs(Vb02))^(1/2)),data=dataF)+
  scale_fill_gradient2(low = "#0072B2", mid = "white",high ="#E41A1C",
                       midpoint = 0,space = "Lab",na.value = "grey50",
                       guide = "colourbar",aesthetics = "fill",limits=c(-0.62,0.6))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_blank(),
                   axis.title = element_blank(),
                   legend.title = element_blank(),
                   legend.position = "none",
                   legend.key.width=unit(1,"line"),
                   legend.key.height=unit(1,"line"),
                   plot.title = element_text(hjust=0.5,size=16))+
  ggtitle("b=0")
PT.B=ggplot()+xlab(" ")+ylab(" ")+
  geom_raster(aes(x=V1,y=V2,fill=sign(VbB2)*(abs(VbB2))^(1/2)),data=dataF)+
  scale_fill_gradient2(low = "#0072B2", mid = "white",high ="#E41A1C",
                       midpoint = 0,space = "Lab",na.value = "grey50",
                       guide = "colourbar",aesthetics = "fill",limits=c(-0.62,0.6))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_blank(),
                   axis.title = element_blank(),
                   legend.title = element_blank(),
                   legend.position = "none",
                   legend.key.width=unit(1,"line"),
                   legend.key.height=unit(1,"line"),
                   plot.title = element_text(hjust=0.5,size=16))+
  ggtitle("b=B")
PT.full=ggplot()+xlab(" ")+ylab(" ")+
  geom_raster(aes(x=V1,y=V2,fill=sign(Vfull2)*(abs(Vfull2))^(1/2)),data=dataF)+
  scale_fill_gradient2(low = "#0072B2", mid = "white",high ="#E41A1C",
                       midpoint = 0,space = "Lab",na.value = "grey50",
                       guide = "colourbar",aesthetics = "fill",limits=c(-0.62,0.6))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_blank(),
                   axis.title = element_blank(),
                   legend.title = element_blank(),
                   legend.position = "right",
                   legend.key.width=unit(0.5,"line"),
                   legend.key.height=unit(1.5,"line"),
                   plot.title = element_text(hjust=0.5,size=16))+
  ggtitle("Full")
print(PT.0+PT.B+PT.full)


# Plot Figure 5(b) 
K.full=readMat(here("Case_Studies/FSG","Kfull.mat"))$Kfull
K.online.1=readMat(here("Case_Studies/Scenario1_OSG_Long","K_online_1.mat"))$Konline
dataF=data.frame(V1=rep(1:(2*Q.sl),each=2*Q.sl),V2=rep(seq(-1,-2*Q.sl,by=-1),times=2*Q.sl),
                 Vfull=c(K.full),Vb0=c(K.online.1[1,,]/R/(Tt-P)),VbB=c(K.online.1[2,,]/R/(B+1)/(Tt-P)))
PT.0=ggplot()+xlab(" ")+ylab(" ")+
  geom_raster(aes(x=V1,y=V2,fill=sign(Vb0)*(abs(Vb0)^(1/2))),data=dataF)+
  scale_fill_gradient2(low = "#0072B2", mid = "white",high ="#E41A1C",
                       midpoint = 0,space = "Lab",na.value = "grey50",
                       guide = "colourbar",aesthetics = "fill",limits=c(-0.56,1))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_blank(),
                   axis.title = element_blank(),
                   legend.title = element_blank(),
                   legend.position = "none",
                   legend.key.width=unit(1,"line"),
                   legend.key.height=unit(1,"line"),
                   plot.title = element_text(hjust=0.5,size=16))+
  ggtitle("b=0")
PT.B=ggplot()+xlab(" ")+ylab(" ")+
  geom_raster(aes(x=V1,y=V2,fill=sign(VbB)*(abs(VbB)^(1/2))),data=dataF)+
  scale_fill_gradient2(low = "#0072B2", mid = "white",high ="#E41A1C",
                       midpoint = 0,space = "Lab",na.value = "grey50",
                       guide = "colourbar",aesthetics = "fill",limits=c(-0.56,1))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_blank(),
                   axis.title = element_blank(),
                   legend.title = element_blank(),
                   legend.position = "none",
                   legend.key.width=unit(1,"line"),
                   legend.key.height=unit(1,"line"),
                   plot.title = element_text(hjust=0.5,size=16))+
  ggtitle("b=B")
PT.full=ggplot()+xlab(" ")+ylab(" ")+
  geom_raster(aes(x=V1,y=V2,fill=sign(Vfull)*(abs(Vfull)^(1/2))),data=dataF)+
  scale_fill_gradient2(low = "#0072B2", mid = "white",high ="#E41A1C",
                       midpoint = 0,space = "Lab",na.value = "grey50",
                       guide = "colourbar",aesthetics = "fill",limits=c(-0.56,1))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_blank(),
                   axis.title = element_blank(),
                   legend.title = element_blank(),
                   legend.position = "right",
                   legend.key.width=unit(0.5,"line"),
                   legend.key.height=unit(1.5,"line"),
                   plot.title = element_text(hjust=0.5,size=16))+
  ggtitle("Full")
print(PT.0+PT.B+PT.full)



###### Part 3. Reproduce Table S3
RFD.OSG.Long=read.csv(here("Case_Studies/Scenario1_OSG_Long","RFDs_OSG_Long.csv"))[,-1]
print(t(RFD.OSG.Long))

