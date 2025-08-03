#######################################################################################
# This file reproduces Figures 7-8 and S8-9                                           #
#######################################################################################
# Please ensure that you have run the Rscripts "FSG.R", "Scenario1_OSG_Long.R",  
# and "Scenario2_OSG_Short.R", and saved all outputs.
Col=c("#3288BD","#66C2A5","#ABDDA4","#E6F598","#FEE08B","#FDAE61","#F46D43","#D53E4F")

# R packages
# library(R.matlab)
# library(ggplot2)
# library(patchwork)
# library(scales)
# library(dplyr)

# OSG-Short information
R=10
B=517
T0=31*8       
Tt=7*8 
P=2
Q.sl=300



###### Part 1. Reproduce Figures 7 and S8
# Plot Figures 7(a) and S8(a)
TGHpara.u.online=readMat(here("Case_Studies/Scenario2_OSG_Short","TGHpara_u_all_2.mat"))$TGHparauall
TGHpara.v.online=readMat(here("Case_Studies/Scenario2_OSG_Short","TGHpara_v_all_2.mat"))$TGHparavall
dataF=data.frame(aseq=rep(1:Q.sl,times=4),
                 res=c(TGHpara.u.online[1,,1],TGHpara.u.online[1,,2],TGHpara.u.online[B+1,,1],TGHpara.u.online[B+1,,2]),
                 Type=as.factor(rep(rep(c(" \u03C9","h"),each=Q.sl),times=2)),
                 b=as.factor(rep(c("b=0","b=B"),each=2*Q.sl)))
dataF=dataF %>% mutate(ymin = case_when(Type == " \u03C9" ~ 0,
                                        Type == "h" ~ 0),
                       ymax = case_when(Type == " \u03C9" ~ 0.02,
                                        Type == "h" ~ 0.2))
PT=ggplot(data=dataF,aes(x=aseq,y=res,color=b))+
  geom_line()+
  facet_wrap(~ Type, nrow = 2,scales="free")+
  geom_blank(aes(y = ymin)) +  
  geom_blank(aes(y = ymax)) +
  scale_color_manual(values = Col[c(1,8)])+
  xlab(expression(alpha))+ylab(" ")+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(1,1),
                   legend.position =c(0.78,0.99),
                   legend.title = element_blank(),
                   legend.key.width=unit(1,"line"),
                   legend.key.height=unit(0.8,"line"))
print(PT)   # 4.15*3.10

dataF=data.frame(aseq=rep(1:Q.sl,times=4),
                 res=c(TGHpara.v.online[1,,1],TGHpara.v.online[1,,2],TGHpara.v.online[B+1,,1],TGHpara.v.online[B+1,,2]),
                 Type=as.factor(rep(rep(c(" \u03C9","h"),each=Q.sl),times=2)),
                 b=as.factor(rep(c("b=0","b=B"),each=2*Q.sl)))
dataF=dataF %>% mutate(ymin = case_when(Type == " \u03C9" ~ 0,
                                        Type == "h" ~ 0),
                       ymax = case_when(Type == " \u03C9" ~ 0.02,
                                        Type == "h" ~ 0.2))
PT=ggplot(data=dataF,aes(x=aseq,y=res,color=b))+
  geom_line()+
  facet_wrap(~ Type, nrow = 2,scales="free")+
  geom_blank(aes(y = ymin)) +  
  geom_blank(aes(y = ymax)) +
  scale_color_manual(values = Col[c(1,8)])+
  xlab(expression(alpha))+ylab(" ")+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(1,1),
                   legend.position =c(0.78,0.99),
                   legend.title = element_blank(),
                   legend.key.width=unit(1,"line"),
                   legend.key.height=unit(0.8,"line"))
print(PT)          # 4.15*3.10


# Plot Figures 7(b) and S8(b)
TGHpara.u.full=read.csv(here("Case_Studies/FSG","TGHparaufull.csv"))[,-1]
TGHpara.v.full=read.csv(here("Case_Studies/FSG","TGHparavfull.csv"))[,-1]
dataF=data.frame(aseq=rep(0:B,times=4),
                 res=c(TGHpara.u.online[,1,1],TGHpara.v.online[,1,1],TGHpara.u.online[,1,2],TGHpara.v.online[,1,2]),
                 Type=as.factor(rep(c("U","V","U","V"),each=B+1)),
                 group=as.factor(rep(c(" \u03C9","h"),each=2*(B+1))))
data.full=data.frame(group=as.factor(rep(c(" \u03C9","h"),each=2)),
                     res=c(TGHpara.u.full[1,1],TGHpara.v.full[1,1],TGHpara.u.full[1,2],TGHpara.v.full[1,2]),
                     Type=as.factor(c("U","V","U","V")))
PT=ggplot()+facet_wrap(~ group, nrow = 2,scales="free")+
  geom_line(aes(x=aseq,y=res,color=Type),data=dataF)+
  scale_color_manual(values = Col[c(1,8)])+
  #scale_x_continuous(breaks = c(0,3,6,9))+
  xlab(expression(b))+ylab(" ")+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(1,1),
                   legend.position =c(0.99,0.3),
                   legend.title = element_blank(),
                   legend.key.width=unit(1,"line"),
                   legend.key.height=unit(0.8,"line"))+
  geom_hline(data=data.full,aes(yintercept = res,color=Type),linetype=2)
print(PT)

dataF=data.frame(aseq=rep(0:B,times=4),
                 res=c(TGHpara.u.online[,151,1],TGHpara.v.online[,151,1],TGHpara.u.online[,151,2],TGHpara.v.online[,151,2]),
                 Type=as.factor(rep(c("U","V","U","V"),each=B+1)),
                 group=as.factor(rep(c(" \u03C9","h"),each=2*(B+1))))
data.full=data.frame(group=as.factor(rep(c(" \u03C9","h"),each=2)),
                     res=c(TGHpara.u.full[151,1],TGHpara.v.full[151,1],TGHpara.u.full[151,2],TGHpara.v.full[151,2]),
                     Type=as.factor(c("U","V","U","V")))
PT=ggplot()+facet_wrap(~ group, nrow = 2,scales="free")+
  geom_line(aes(x=aseq,y=res,color=Type),data=dataF)+
  scale_color_manual(values = Col[c(1,8)])+
  #scale_x_continuous(breaks = c(0,3,6,9))+
  xlab(expression(b))+ylab(" ")+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(1,1),
                   legend.position =c(0.99,0.215),
                   legend.title = element_blank(),
                   legend.key.width=unit(1,"line"),
                   legend.key.height=unit(0.4,"line"))+
  geom_hline(data=data.full,aes(yintercept = res,color=Type),linetype=2)
PT


# Plot Figure 7(c) and (d)
TGHpara.u.online.1=readMat(here("Case_Studies/Scenario1_OSG_Long","TGHpara_u_all_1.mat"))$TGHparauall
TGHpara.v.online.1=readMat(here("Case_Studies/Scenario1_OSG_Long","TGHpara_v_all_1.mat"))$TGHparavall
RFD.omega.u.1=RFD.h.u.1=RFD.omega.v.1=RFD.h.v.1=rep(0,10)   # RFDs for Tukey h parameters in OSG-Long
for(b in 1:10){
  RFD.omega.u.1[b]=sqrt(sum((TGHpara.u.online.1[b,,1]-TGHpara.u.full[,1])^2))/sqrt(sum(TGHpara.u.full[,1]^2))
  RFD.h.u.1[b]=sqrt(sum((TGHpara.u.online.1[b,,2]-TGHpara.u.full[,2])^2))/sqrt(sum(TGHpara.u.full[,2]^2))
  RFD.omega.v.1[b]=sqrt(sum((TGHpara.v.online.1[b,,1]-TGHpara.v.full[,1])^2))/sqrt(sum(TGHpara.v.full[,1]^2))
  RFD.h.v.1[b]=sqrt(sum((TGHpara.v.online.1[b,,2]-TGHpara.v.full[,2])^2))/sqrt(sum(TGHpara.v.full[,2]^2))
}
RFD.omega.u=RFD.h.u=RFD.omega.v=RFD.h.v=rep(0,B+1)          # RFDs for Tukey h parameters in OSG-Short   
for(b in 1:(B+1)){
  RFD.omega.u[b]=sqrt(sum((TGHpara.u.online[b,,1]-TGHpara.u.full[,1])^2))/sqrt(sum(TGHpara.u.full[,1]^2))
  RFD.h.u[b]=sqrt(sum((TGHpara.u.online[b,,2]-TGHpara.u.full[,2])^2))/sqrt(sum(TGHpara.u.full[,2]^2))
  RFD.omega.v[b]=sqrt(sum((TGHpara.v.online[b,,1]-TGHpara.v.full[,1])^2))/sqrt(sum(TGHpara.v.full[,1]^2))
  RFD.h.v[b]=sqrt(sum((TGHpara.v.online[b,,2]-TGHpara.v.full[,2])^2))/sqrt(sum(TGHpara.v.full[,2]^2))
}

dataF=data.frame(bseq=rep(0:B,times=2),omega=c(res.omega.u,res.omega.v),
                 h=c(res.h.u,res.h.v),
                 Type=as.factor(rep(c("U","V"),each=B+1)),
                 group=as.factor(rep("OSG-Short",each=2*(B+1))))
datay=data.frame(bseq=rep((365*(1:10)-31)/(3650-31)*B,times=2),
                 omega=c(res.omega.u.1,res.omega.v.1),
                 h=c(res.h.u.1,res.h.v.1),
                 Type=as.factor(rep(c("U","V"),each=10)),
                 group=as.factor(rep("OSG-Long",each=20)))
dataF=rbind(dataF,datay)
PT=ggplot(dataF,aes(x=bseq,y=omega,color=group))+
  facet_wrap(~ Type, nrow = 2,scales="free")+
  geom_line()+
  geom_point(aes(x=bseq,y=omega),color=Col[8],shape=4,data=dataF[-(1:(2*(B+1))),])+
  scale_color_manual(values = c(Col[1],Col[8]))+
  xlab(expression(b))+ylab(" ")+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=14),
                   legend.justification = c(1,1),
                   legend.position =c(0.99,0.99),
                   legend.title = element_blank(),
                   legend.key.width=unit(1,"line"),
                   legend.key.height=unit(0.8,"line"))
print(PT)

PT=ggplot(dataF,aes(x=bseq,y=h,color=group))+
  facet_wrap(~ Type, nrow = 2,scales="free")+
  geom_line()+
  geom_point(aes(x=bseq,y=h),color=Col[8],shape=4,data=dataF[-(1:(2*(B+1))),])+
  scale_color_manual(values = c(Col[1],Col[8]))+
  xlab(expression(b))+ylab(" ")+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=14),
                   legend.justification = c(1,1),
                   legend.position ="none",
                   legend.title = element_blank(),
                   legend.key.width=unit(1,"line"),
                   legend.key.height=unit(0.8,"line"))
print(PT)



###### Part 2. Reproduce Figures 8 and S9
# Plot Figure 8(a)
Phi.hat.full=readMat(here("Case_Studies/FSG","Phihatfull.mat"))$Phihatfull
Phi.hat.online=readMat(here("Case_Studies/Scenario2_OSG_Short","Phihat_online_2.mat"))$Phihatonline
dataF=data.frame(V1=rep(1:(2*Q.sl),each=2*Q.sl),V2=rep(seq(-1,-2*Q.sl,by=-1),times=2*Q.sl),
                 Vfull1=c(Phi.hat.full[1:(2*Q.sl),]),Vfull2=c(Phi.hat.full[-(1:(2*Q.sl)),]),
                 Vb01=c(Phi.hat.online[1,1:(2*Q.sl),]),Vb02=c(Phi.hat.online[1,-(1:(2*Q.sl)),]),
                 VbB1=c(Phi.hat.online[2,1:(2*Q.sl),]),VbB2=c(Phi.hat.online[2,-(1:(2*Q.sl)),]))
PT.0=ggplot()+xlab(" ")+ylab(" ")+
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
  geom_raster(aes(x=V1,y=V2,fill=sign(VbB1)*(abs(VbB1))^(1/2)),data=dataF)+
  scale_colour_gradient2(low = "#0072B2", mid = "white",high ="#E41A1C",
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
print(PT.0+PT.B+PT.full)      # 8.95*3.00


# Plot Figure S9
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


# Plot Figure 8(b)
K.full=readMat(here("Case_Studies/FSG","Kfull.mat"))$Kfull
K.online=readMat(here("Case_Studies/Scenario2_OSG_Short","K_online_2.mat"))$Konline
dataF=data.frame(V1=rep(1:(2*Q.sl),each=2*Q.sl),V2=rep(seq(-1,-2*Q.sl,by=-1),times=2*Q.sl),
                 Vfull=c(K.full),Vb0=c(K.online[1,,]/R/(T0-P)),VbB=c(K.online[2,,]/R/(T0+B*Tt-(B+1)*P)))
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


# Plot Figure 8(c)
RFD.OSG.Long=read.csv(here("Case_Studies/Scenario1_OSG_Long","RFDs_OSG_Long.csv"))[,-1]
RFD.OSG.Short=read.csv(here("Case_Studies/Scenario2_OSG_Short","RFDs_OSG_Short.csv"))[,-1]
dataF=data.frame(bseq=rep(0:B,times=3),res=c(as.matrix(RFD.OSG.Short)),
                 Type=as.factor(rep(c(" \u03A6\u0031"," \u03A6\u0032","K"),each=B+1)),
                 group=as.factor(rep("OSG-Short",each=3*(B+1))))
datay=data.frame(bseq=rep((365*(1:10)-31)/(3650-31)*B,times=3),res=c(as.matrix(RFD.OSG.Long)),
                 Type=as.factor(rep(c(" \u03A6\u0031"," \u03A6\u0032","K"),each=10)),
                 group=as.factor(rep("OSG-Long",each=30)))
dataF=rbind(dataF,datay)
PT=ggplot(dataF,aes(x=bseq,y=res,color=group))+
  facet_wrap(~ Type, ncol=3,scales="free")+
  geom_line()+
  geom_point(aes(x=bseq,y=res),color=Col[8],shape=4,data=dataF[-(1:(3*(B+1))),])+
  scale_color_manual(values = c(Col[1],Col[8]))+
  xlab(expression(b))+ylab(" ")+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=14),
                   legend.justification = c(1,1),
                   legend.position =c(0.99,0.99),
                   legend.title = element_blank(),
                   legend.key.width=unit(1,"line"),
                   legend.key.height=unit(0.8,"line"))
print(PT)          # 7.92*2.08





