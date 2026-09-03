#######################################################################################
# This file reproduces Figures 6 and 9, S9, S12, and S13                              #
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

# ARP information for ploting figures
world_map=map_data("world")                                                   # Extract the map data for the region
arabian_peninsula_countries=c("Saudi Arabia", "Yemen", 
                              "Oman", "United Arab Emirates", 
                              "Qatar", "Bahrain", "Kuwait")                   # Define the countries of the Arabian Peninsula
arabian_peninsula=subset(world_map, region %in% arabian_peninsula_countries)  # Filter the map data for the Arabian Peninsula countries
dataARP=data.frame(lon=rep(arabian_peninsula$long,times=2),lat=rep(arabian_peninsula$lat,times=2),
                   type=rep(arabian_peninsula$group,times=2),
                   group=as.factor(rep(c("U","V"),each=length(arabian_peninsula$long))))



###### Part 1. Read assessment indices for FSG, OSG-Long and OSG-Short
# FSG
Iuq.u.full=read.csv(here("Case_Studies/FSG","Iuq_u_full.csv"))$x
Iuq.v.full=read.csv(here("Case_Studies/FSG","Iuq_v_full.csv"))$x
Ibc.full=read.csv(here("Case_Studies/FSG","Ibc_full.csv"))$x
Ibc.full=Ibc.full[1:length(id.ARP)]-Ibc.full[-(1:length(id.ARP))]
Itc1.u.full=read.csv(here("Case_Studies/FSG","Itc1_u_full.csv"))$x
Itc1.u.full=Itc1.u.full[1:length(id.ARP)]-Itc1.u.full[-(1:length(id.ARP))]
Itc1.v.full=read.csv(here("Case_Studies/FSG","Itc1_v_full.csv"))$x
Itc1.v.full=Itc1.v.full[1:length(id.ARP)]-Itc1.v.full[-(1:length(id.ARP))]
Itc2.u.full=read.csv(here("Case_Studies/FSG","Itc2_u_full.csv"))$x
Itc2.u.full=Itc2.u.full[1:length(id.ARP)]-Itc2.u.full[-(1:length(id.ARP))]
Itc2.v.full=read.csv(here("Case_Studies/FSG","Itc2_v_full.csv"))$x
Itc2.v.full=Itc2.v.full[1:length(id.ARP)]-Itc2.v.full[-(1:length(id.ARP))]
Itp.full=as.matrix(read.csv(here("Case_Studies/FSG","Itp_full.csv"))[,-1])
Itp.full=Itp.full[,1:length(id.ARP)]/Itp.full[,-(1:length(id.ARP))]
Iwdt.u.full=read.csv(here("Case_Studies/FSG","Iwdt_u_full.csv"))$x
Iwdt.v.full=read.csv(here("Case_Studies/FSG","Iwdt_v_full.csv"))$x
Iwds.u.full=read.csv(here("Case_Studies/FSG","Iwds_u_full.csv"))$x
Iwds.v.full=read.csv(here("Case_Studies/FSG","Iwds_v_full.csv"))$x
Imd.full=read.csv(here("Case_Studies/FSG","Imd_full.csv"))$x
Isdd.full=read.csv(here("Case_Studies/FSG","Isdd_full.csv"))$x
I75qd.full=read.csv(here("Case_Studies/FSG","I75qd_full.csv"))$x
I25qd.full=read.csv(here("Case_Studies/FSG","I25qd_full.csv"))$x

# OSG-Long
Iuq.u.online.1=read.csv(here("Case_Studies/Scenario1_OSG_Long","Iuq_u_online_1.csv"))$x
Iuq.v.online.1=read.csv(here("Case_Studies/Scenario1_OSG_Long","Iuq_v_online_1.csv"))$x
Ibc.online.1=read.csv(here("Case_Studies/Scenario1_OSG_Long","Ibc_online_1.csv"))$x
Ibc.online.1=Ibc.online.1[1:length(id.ARP)]-Ibc.online.1[-(1:length(id.ARP))]
Itc1.u.online.1=read.csv(here("Case_Studies/Scenario1_OSG_Long","Itc1_u_online_1.csv"))$x
Itc1.u.online.1=Itc1.u.online.1[1:length(id.ARP)]-Itc1.u.online.1[-(1:length(id.ARP))]
Itc1.v.online.1=read.csv(here("Case_Studies/Scenario1_OSG_Long","Itc1_v_online_1.csv"))$x
Itc1.v.online.1=Itc1.v.online.1[1:length(id.ARP)]-Itc1.v.online.1[-(1:length(id.ARP))]
Itc2.u.online.1=read.csv(here("Case_Studies/Scenario1_OSG_Long","Itc2_u_online_1.csv"))$x
Itc2.u.online.1=Itc2.u.online.1[1:length(id.ARP)]-Itc2.u.online.1[-(1:length(id.ARP))]
Itc2.v.online.1=read.csv(here("Case_Studies/Scenario1_OSG_Long","Itc2_v_online_1.csv"))$x
Itc2.v.online.1=Itc2.v.online.1[1:length(id.ARP)]-Itc2.v.online.1[-(1:length(id.ARP))]
Itp.online.1=as.matrix(read.csv(here("Case_Studies/Scenario1_OSG_Long","Itp_online_1.csv"))[,-1])
Itp.online.1=Itp.online.1[,1:length(id.ARP)]/Itp.online.1[,-(1:length(id.ARP))]
Iwdt.u.online.1=read.csv(here("Case_Studies/Scenario1_OSG_Long","Iwdt_u_online_1.csv"))$x
Iwdt.v.online.1=read.csv(here("Case_Studies/Scenario1_OSG_Long","Iwdt_v_online_1.csv"))$x
Iwds.u.online.1=read.csv(here("Case_Studies/Scenario1_OSG_Long","Iwds_u_online_1.csv"))$x
Iwds.v.online.1=read.csv(here("Case_Studies/Scenario1_OSG_Long","Iwds_v_online_1.csv"))$x
Imd.online.1=read.csv(here("Case_Studies/Scenario1_OSG_Long","Imd_online_1.csv"))$x
Isdd.online.1=read.csv(here("Case_Studies/Scenario1_OSG_Long","Isdd_online_1.csv"))$x
I75qd.online.1=read.csv(here("Case_Studies/Scenario1_OSG_Long","I75qd_online_1.csv"))$x
I25qd.online.1=read.csv(here("Case_Studies/Scenario1_OSG_Long","I25qd_online_1.csv"))$x

# OSG-Short
Iuq.u.online=read.csv(here("Case_Studies/Scenario2_OSG_Short","Iuq_u_online_2.csv"))$x
Iuq.v.online=read.csv(here("Case_Studies/Scenario2_OSG_Short","Iuq_v_online_2.csv"))$x
Ibc.online=read.csv(here("Case_Studies/Scenario2_OSG_Short","Ibc_online_2.csv"))$x
Ibc.online=Ibc.online[1:length(id.ARP)]-Ibc.online[-(1:length(id.ARP))]
Itc1.u.online=read.csv(here("Case_Studies/Scenario2_OSG_Short","Itc1_u_online_2.csv"))$x
Itc1.u.online=Itc1.u.online[1:length(id.ARP)]-Itc1.u.online[-(1:length(id.ARP))]
Itc1.v.online=read.csv(here("Case_Studies/Scenario2_OSG_Short","Itc1_v_online_2.csv"))$x
Itc1.v.online=Itc1.v.online[1:length(id.ARP)]-Itc1.v.online[-(1:length(id.ARP))]
Itc2.u.online=read.csv(here("Case_Studies/Scenario2_OSG_Short","Itc2_u_online_2.csv"))$x
Itc2.u.online=Itc2.u.online[1:length(id.ARP)]-Itc2.u.online[-(1:length(id.ARP))]
Itc2.v.online=read.csv(here("Case_Studies/Scenario2_OSG_Short","Itc2_v_online_2.csv"))$x
Itc2.v.online=Itc2.v.online[1:length(id.ARP)]-Itc2.v.online[-(1:length(id.ARP))]
Itp.online=as.matrix(read.csv(here("Case_Studies/Scenario2_OSG_Short","Itp_online_2.csv"))[,-1])
Itp.online=Itp.online[,1:length(id.ARP)]/Itp.online[,-(1:length(id.ARP))]
Iwdt.u.online=read.csv(here("Case_Studies/Scenario2_OSG_Short","Iwdt_u_online_2.csv"))$x
Iwdt.v.online=read.csv(here("Case_Studies/Scenario2_OSG_Short","Iwdt_v_online_2.csv"))$x
Iwds.u.online=read.csv(here("Case_Studies/Scenario2_OSG_Short","Iwds_u_online_2.csv"))$x
Iwds.v.online=read.csv(here("Case_Studies/Scenario2_OSG_Short","Iwds_v_online_2.csv"))$x
Imd.online=read.csv(here("Case_Studies/Scenario2_OSG_Short","Imd_online_2.csv"))$x
Isdd.online=read.csv(here("Case_Studies/Scenario2_OSG_Short","Isdd_online_2.csv"))$x
I75qd.online=read.csv(here("Case_Studies/Scenario2_OSG_Short","I75qd_online_2.csv"))$x
I25qd.online=read.csv(here("Case_Studies/Scenario2_OSG_Short","I25qd_online_2.csv"))$x




###### Part 2. Reproduce Figures 6, 9, S9, S12, and S13
### Plot Figures 6(a), 9(a), S9(a), S12(a), and S13(a)
dataFu=data.frame(lon=Dat.loc[id.ARP,1],lat=Dat.loc[id.ARP,2],
                  Iuq.online=c(Iuq.u.online),Iuq.online.1=c(Iuq.u.online.1),Iuq.full=c(Iuq.u.full))
dataFv=data.frame(lon=Dat.loc[id.ARP,1],lat=Dat.loc[id.ARP,2],
                  Iuq.online=c(Iuq.v.online),Iuq.online.1=c(Iuq.v.online.1),Iuq.full=c(Iuq.v.full))
# Panel 6(a)
PT.online=ggplot() +
  geom_raster(mapping=aes(lon, lat, fill = Iuq.online.1),data=dataFu)+
  scale_fill_gradient(low = "white",high ="#E41A1C",limits=c(1,1.43))+
  geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_y_continuous(limits=c(12,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position =c(0.01,0.01),
                   legend.title = element_text(),
                   legend.key.width=unit(0.8,"line"),
                   legend.key.height=unit(0.8,"line"))+
  labs(fill=expression(I[uq]))+
  ylab("Latitude")+xlab("Longitude")
print(PT.online)  # 3.40*3.00

# Panel 9(a)
PT.online=ggplot() +
  geom_raster(mapping=aes(lon, lat, fill = Iuq.online),data=dataFu)+
  scale_fill_gradient(low = "white",high ="#E41A1C",limits=c(1,1.43))+
  geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_y_continuous(limits=c(12,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position =c(0.01,0.01),
                   legend.title = element_text(),
                   legend.key.width=unit(0.8,"line"),
                   legend.key.height=unit(0.8,"line"))+
  labs(fill=expression(I[uq]))+
  ylab("Latitude")+xlab("Longitude")
print(PT.online)

# Panel S9(a)
PT.online=ggplot() +
  geom_raster(mapping=aes(lon, lat, fill = Iuq.online.1),data=dataFv)+
  scale_fill_gradient(low = "white",high ="#E41A1C",limits=c(0.9,1.44))+
  geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_y_continuous(limits=c(12,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position =c(0.01,0.01),
                   legend.title = element_text(),
                   legend.key.width=unit(0.8,"line"),
                   legend.key.height=unit(0.8,"line"))+
  labs(fill=expression(I[uq]))+
  ylab("Latitude")+xlab("Longitude")
print(PT.online)  # 3.40*3.00

# Panel S12(a)
PT.online=ggplot() +
  geom_raster(mapping=aes(lon, lat, fill = Iuq.online),data=dataFv)+
  scale_fill_gradient(low = "white",high ="#E41A1C",limits=c(0.9,1.44))+
  geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_y_continuous(limits=c(12,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position =c(0.01,0.01),
                   legend.title = element_text(),
                   legend.key.width=unit(0.8,"line"),
                   legend.key.height=unit(0.8,"line"))+
  labs(fill=expression(I[uq]))+
  ylab("Latitude")+xlab("Longitude")
print(PT.online)

# Panel S13(a)
dataF=data.frame(I.uq=c((Iuq.u.online.1+Iuq.v.online.1)/2,(Iuq.u.online+Iuq.v.online)/2,(Iuq.u.full+Iuq.v.full)/2),
                 Method=as.factor(rep(c(" OSG-Long "," OSG-Short ","FSG"),each=length(id.ARP))))
PT=ggplot(data = dataF,aes(x=Method,y=I.uq))+geom_boxplot()+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.border = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.text.y = element_text(angle=90,size=12),
                   axis.title=element_text(size=12),
                   #legend.justification=c(0,1),
                   legend.position ="right",
                   legend.title = element_blank(),
                   legend.background = element_blank(),
                   legend.key.width=unit(1,"line"),
                   legend.key.height=unit(1,"line"))+
  ylab(expression(I[uq]))+
  geom_hline(yintercept = 1,linetype=2,colour="red")
print(PT)  # 3.46*3.00



### Plot Figures 6(b), 9(b), S9(b)=S12(b), and S13(b)
dataF=data.frame(lon=Dat.loc[id.ARP,1],lat=Dat.loc[id.ARP,2],
                  Ibc.online=c(Ibc.online),Ibc.online.1=c(Ibc.online.1),Ibc.full=c(Ibc.full))
# Panel 6(b)
PT.online=ggplot() +
  geom_raster(mapping=aes(lon, lat, fill = Ibc.online.1),data=dataF)+
  scale_fill_gradient2(low="#3288BD",mid="white",high="#E41A1C",midpoint = 0,limits=c(-0.23,0.23))+
  geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_y_continuous(limits=c(12,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position =c(0.01,0.01),
                   legend.title = element_text(),
                   legend.key.width=unit(0.8,"line"),
                   legend.key.height=unit(0.8,"line"))+
  labs(fill=expression(I[bc]))+
  ylab("Latitude")+xlab("Longitude")
print(PT.online)

# Panel 9(b)
PT.online=ggplot() +
  geom_raster(mapping=aes(lon, lat, fill = Ibc.online),data=dataF)+
  scale_fill_gradient2(low="#3288BD",mid="white",high="#E41A1C",midpoint = 0,limits=c(-0.23,0.23))+
  geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_y_continuous(limits=c(12,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position =c(0.01,0.01),
                   legend.title = element_text(),
                   legend.key.width=unit(0.8,"line"),
                   legend.key.height=unit(0.8,"line"))+
  labs(fill=expression(I[bc]))+
  ylab("Latitude")+xlab("Longitude")
print(PT.online)

# Panel S9(b)=S12(b)
dataF=data.frame(lon=Dat.loc[id.ARP,1],lat=Dat.loc[id.ARP,2],
                 bd.dat=c(read.csv(here("Case_Studies/FSG","Ibc_full.csv"))$x)[-(1:length(id.ARP))])
PT=ggplot() +
  geom_raster(mapping=aes(lon, lat, fill = bd.dat),data=dataF)+
  scale_fill_gradient2(low = "#3288BD",mid="white",high ="#E41A1C",midpoint = 0)+
  geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_y_continuous(limits=c(12,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position =c(0.01,0.01),
                   legend.title = element_text(),
                   legend.key.width=unit(0.8,"line"),
                   legend.key.height=unit(0.8,"line"))+
  labs(fill=" ")+
  ylab("Latitude")+xlab("Longitude")
print(PT)

# Panel S13(b)
dataF=data.frame(I.bc=c(Ibc.online.1,Ibc.online,Ibc.full),
                 Method=as.factor(c(rep(" OSG-Long ",length(id.ARP)),rep(" OSG-Short ",length(id.ARP)),rep("FSG",length(id.ARP)))))
PT=ggplot(data = dataF,aes(x=Method,y=I.bc))+geom_boxplot()+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.border = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.text.y = element_text(angle=90,size=12),
                   axis.title=element_text(size=12),
                   #legend.justification=c(0,1),
                   legend.position ="right",
                   legend.title = element_blank(),
                   legend.background = element_blank(),
                   legend.key.width=unit(1,"line"),
                   legend.key.height=unit(1,"line"))+
  ylab(expression(I[bc]))+
  geom_hline(yintercept =0,color="red",linetype=2)
print(PT)



### Plot Figures 6(c), 9(c), S9(c), S12(c), and S13(c)
dataFu=data.frame(lon=Dat.loc[id.ARP,1],lat=Dat.loc[id.ARP,2],
                  Itc1.online=c(Itc1.u.online),Itc1.online.1=c(Itc1.u.online.1),Itc1.full=c(Itc1.u.full))
dataFv=data.frame(lon=Dat.loc[id.ARP,1],lat=Dat.loc[id.ARP,2],
                  Itc1.online=c(Itc1.v.online),Itc1.online.1=c(Itc1.v.online.1),Itc1.full=c(Itc1.v.full))
# Panel 6(c)
PT.online=ggplot() +
  geom_raster(mapping=aes(lon, lat, fill = Itc1.online.1),data=dataFu)+
  scale_fill_gradient2(low = "#3288BD",mid="white",high ="#E41A1C",midpoint = 0,limits=c(-0.1,0.1))+
  geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_y_continuous(limits=c(12,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position =c(0.01,0.01),
                   legend.title = element_text(),
                   legend.key.width=unit(0.8,"line"),
                   legend.key.height=unit(0.8,"line"))+
  labs(fill=expression(I[tc1]))+
  ylab("Latitude")+xlab("Longitude")
print(PT.online)

# Panel 9(c)
PT.online=ggplot() +
  geom_raster(mapping=aes(lon, lat, fill = Itc1.online),data=dataFu)+
  scale_fill_gradient2(low = "#3288BD",mid="white",high ="#E41A1C",midpoint = 0,limits=c(-0.1,0.1))+
  geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_y_continuous(limits=c(12,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position =c(0.01,0.01),
                   legend.title = element_text(),
                   legend.key.width=unit(0.8,"line"),
                   legend.key.height=unit(0.8,"line"))+
  labs(fill=expression(I[tc1]))+
  ylab("Latitude")+xlab("Longitude")
print(PT.online)

# Panel S9(c)
PT.online=ggplot() +
  geom_raster(mapping=aes(lon, lat, fill = Itc1.online.1),data=dataFv)+
  scale_fill_gradient2(low = "#3288BD",mid="white",high ="#E41A1C",midpoint = 0,limits=c(-0.1,0.1))+
  geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_y_continuous(limits=c(12,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position =c(0.01,0.01),
                   legend.title = element_text(),
                   legend.key.width=unit(0.8,"line"),
                   legend.key.height=unit(0.8,"line"))+
  labs(fill=expression(I[tc1]))+
  ylab("Latitude")+xlab("Longitude")
print(PT.online)

# Panel S12(c)
PT.online=ggplot() +
  geom_raster(mapping=aes(lon, lat, fill = Itc1.online),data=dataFv)+
  scale_fill_gradient2(low = "#3288BD",mid="white",high ="#E41A1C",midpoint = 0,limits=c(-0.1,0.1))+
  geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_y_continuous(limits=c(12,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position =c(0.01,0.01),
                   legend.title = element_text(),
                   legend.key.width=unit(0.8,"line"),
                   legend.key.height=unit(0.8,"line"))+
  labs(fill=expression(I[tc1]))+
  ylab("Latitude")+xlab("Longitude")
print(PT.online)

# Panel S13(c)
dataF=data.frame(I.tc=c(0.5*Itc1.u.online.1+0.5*Itc1.v.online.1,0.5*Itc1.u.online+0.5*Itc1.v.online,0.5*Itc1.u.full+0.5*Itc1.v.full),
                 Method=as.factor(c(rep(" OSG-Long ",length(id.ARP)),rep(" OSG-Short ",length(id.ARP)),rep("FSG",length(id.ARP)))))
PT=ggplot(data = dataF,aes(x=Method,y=I.tc))+geom_boxplot()+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.border = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.text.y = element_text(angle=90,size=12),
                   axis.title=element_text(size=12),
                   #legend.justification=c(0,1),
                   legend.position ="right",
                   legend.title = element_blank(),
                   legend.background = element_blank(),
                   legend.key.width=unit(1,"line"),
                   legend.key.height=unit(1,"line"))+
  ylab(expression(I[tc1]))+
  geom_hline(yintercept =0,color="red",linetype=2)
print(PT)



### Plot Figures 6(d), 9(d), S9(d), S12(d), and S13(d)
dataFu=data.frame(lon=Dat.loc[id.ARP,1],lat=Dat.loc[id.ARP,2],
                  Itc2.online=c(Itc2.u.online),Itc2.online.1=c(Itc2.u.online.1),Itc2.full=c(Itc2.u.full))
dataFv=data.frame(lon=Dat.loc[id.ARP,1],lat=Dat.loc[id.ARP,2],
                  Itc2.online=c(Itc2.v.online),Itc2.online.1=c(Itc2.v.online.1),Itc2.full=c(Itc2.v.full))
# Panel 6(d)
PT.online=ggplot() +
  geom_raster(mapping=aes(lon, lat, fill = Itc2.online.1),data=dataFu)+
  scale_fill_gradient2(low = "#3288BD",mid="white",high ="#E41A1C",midpoint = 0,limits=c(-0.071,0.071))+
  geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_y_continuous(limits=c(12,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position =c(0.01,0.01),
                   legend.title = element_text(),
                   legend.key.width=unit(0.8,"line"),
                   legend.key.height=unit(0.8,"line"))+
  labs(fill=expression(I[tc2]))+
  ylab("Latitude")+xlab("Longitude")
print(PT.online)

# Panel 9(d)
PT.online=ggplot() +
  geom_raster(mapping=aes(lon, lat, fill = Itc2.online),data=dataFu)+
  scale_fill_gradient2(low = "#3288BD",mid="white",high ="#E41A1C",midpoint = 0,limits=c(-0.071,0.071))+
  geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_y_continuous(limits=c(12,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position =c(0.01,0.01),
                   legend.title = element_text(),
                   legend.key.width=unit(0.8,"line"),
                   legend.key.height=unit(0.8,"line"))+
  labs(fill=expression(I[tc2]))+
  ylab("Latitude")+xlab("Longitude")
print(PT.online)

# Panel S9(d)
PT.online=ggplot() +
  geom_raster(mapping=aes(lon, lat, fill = Itc2.online.1),data=dataFv)+
  scale_fill_gradient2(low = "#3288BD",mid="white",high ="#E41A1C",midpoint = 0,limits=c(-0.071,0.071))+
  geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_y_continuous(limits=c(12,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position =c(0.01,0.01),
                   legend.title = element_text(),
                   legend.key.width=unit(0.8,"line"),
                   legend.key.height=unit(0.8,"line"))+
  labs(fill=expression(I[tc2]))+
  ylab("Latitude")+xlab("Longitude")
print(PT.online)

# Panel S12(d)
PT.online=ggplot() +
  geom_raster(mapping=aes(lon, lat, fill = Itc2.online),data=dataFv)+
  scale_fill_gradient2(low = "#3288BD",mid="white",high ="#E41A1C",midpoint = 0,limits=c(-0.071,0.071))+
  geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_y_continuous(limits=c(12,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position =c(0.01,0.01),
                   legend.title = element_text(),
                   legend.key.width=unit(0.8,"line"),
                   legend.key.height=unit(0.8,"line"))+
  labs(fill=expression(I[tc2]))+
  ylab("Latitude")+xlab("Longitude")
print(PT.online)

# Panel S13(d)
dataF=data.frame(I.tc=c(c(0.5*Itc2.u.online.1+0.5*Itc2.v.online.1),c(0.5*Itc2.u.online+0.5*Itc2.v.online),c(0.5*Itc2.u.full+0.5*Itc2.v.full)),
                 Method=as.factor(c(rep(" OSG-Long ",length(id.ARP)),rep(" OSG-Short ",length(id.ARP)),rep("FSG",length(id.ARP)))))
PT=ggplot(data = dataF,aes(x=Method,y=I.tc))+geom_boxplot()+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.border = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.text.y = element_text(angle=90,size=12),
                   axis.title=element_text(size=12),
                   #legend.justification=c(0,1),
                   legend.position ="right",
                   legend.title = element_blank(),
                   legend.background = element_blank(),
                   legend.key.width=unit(1,"line"),
                   legend.key.height=unit(1,"line"))+
  ylab(expression(I[tc2]))+
  geom_hline(yintercept =0,color="red",linetype=2)
print(PT)



### Plot Figures 6(e), 9(e), and S13(e)
dataF=data.frame(lon=Dat.loc[id.ARP,1],lat=Dat.loc[id.ARP,2],
                  Itp1.online=c(Itp.online[1,]),Itp1.online.1=c(Itp.online.1[1,]),Itp1.full=c(Itp.full[1,]))
# Panel 6(e)
PT.online=ggplot() +
  geom_raster(mapping=aes(lon, lat, fill = Itp1.online),data=dataF)+
  scale_fill_gradient2(low = "#3288BD",mid="white",high ="#E41A1C",midpoint = 1,limits=c(0,3))+
  geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_y_continuous(limits=c(12,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position =c(0.01,0.01),
                   legend.title = element_text(),
                   legend.key.width=unit(0.8,"line"),
                   legend.key.height=unit(0.8,"line"))+
  labs(fill=expression(I[tp]^{"[0,5)"}))+
  ylab("Latitude")+xlab("Longitude")
print(PT.online)

# Panel 9(e)
PT.online=ggplot() +
  geom_raster(mapping=aes(lon, lat, fill = Itp1.online),data=dataF)+
  scale_fill_gradient2(low = "#3288BD",mid="white",high ="#E41A1C",midpoint = 1,limits=c(0,3))+
  geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_y_continuous(limits=c(12,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position =c(0.01,0.01),
                   legend.title = element_text(),
                   legend.key.width=unit(0.8,"line"),
                   legend.key.height=unit(0.8,"line"))+
  labs(fill=expression(I[tp]^{"[0,5)"}))+
  ylab("Latitude")+xlab("Longitude")
print(PT.online)

# Panel S13(e)
dataF=data.frame(I.tp=c(Itp.online.1[1,],Itp.online[1,],Itp.full[1,]),
                 Method=as.factor(c(rep(" OSG-Long ",length(id.ARP)),rep(" OSG-Short ",length(id.ARP)),rep("FSG",length(id.ARP)))))
PT=ggplot(data = dataF,aes(x=Method,y=I.tp))+geom_boxplot()+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.border = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.text.y = element_text(angle=90,size=12),
                   axis.title=element_text(size=12),
                   #legend.justification=c(0,1),
                   legend.position ="right",
                   legend.title = element_blank(),
                   legend.background = element_blank(),
                   legend.key.width=unit(1,"line"),
                   legend.key.height=unit(1,"line"))+
  ylab(expression(I[tp]^{"[0,5)"}))+
  geom_hline(yintercept =1,color="red",linetype=2)
PT



### Plot Figures 6(f), 9(f), and S13(f)
dataF=data.frame(lon=Dat.loc[id.ARP,1],lat=Dat.loc[id.ARP,2],
                  Itp2.online=c(Itp.online[2,]),Itp2.online.1=c(Itp.online.1[2,]),Itp2.full=c(Itp.full[2,]))
# Panel 6(f)
PT.online=ggplot() +
  geom_raster(mapping=aes(lon, lat, fill = Itp2.online.1),data=dataF)+
  scale_fill_gradient2(low = "#3288BD",mid="white",high ="#E41A1C",midpoint = 1,limits=c(0,3))+
  geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_y_continuous(limits=c(12,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position =c(0.01,0.01),
                   legend.title = element_text(),
                   legend.key.width=unit(0.8,"line"),
                   legend.key.height=unit(0.8,"line"))+
  labs(fill=expression(I[tp]^{"[5,10)"}))+
  ylab("Latitude")+xlab("Longitude")
print(PT.online)

# Panel 9(f)
PT.online=ggplot() +
  geom_raster(mapping=aes(lon, lat, fill = Itp2.online),data=dataF)+
  scale_fill_gradient2(low = "#3288BD",mid="white",high ="#E41A1C",midpoint = 1,limits=c(0,3))+
  geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_y_continuous(limits=c(12,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position =c(0.01,0.01),
                   legend.title = element_text(),
                   legend.key.width=unit(0.8,"line"),
                   legend.key.height=unit(0.8,"line"))+
  labs(fill=expression(I[tp]^{"[5,10)"}))+
  ylab("Latitude")+xlab("Longitude")
print(PT.online)

# Panel S13(f)
dataF=data.frame(I.tp=c(Itp.online.1[2,],Itp.online[2,],Itp.full[2,]),
                 Method=as.factor(c(rep(" OSG-Long ",length(id.ARP)),rep(" OSG-Short ",length(id.ARP)),rep("FSG",length(id.ARP)))))
PT=ggplot(data = dataF,aes(x=Method,y=I.tp))+geom_boxplot()+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.border = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.text.y = element_text(angle=90,size=12),
                   axis.title=element_text(size=12),
                   #legend.justification=c(0,1),
                   legend.position ="right",
                   legend.title = element_blank(),
                   legend.background = element_blank(),
                   legend.key.width=unit(1,"line"),
                   legend.key.height=unit(1,"line"))+
  ylab(expression(I[tp]^{"[5,10)"}))+
  geom_hline(yintercept =1,color="red",linetype=2)
print(PT)



### Plot Figures 6(g), 9(g), and S13(g)
dataF=data.frame(lon=Dat.loc[id.ARP,1],lat=Dat.loc[id.ARP,2],
                  Itp3.online=c(Itp.online[3,]),Itp3.online.1=c(Itp.online.1[3,]),Itp3.full=c(Itp.full[3,]))
# Panel 6(g)
PT.online=ggplot() +
  geom_raster(mapping=aes(lon, lat, fill = Itp3.online.1),data=dataF)+
  scale_fill_gradient2(low = "#3288BD",mid="white",high ="#E41A1C",midpoint = 1,limits=c(0,3))+
  geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_y_continuous(limits=c(12,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position =c(0.01,0.01),
                   legend.title = element_text(),
                   legend.key.width=unit(0.8,"line"),
                   legend.key.height=unit(0.8,"line"))+
  labs(fill=expression(I[tp]^{"[10,20)"}))+
  ylab("Latitude")+xlab("Longitude")
print(PT.online)

# Panel 9(g)
PT.online=ggplot() +
  geom_raster(mapping=aes(lon, lat, fill = Itp3.online),data=dataF)+
  scale_fill_gradient2(low = "#3288BD",mid="white",high ="#E41A1C",midpoint = 1,limits=c(0,3))+
  geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_y_continuous(limits=c(12,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position =c(0.01,0.01),
                   legend.title = element_text(),
                   legend.key.width=unit(0.8,"line"),
                   legend.key.height=unit(0.8,"line"))+
  labs(fill=expression(I[tp]^{"[10,20)"}))+
  ylab("Latitude")+xlab("Longitude")
print(PT.online)

# Panel S13(g)
dataF=data.frame(I.tp=c(Itp.online.1[3,],Itp.online[3,],Itp.full[3,]),
                 Method=as.factor(c(rep(" OSG-Long ",length(id.ARP)),rep(" OSG-Short ",length(id.ARP)),rep("FSG",length(id.ARP)))))
PT=ggplot(data = dataF,aes(x=Method,y=I.tp))+geom_boxplot()+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.border = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.text.y = element_text(angle=90,size=12),
                   axis.title=element_text(size=12),
                   #legend.justification=c(0,1),
                   legend.position ="right",
                   legend.title = element_blank(),
                   legend.background = element_blank(),
                   legend.key.width=unit(1,"line"),
                   legend.key.height=unit(1,"line"))+
  ylab(expression(I[tp]^{"[10,20)"}))+
  geom_hline(yintercept =1,color="red",linetype=2)
print(PT)



### Plot Figures 6(h), 9(h), and S13(h)
dataF=data.frame(lon=Dat.loc[id.ARP,1],lat=Dat.loc[id.ARP,2],
                  Itp1.online=c(Itp.online[4,]),Itp1.online.1=c(Itp.online.1[4,]),Itp1.full=c(Itp.full[4,]))
# Panel 6(h)
PT.online=ggplot() +
  geom_raster(mapping=aes(lon, lat, fill = Itp1.online.1),data=dataF)+
  scale_fill_gradient2(low = "#3288BD",mid="white",high ="#E41A1C",midpoint = 1,limits=c(0,3))+
  geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_y_continuous(limits=c(12,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position =c(0.01,0.01),
                   legend.title = element_text(),
                   legend.key.width=unit(0.8,"line"),
                   legend.key.height=unit(0.8,"line"))+
  labs(fill=expression(I[tp]^{"[20,"*infinity*")"}))+
  ylab("Latitude")+xlab("Longitude")
print(PT.online)

# Panel 9(h)
PT.online=ggplot() +
  geom_raster(mapping=aes(lon, lat, fill = Itp1.online),data=dataF)+
  scale_fill_gradient2(low = "#3288BD",mid="white",high ="#E41A1C",midpoint = 1,limits=c(0,3))+
  geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_y_continuous(limits=c(12,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position =c(0.01,0.01),
                   legend.title = element_text(),
                   legend.key.width=unit(0.8,"line"),
                   legend.key.height=unit(0.8,"line"))+
  labs(fill=expression(I[tp]^{"[20,"*infinity*")"}))+
  ylab("Latitude")+xlab("Longitude")
print(PT.online)

# Panel S13(h)
dataF=data.frame(I.tp=c(Itp.online.1[4,],Itp.online[4,],Itp.full[4,]),
                 Method=as.factor(c(rep(" OSG-Long ",length(id.ARP)),rep(" OSG-Short ",length(id.ARP)),rep("FSG",length(id.ARP)))))
PT=ggplot(data = dataF,aes(x=Method,y=I.tp))+geom_boxplot()+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.border = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.text.y = element_text(angle=90,size=12),
                   axis.title=element_text(size=12),
                   #legend.justification=c(0,1),
                   legend.position ="right",
                   legend.title = element_blank(),
                   legend.background = element_blank(),
                   legend.key.width=unit(1,"line"),
                   legend.key.height=unit(1,"line"))+
  ylab(expression(I[tp]^{"[20,"*infinity*")"}))+
  geom_hline(yintercept =1,color="red",linetype=2)
print(PT)



### Plot Figures 6(i), 9(i), S9(e), S12(e), and S13(i)
dataFu=data.frame(lon=Dat.loc[id.ARP,1],lat=Dat.loc[id.ARP,2],
                  Iwdt.online=c(Iwdt.u.online),Iwdt.online.1=c(Iwdt.u.online.1),Iwdt.full=c(Iwdt.u.full))
dataFv=data.frame(lon=Dat.loc[id.ARP,1],lat=Dat.loc[id.ARP,2],
                  Iwdt.online=c(Iwdt.v.online),Iwdt.online.1=c(Iwdt.v.online.1),Iwdt.full=c(Iwdt.v.full))
# Panel 6(i)
PT.online=ggplot() +
  geom_raster(mapping=aes(lon, lat, fill = Iwdt.online.1),data=dataFu)+
  scale_fill_gradient(low = "white",high ="#E41A1C",limits=c(0,0.06))+
  geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_y_continuous(limits=c(12,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position =c(0.01,0.01),
                   legend.title = element_text(),
                   legend.key.width=unit(0.8,"line"),
                   legend.key.height=unit(0.8,"line"))+
  labs(fill=expression(I[wdt]))+
  ylab("Latitude")+xlab("Longitude")
print(PT.online)

# Panel 9(i)
PT.online=ggplot() +
  geom_raster(mapping=aes(lon, lat, fill = Iwdt.online),data=dataFu)+
  scale_fill_gradient(low = "white",high ="#E41A1C",limits=c(0,0.06))+
  geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_y_continuous(limits=c(12,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position =c(0.01,0.01),
                   legend.title = element_text(),
                   legend.key.width=unit(0.8,"line"),
                   legend.key.height=unit(0.8,"line"))+
  labs(fill=expression(I[wdt]))+
  ylab("Latitude")+xlab("Longitude")
print(PT.online)

# Panel S9(e)
PT.online=ggplot() +
  geom_raster(mapping=aes(lon, lat, fill = Iwdt.online.1),data=dataFv)+
  scale_fill_gradient(low = "white",high ="#E41A1C",limits=c(0,0.05))+
  geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_y_continuous(limits=c(12,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position =c(0.01,0.01),
                   legend.title = element_text(),
                   legend.key.width=unit(0.8,"line"),
                   legend.key.height=unit(0.8,"line"))+
  labs(fill=expression(I[wdt]))+
  ylab("Latitude")+xlab("Longitude")
print(PT.online)

# Panel S12(e)
PT.online=ggplot() +
  geom_raster(mapping=aes(lon, lat, fill = Iwdt.online),data=dataFv)+
  scale_fill_gradient(low = "white",high ="#E41A1C",limits=c(0,0.05))+
  geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_y_continuous(limits=c(12,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position =c(0.01,0.01),
                   legend.title = element_text(),
                   legend.key.width=unit(0.8,"line"),
                   legend.key.height=unit(0.8,"line"))+
  labs(fill=expression(I[wdt]))+
  ylab("Latitude")+xlab("Longitude")
print(PT.online)

# Panel S13(i)
dataF=data.frame(I.wdt=c(0.5*Iwdt.u.online.1+0.5*Iwdt.v.online.1,0.5*Iwdt.u.online+0.5*Iwdt.v.online,0.5*Iwdt.u.full+0.5*Iwdt.v.full),
                 Method=as.factor(rep(c(" OSG-Long "," OSG-Short ","FSG"),each=length(id.ARP))))
PT=ggplot(data = dataF,aes(x=Method,y=I.wdt))+geom_boxplot()+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.border = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.text.y = element_text(angle=90,size=12),
                   axis.title=element_text(size=12),
                   #legend.justification=c(0,1),
                   legend.position ="right",
                   legend.title = element_blank(),
                   legend.background = element_blank(),
                   legend.key.width=unit(1,"line"),
                   legend.key.height=unit(1,"line"))+
  ylab(expression(I[wdt]))+
  geom_hline(yintercept =0,color="red",linetype=2)+
  geom_hline(yintercept =median(0.5*Iwdt.u.online.1+0.5*Iwdt.v.online.1),color="gray",linetype=2)
print(PT)



### Plot Figures 6(j), 9(j), S9(f), S12(f), and S13(j)
dataSu=data.frame(tseq=1:(2*365*8),
                  Iwds.online.1=c(Iwds.u.online.1)[c(1:(365*8),(365*8+(9-1)*365*8+1):(365*8+9*365*8))],
                  Iwds.online=c(Iwds.u.online)[c(1:(365*8),(365*8+(9-1)*365*8+1):(365*8+9*365*8))],
                  Iwds.full=c(Iwds.u.full)[c(1:(365*8),(365*8+(9-1)*365*8+1):(365*8+9*365*8))])
dataSv=data.frame(tseq=1:(2*365*8),
                  Iwds.online.1=c(Iwds.v.online.1)[c(1:(365*8),(365*8+(9-1)*365*8+1):(365*8+9*365*8))],
                  Iwds.online=c(Iwds.v.online)[c(1:(365*8),(365*8+(9-1)*365*8+1):(365*8+9*365*8))],
                  Iwds.full=c(Iwds.v.full)[c(1:(365*8),(365*8+(9-1)*365*8+1):(365*8+9*365*8))])
# Panel 6(j)
PT.online=ggplot() +
  geom_line(mapping=aes(x=tseq, y= Iwds.online.1),data=dataSu)+
  scale_y_continuous(limits = c(0,0.17))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position =c(0.01,0.01),
                   legend.title = element_text(),
                   legend.key.width=unit(0.8,"line"),
                   legend.key.height=unit(0.8,"line"))+
  ylab(expression(I[wds]))+xlab(" ")+
  geom_vline(xintercept = 365*8,linetype=2,colour="gray")+
  scale_x_continuous(breaks=c(1460,4380),labels=c("2014","2023"))
print(PT.online)

# Panel 9(j)
PT.online=ggplot() +
  geom_line(mapping=aes(x=tseq, y= Iwds.online),data=dataSu)+
  scale_y_continuous(limits = c(0,0.17))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position =c(0.01,0.01),
                   legend.title = element_text(),
                   legend.key.width=unit(0.8,"line"),
                   legend.key.height=unit(0.8,"line"))+
  ylab(expression(I[wds]))+xlab(" ")+
  geom_vline(xintercept = 365*8,linetype=2,color="Gray")+
  scale_x_continuous(breaks=c(1460,4380),labels=c("2014","2023"))
print(PT.online)

# Panel S9(f)
PT.online=ggplot() +
  geom_line(mapping=aes(x=tseq, y= Iwds.online.1),data=dataSv)+
  scale_y_continuous(limits = c(0,0.24))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position =c(0.01,0.01),
                   legend.title = element_text(),
                   legend.key.width=unit(0.8,"line"),
                   legend.key.height=unit(0.8,"line"))+
  ylab(expression(I[wds]))+xlab(" ")+
  geom_vline(xintercept = 365*8,linetype=2,color="gray")+
  scale_x_continuous(breaks=c(1460,4380),labels=c("2014","2023"))
print(PT.online)

# Panel S12(f)
PT.online=ggplot() +
  geom_line(mapping=aes(x=tseq, y= Iwds.online),data=dataSv)+
  scale_y_continuous(limits = c(0,0.24))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position =c(0.01,0.01),
                   legend.title = element_text(),
                   legend.key.width=unit(0.8,"line"),
                   legend.key.height=unit(0.8,"line"))+
  ylab(expression(I[wds]))+xlab(" ")+
  geom_vline(xintercept = 365*8,linetype=2,color="gray")+
  scale_x_continuous(breaks=c(1460,4380),labels=c("2014","2023"))
print(PT.online)

# Panel S13(j)
dataF=data.frame(I.wds=c(0.5*Iwds.u.online.1+0.5*Iwds.v.online.1,0.5*Iwds.u.online+0.5*Iwds.v.online,0.5*Iwds.u.full+0.5*Iwds.v.full),
                 Method=as.factor(rep(c(" OSG-Long "," OSG-Short ","FSG"),each=T0+B*Tt)))
PT=ggplot(data = dataF,aes(x=Method,y=I.wds))+geom_boxplot()+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.border = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.text.y = element_text(angle=90,size=12),
                   axis.title=element_text(size=12),
                   #legend.justification=c(0,1),
                   legend.position ="right",
                   legend.title = element_blank(),
                   legend.background = element_blank(),
                   legend.key.width=unit(1,"line"),
                   legend.key.height=unit(1,"line"))+
  ylab(expression(I[wds]))+
  geom_hline(yintercept =0,color="red",linetype=2)+
  geom_hline(yintercept =median(0.5*Iwds.u.online.1+0.5*Iwds.v.online.1),color="gray",linetype=2)
print(PT)



### Plot Figures 6(k), 9(k), S9(g), S12(g), and S13(k)
dataFu=data.frame(lon=Dat.loc[id.ARP,1],lat=Dat.loc[id.ARP,2],Imd.online=c(Imd.online[1:length(id.ARP)]),
                  Imd.online.1=c(Imd.online.1[1:length(id.ARP)]),Imd.full=c(Imd.full[1:length(id.ARP)]))
dataFv=data.frame(lon=Dat.loc[id.ARP,1],lat=Dat.loc[id.ARP,2],Imd.online=c(Imd.online[-(1:length(id.ARP))]),
                  Imd.online.1=c(Imd.online.1[-(1:length(id.ARP))]),Imd.full=c(Imd.full[-(1:length(id.ARP))]))
# Panel 6(k)
PT.online=ggplot() +
  geom_raster(mapping=aes(lon, lat, fill = Imd.online.1),data=dataFu)+
  scale_fill_gradient2(low ="white",high ="#E41A1C",limits=c(0,0.2))+
  geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_y_continuous(limits=c(12,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position =c(0.01,0.01),
                   legend.title = element_text(),
                   legend.key.width=unit(0.8,"line"),
                   legend.key.height=unit(0.8,"line"))+
  labs(fill=expression(I[md]))+
  ylab("Latitude")+xlab("Longitude")
print(PT.online)

# Panel 9(k)
PT.online=ggplot() +
  geom_raster(mapping=aes(lon, lat, fill = Imd.online),data=dataFu)+
  scale_fill_gradient2(low ="white",high ="#E41A1C",limits=c(0,0.2))+
  geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_y_continuous(limits=c(12,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position =c(0.01,0.01),
                   legend.title = element_text(),
                   legend.key.width=unit(0.8,"line"),
                   legend.key.height=unit(0.8,"line"))+
  labs(fill=expression(I[md]))+
  ylab("Latitude")+xlab("Longitude")
print(PT.online)

# Panel S9(g)
PT.online=ggplot() +
  geom_raster(mapping=aes(lon, lat, fill = Imd.online.1),data=dataFv)+
  scale_fill_gradient2(low ="white",high ="#E41A1C",limits=c(0,0.2))+
  geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_y_continuous(limits=c(12,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position =c(0.01,0.01),
                   legend.title = element_text(),
                   legend.key.width=unit(0.8,"line"),
                   legend.key.height=unit(0.8,"line"))+
  labs(fill=expression(I[md]))+
  ylab("Latitude")+xlab("Longitude")
print(PT.online)

# Panel S12(g)
PT.online=ggplot() +
  geom_raster(mapping=aes(lon, lat, fill = Imd.online),data=dataFv)+
  scale_fill_gradient2(low ="white",high ="#E41A1C",limits=c(0,0.2))+
  geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_y_continuous(limits=c(12,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position =c(0.01,0.01),
                   legend.title = element_text(),
                   legend.key.width=unit(0.8,"line"),
                   legend.key.height=unit(0.8,"line"))+
  labs(fill=expression(I[md]))+
  ylab("Latitude")+xlab("Longitude")
print(PT.online)

# Panel S13(k)
dataF=data.frame(I.md=c(0.5*Imd.online.1[1:length(id.ARP)]+0.5*Imd.online.1[-(1:length(id.ARP))],
                        0.5*Imd.online[1:length(id.ARP)]+0.5*Imd.online[-(1:length(id.ARP))],
                        0.5*Imd.full[1:length(id.ARP)]+0.5*Imd.full[-(1:length(id.ARP))]),
                 Method=as.factor(c(rep(" OSG-Long ",length(id.ARP)),rep(" OSG-Short ",length(id.ARP)),rep("FSG",length(id.ARP)))))
PT=ggplot(data = dataF,aes(x=Method,y=I.md))+geom_boxplot()+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.border = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.text.y = element_text(angle=90,size=12),
                   axis.title=element_text(size=12),
                   #legend.justification=c(0,1),
                   legend.position ="right",
                   legend.title = element_blank(),
                   legend.background = element_blank(),
                   legend.key.width=unit(1,"line"),
                   legend.key.height=unit(1,"line"))+
  ylab(expression(I[md]))+
  geom_hline(yintercept =0,color="red",linetype=2)+
  geom_hline(yintercept =median(0.5*Imd.online.1[1:length(id.ARP)]+0.5*Imd.online.1[-(1:length(id.ARP))]),color="gray",linetype=2)
print(PT)



### Plot Figures 6(l), 9(l), S9(h), S12(h), and S13(l)
dataFu=data.frame(lon=Dat.loc[id.ARP,1],lat=Dat.loc[id.ARP,2],Isdd.online=c(Isdd.online[1:length(id.ARP)]),
                  Isdd.online.1=c(Isdd.online.1[1:length(id.ARP)]),Isdd.full=c(Isdd.full[1:length(id.ARP)]))
dataFv=data.frame(lon=Dat.loc[id.ARP,1],lat=Dat.loc[id.ARP,2],Isdd.online=c(Isdd.online[-(1:length(id.ARP))]),
                  Isdd.online.1=c(Isdd.online.1[-(1:length(id.ARP))]),Isdd.full=c(Isdd.full[-(1:length(id.ARP))]))
# Panel 6(l)
PT.online=ggplot() +
  geom_raster(mapping=aes(lon, lat, fill = Isdd.online.1),data=dataFu)+
  scale_fill_gradient2(low ="white",high ="#E41A1C",limits=c(0,0.3))+
  geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_y_continuous(limits=c(12,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position =c(0.01,0.01),
                   legend.title = element_text(),
                   legend.key.width=unit(0.8,"line"),
                   legend.key.height=unit(0.8,"line"))+
  labs(fill=expression(I[sdd]))+
  ylab("Latitude")+xlab("Longitude")
print(PT.online)

# Panel 9(l)
PT.online=ggplot() +
  geom_raster(mapping=aes(lon, lat, fill = Isdd.online),data=dataFu)+
  scale_fill_gradient2(low ="white",high ="#E41A1C",limits=c(0,0.3))+
  geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_y_continuous(limits=c(12,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position =c(0.01,0.01),
                   legend.title = element_text(),
                   legend.key.width=unit(0.8,"line"),
                   legend.key.height=unit(0.8,"line"))+
  labs(fill=expression(I[sdd]))+
  ylab("Latitude")+xlab("Longitude")
print(PT.online)

# Panel S9(h)
PT.online=ggplot() +
  geom_raster(mapping=aes(lon, lat, fill = Isdd.online.1),data=dataFv)+
  scale_fill_gradient2(low ="white",high ="#E41A1C",limits=c(0,0.31))+
  geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_y_continuous(limits=c(12,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position =c(0.01,0.01),
                   legend.title = element_text(),
                   legend.key.width=unit(0.8,"line"),
                   legend.key.height=unit(0.8,"line"))+
  labs(fill=expression(I[sdd]))+
  ylab("Latitude")+xlab("Longitude")
print(PT.online)

# Panel S12(h)
PT.online=ggplot() +
  geom_raster(mapping=aes(lon, lat, fill = Isdd.online),data=dataFv)+
  scale_fill_gradient2(low ="white",high ="#E41A1C",limits=c(0,0.31))+
  geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_y_continuous(limits=c(12,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position =c(0.01,0.01),
                   legend.title = element_text(),
                   legend.key.width=unit(0.8,"line"),
                   legend.key.height=unit(0.8,"line"))+
  labs(fill=expression(I[sdd]))+
  ylab("Latitude")+xlab("Longitude")
print(PT.online)

# Panel S13(l)
dataF=data.frame(I.sdd=c(0.5*Isdd.online.1[1:length(id.ARP)]+0.5*Isdd.online.1[-(1:length(id.ARP))],
                         0.5*Isdd.online[1:length(id.ARP)]+0.5*Isdd.online[-(1:length(id.ARP))],
                         0.5*Isdd.full[1:length(id.ARP)]+0.5*Isdd.full[-(1:length(id.ARP))]),
                 Method=as.factor(c(rep(" OSG-Long ",length(id.ARP)),rep(" OSG-Short ",length(id.ARP)),rep("FSG",length(id.ARP)))))
PT=ggplot(data = dataF,aes(x=Method,y=I.sdd))+geom_boxplot()+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.border = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.text.y = element_text(angle=90,size=12),
                   axis.title=element_text(size=12),
                   #legend.justification=c(0,1),
                   legend.position ="right",
                   legend.title = element_blank(),
                   legend.background = element_blank(),
                   legend.key.width=unit(1,"line"),
                   legend.key.height=unit(1,"line"))+
  ylab(expression(I[sdd]))+
  geom_hline(yintercept =0,color="red",linetype=2)+
  geom_hline(yintercept =median(0.5*Isdd.online.1[1:length(id.ARP)]+0.5*Isdd.online.1[-(1:length(id.ARP))]),color="gray",linetype=2)
print(PT)



### Plot Figures S9(j), S9(l), S12(j), S12(l), and S13(n)
dataFu=data.frame(lon=Dat.loc[id.ARP,1],lat=Dat.loc[id.ARP,2],I75qd.online=c(I75qd.online[1:length(id.ARP)]),
                  I75qd.online.1=c(I75qd.online.1[1:length(id.ARP)]),I75qd.full=c(I75qd.full[1:length(id.ARP)]))
dataFv=data.frame(lon=Dat.loc[id.ARP,1],lat=Dat.loc[id.ARP,2],I75qd.online=c(I75qd.online[-(1:length(id.ARP))]),
                  I75qd.online.1=c(I75qd.online.1[-(1:length(id.ARP))]),I75qd.full=c(I75qd.full[-(1:length(id.ARP))]))
# Panel S9(j)
PT.online=ggplot() +
  geom_raster(mapping=aes(lon, lat, fill = I75qd.online.1),data=dataFu)+
  scale_fill_gradient2(low ="white",high ="#E41A1C",limits=c(0,0.3))+
  geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_y_continuous(limits=c(12,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position =c(0.01,0.01),
                   legend.title = element_text(),
                   legend.key.width=unit(0.8,"line"),
                   legend.key.height=unit(0.8,"line"))+
  labs(fill=expression(I[qd]^"0.75"))+
  ylab("Latitude")+xlab("Longitude")
print(PT.online)

# Panel S12(j)
PT.online=ggplot() +
  geom_raster(mapping=aes(lon, lat, fill = I75qd.online),data=dataFu)+
  scale_fill_gradient2(low ="white",high ="#E41A1C",limits=c(0,0.3))+
  geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_y_continuous(limits=c(12,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position =c(0.01,0.01),
                   legend.title = element_text(),
                   legend.key.width=unit(0.8,"line"),
                   legend.key.height=unit(0.8,"line"))+
  labs(fill=expression(I[qd]^"0.75"))+
  ylab("Latitude")+xlab("Longitude")
print(PT.online)

# Panel S9(l)
PT.online=ggplot() +
  geom_raster(mapping=aes(lon, lat, fill = I75qd.online.1),data=dataFv)+
  scale_fill_gradient2(low ="white",high ="#E41A1C",limits=c(0,0.3))+
  geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_y_continuous(limits=c(12,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position =c(0.01,0.01),
                   legend.title = element_text(),
                   legend.key.width=unit(0.8,"line"),
                   legend.key.height=unit(0.8,"line"))+
  labs(fill=expression(I[qd]^"0.75"))+
  ylab("Latitude")+xlab("Longitude")
print(PT.online)

# Panel S12(l)
PT.online=ggplot() +
  geom_raster(mapping=aes(lon, lat, fill = I75qd.online),data=dataFv)+
  scale_fill_gradient2(low ="white",high ="#E41A1C",limits=c(0,0.3))+
  geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_y_continuous(limits=c(12,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position =c(0.01,0.01),
                   legend.title = element_text(),
                   legend.key.width=unit(0.8,"line"),
                   legend.key.height=unit(0.8,"line"))+
  labs(fill=expression(I[qd]^"0.75"))+
  ylab("Latitude")+xlab("Longitude")
print(PT.online)

# Panel S13(n)
dataF=data.frame(I.75qd=c(0.5*I75qd.online.1[1:length(id.ARP)]+0.5*I75qd.online.1[-(1:length(id.ARP))],
                          0.5*I75qd.online[1:length(id.ARP)]+0.5*I75qd.online[-(1:length(id.ARP))],
                          0.5*I75qd.full[1:length(id.ARP)]+0.5*I75qd.full[-(1:length(id.ARP))]),
                 Method=as.factor(c(rep(" OSG-Long ",length(id.ARP)),rep(" OSG-Short ",length(id.ARP)),rep("FSG",length(id.ARP)))))
PT=ggplot(data = dataF,aes(x=Method,y=I.75qd))+geom_boxplot()+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.border = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.text.y = element_text(angle=90,size=12),
                   axis.title=element_text(size=12),
                   #legend.justification=c(0,1),
                   legend.position ="right",
                   legend.title = element_blank(),
                   legend.background = element_blank(),
                   legend.key.width=unit(1,"line"),
                   legend.key.height=unit(1,"line"))+
  ylab(expression(I[qd]^"0.75"))+
  geom_hline(yintercept =0,color="red",linetype=2)+
  geom_hline(yintercept =median(0.5*I75qd.online.1[1:length(id.ARP)]+0.5*I75qd.online.1[-(1:length(id.ARP))]),color="gray",linetype=2)
print(PT)



### Plot Figures S9(i), S9(k), S12(i), S12(k), and S13(m)
dataFu=data.frame(lon=Dat.loc[id.ARP,1],lat=Dat.loc[id.ARP,2],I25qd.online=c(I25qd.online[1:length(id.ARP)]),
                  I25qd.online.1=c(I25qd.online.1[1:length(id.ARP)]),I25qd.full=c(I25qd.full[1:length(id.ARP)]))
dataFv=data.frame(lon=Dat.loc[id.ARP,1],lat=Dat.loc[id.ARP,2],I25qd.online=c(I25qd.online[-(1:length(id.ARP))]),
                  I25qd.online.1=c(I25qd.online.1[-(1:length(id.ARP))]),I25qd.full=c(I25qd.full[-(1:length(id.ARP))]))
# Panel S9(i)
PT.online=ggplot() +
  geom_raster(mapping=aes(lon, lat, fill = I25qd.online.1),data=dataFu)+
  scale_fill_gradient2(low ="white",high ="#E41A1C",limits=c(0,0.3))+
  geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_y_continuous(limits=c(12,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position =c(0.01,0.01),
                   legend.title = element_text(),
                   legend.key.width=unit(0.8,"line"),
                   legend.key.height=unit(0.8,"line"))+
  labs(fill=expression(I[qd]^"0.25"))+
  ylab("Latitude")+xlab("Longitude")
print(PT.online)

# Panel S12(i)
PT.online=ggplot() +
  geom_raster(mapping=aes(lon, lat, fill = I25qd.online),data=dataFu)+
  scale_fill_gradient2(low ="white",high ="#E41A1C",limits=c(0,0.3))+
  geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_y_continuous(limits=c(12,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position =c(0.01,0.01),
                   legend.title = element_text(),
                   legend.key.width=unit(0.8,"line"),
                   legend.key.height=unit(0.8,"line"))+
  labs(fill=expression(I[qd]^"0.25"))+
  ylab("Latitude")+xlab("Longitude")
print(PT.online)

# Panel S9(k)
PT.online=ggplot() +
  geom_raster(mapping=aes(lon, lat, fill = I25qd.online.1),data=dataFv)+
  scale_fill_gradient2(low ="white",high ="#E41A1C",limits=c(0,0.3))+
  geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_y_continuous(limits=c(12,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position =c(0.01,0.01),
                   legend.title = element_text(),
                   legend.key.width=unit(0.8,"line"),
                   legend.key.height=unit(0.8,"line"))+
  labs(fill=expression(I[qd]^"0.25"))+
  ylab("Latitude")+xlab("Longitude")
print(PT.online)

# Panel S12(k)
PT.online=ggplot() +
  geom_raster(mapping=aes(lon, lat, fill = I25qd.online),data=dataFv)+
  scale_fill_gradient2(low ="white",high ="#E41A1C",limits=c(0,0.3))+
  geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_y_continuous(limits=c(12,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position =c(0.01,0.01),
                   legend.title = element_text(),
                   legend.key.width=unit(0.8,"line"),
                   legend.key.height=unit(0.8,"line"))+
  labs(fill=expression(I[qd]^"0.25"))+
  ylab("Latitude")+xlab("Longitude")
print(PT.online)

# Panel S13(n)
dataF=data.frame(I.25qd=c(0.5*I25qd.online.1[1:length(id.ARP)]+0.5*I25qd.online.1[-(1:length(id.ARP))],
                          0.5*I25qd.online[1:length(id.ARP)]+0.5*I25qd.online[-(1:length(id.ARP))],
                          0.5*I25qd.full[1:length(id.ARP)]+0.5*I25qd.full[-(1:length(id.ARP))]),
                 Method=as.factor(c(rep(" OSG-Long ",length(id.ARP)),rep(" OSG-Short ",length(id.ARP)),rep("FSG",length(id.ARP)))))
PT=ggplot(data = dataF,aes(x=Method,y=I.25qd))+geom_boxplot()+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.border = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.text.y = element_text(angle=90,size=12),
                   axis.title=element_text(size=12),
                   #legend.justification=c(0,1),
                   legend.position ="right",
                   legend.title = element_blank(),
                   legend.background = element_blank(),
                   legend.key.width=unit(1,"line"),
                   legend.key.height=unit(1,"line"))+
  ylab(expression(I[qd]^"0.25"))+
  geom_hline(yintercept =0,color="red",linetype=2)+
  geom_hline(yintercept =median(0.5*I25qd.online.1[1:length(id.ARP)]+0.5*I25qd.online.1[-(1:length(id.ARP))]),color="gray",linetype=2)
print(PT)


