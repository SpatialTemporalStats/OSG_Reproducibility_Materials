#######################################################################################
# This file reproduces Figure S14.                                                    #
#######################################################################################
# Please ensure that you have run the Rscripts "FSG.R", "Scenario1_OSG_Long.R", "Scenario2_OSG_Short.R", 
# "MC_FSG.R", "MC_OSG_Long.R", "MC_OSG_Short.R" and saved all outputs.
Col=c("#3288BD","#66C2A5","#ABDDA4","#E6F598","#FEE08B","#FDAE61","#F46D43","#D53E4F")


# ARP information for ploting figures
world_map=map_data("world")                                                   # Extract the map data for the region
arabian_peninsula_countries=c("Saudi Arabia", "Yemen", 
                              "Oman", "United Arab Emirates", 
                              "Qatar", "Bahrain", "Kuwait")                   # Define the countries of the Arabian Peninsula
arabian_peninsula=subset(world_map, region %in% arabian_peninsula_countries)  # Filter the map data for the Arabian Peninsula countries
dataARP=data.frame(lon=rep(arabian_peninsula$long,times=2),lat=rep(arabian_peninsula$lat,times=2),
                   type=rep(arabian_peninsula$group,times=2),
                   group=as.factor(rep(c("U","V"),each=length(arabian_peninsula$long))))



###### Part 1. MC results for Iuq.u
# FSG
Iuq.full=matrix(0,1215,10)
Iuq.full[,1]=read.csv(here("Case_Studies/FSG","Iuq_u_full.csv"))$x
Iuq.full[,2]=read.csv(here("Case_Studies/MC","Iuq_u_full_1.csv"))$x
Iuq.full[,3]=read.csv(here("Case_Studies/MC","Iuq_u_full_2.csv"))$x
Iuq.full[,4]=read.csv(here("Case_Studies/MC","Iuq_u_full_3.csv"))$x
Iuq.full[,5]=read.csv(here("Case_Studies/MC","Iuq_u_full_4.csv"))$x
Iuq.full[,6]=read.csv(here("Case_Studies/MC","Iuq_u_full_5.csv"))$x
Iuq.full[,7]=read.csv(here("Case_Studies/MC","Iuq_u_full_6.csv"))$x
Iuq.full[,8]=read.csv(here("Case_Studies/MC","Iuq_u_full_7.csv"))$x
Iuq.full[,9]=read.csv(here("Case_Studies/MC","Iuq_u_full_8.csv"))$x
Iuq.full[,10]=read.csv(here("Case_Studies/MC","Iuq_u_full_9.csv"))$x
# OSG-Long
Iuq.long=matrix(0,1215,10)
Iuq.long[,1]=read.csv(here("Case_Studies/Scenario1_OSG_Long","Iuq_u_online_1.csv"))$x
Iuq.long[,2]=read.csv(here("Case_Studies/MC","Iuq_u_long_1.csv"))$x
Iuq.long[,3]=read.csv(here("Case_Studies/MC","Iuq_u_long_2.csv"))$x
Iuq.long[,4]=read.csv(here("Case_Studies/MC","Iuq_u_long_3.csv"))$x
Iuq.long[,5]=read.csv(here("Case_Studies/MC","Iuq_u_long_4.csv"))$x
Iuq.long[,6]=read.csv(here("Case_Studies/MC","Iuq_u_long_5.csv"))$x
Iuq.long[,7]=read.csv(here("Case_Studies/MC","Iuq_u_long_6.csv"))$x
Iuq.long[,8]=read.csv(here("Case_Studies/MC","Iuq_u_long_7.csv"))$x
Iuq.long[,9]=read.csv(here("Case_Studies/MC","Iuq_u_long_8.csv"))$x
Iuq.long[,10]=read.csv(here("Case_Studies/MC","Iuq_u_long_9.csv"))$x
# OSG-Short
Iuq.short=matrix(0,1215,10)
Iuq.short[,1]=read.csv(here("Case_Studies/Scenario2_OSG_Short","Iuq_u_online_2.csv"))$x
Iuq.short[,2]=read.csv(here("Case_Studies/MC","Iuq_u_short_1.csv"))$x
Iuq.short[,3]=read.csv(here("Case_Studies/MC","Iuq_u_short_2.csv"))$x
Iuq.short[,4]=read.csv(here("Case_Studies/MC","Iuq_u_short_3.csv"))$x
Iuq.short[,5]=read.csv(here("Case_Studies/MC","Iuq_u_short_4.csv"))$x
Iuq.short[,6]=read.csv(here("Case_Studies/MC","Iuq_u_short_5.csv"))$x
Iuq.short[,7]=read.csv(here("Case_Studies/MC","Iuq_u_short_6.csv"))$x
Iuq.short[,8]=read.csv(here("Case_Studies/MC","Iuq_u_short_7.csv"))$x
Iuq.short[,9]=read.csv(here("Case_Studies/MC","Iuq_u_short_8.csv"))$x
Iuq.short[,10]=read.csv(here("Case_Studies/MC","Iuq_u_short_9.csv"))$x

# Panel S14(a)
dataFu=data.frame(lon=rep(Dat.loc[id.ARP,1],times=3),lat=rep(Dat.loc[id.ARP,2],times=3),
                  Iuq.full.mean=c(apply(Iuq.full,1,mean),apply(Iuq.long,1,mean),apply(Iuq.short,1,mean)),
                  Iuq.full.sd=c(apply(Iuq.full,1,sd),apply(Iuq.long,1,sd),apply(Iuq.short,1,sd)),
                  group=as.factor(rep(c("FSG","OSG-Long","OSG-Short"),each=1215)))
PT=ggplot() + facet_wrap(~ group, ncol = 3)+
  geom_raster(mapping=aes(lon, lat, fill = Iuq.full.mean),data=dataFu)+
  scale_fill_gradient(low = "white",high ="#E41A1C",limits=c(1,1.43))+
  # geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_y_continuous(limits=c(12,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position = "right",
                   legend.title = element_text(),
                   legend.key.width=unit(0.5,"line"),
                   legend.key.height=unit(1,"line"))+
  labs(fill=expression(Mean(I[uq])))+
  ylab("Latitude")+xlab("Longitude")
print(PT)  # 7.30*2.67

# Panel S14(b)
PT=ggplot() + facet_wrap(~ group, ncol = 3)+
  geom_raster(mapping=aes(lon, lat, fill = Iuq.full.sd),data=dataFu)+
  scale_fill_gradient(low = "white",high ="#E41A1C",limits=c(0,0.01))+
  # geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_y_continuous(limits=c(12,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position = "right",
                   legend.title = element_text(),
                   legend.key.width=unit(0.5,"line"),
                   legend.key.height=unit(1,"line"))+
  labs(fill=expression(~~~~SD(I[uq])))+
  ylab("Latitude")+xlab("Longitude")
print(PT)  # 7.30*2.67


###### Part 2. MC results for Ibc
# FSG
Ibc.full=matrix(0,1215,10)
a=read.csv(here("Case_Studies/FSG","Ibc_full.csv"))$x
Ibc.full[,1]=a[1:length(id.ARP)]-a[-(1:length(id.ARP))]
a=read.csv(here("Case_Studies/MC","Ibc_full_1.csv"))$x
Ibc.full[,2]=a[1:length(id.ARP)]-a[-(1:length(id.ARP))]
a=read.csv(here("Case_Studies/MC","Ibc_full_2.csv"))$x
Ibc.full[,3]=a[1:length(id.ARP)]-a[-(1:length(id.ARP))]
a=read.csv(here("Case_Studies/MC","Ibc_full_3.csv"))$x
Ibc.full[,4]=a[1:length(id.ARP)]-a[-(1:length(id.ARP))]
a=read.csv(here("Case_Studies/MC","Ibc_full_4.csv"))$x
Ibc.full[,5]=a[1:length(id.ARP)]-a[-(1:length(id.ARP))]
a=read.csv(here("Case_Studies/MC","Ibc_full_5.csv"))$x
Ibc.full[,6]=a[1:length(id.ARP)]-a[-(1:length(id.ARP))]
a=read.csv(here("Case_Studies/MC","Ibc_full_6.csv"))$x
Ibc.full[,7]=a[1:length(id.ARP)]-a[-(1:length(id.ARP))]
a=read.csv(here("Case_Studies/MC","Ibc_full_7.csv"))$x
Ibc.full[,8]=a[1:length(id.ARP)]-a[-(1:length(id.ARP))]
a=read.csv(here("Case_Studies/MC","Ibc_full_8.csv"))$x
Ibc.full[,9]=a[1:length(id.ARP)]-a[-(1:length(id.ARP))]
a=read.csv(here("Case_Studies/MC","Ibc_full_9.csv"))$x
Ibc.full[,10]=a[1:length(id.ARP)]-a[-(1:length(id.ARP))]
# OSG-Long
Ibc.long=matrix(0,1215,10)
a=read.csv(here("Case_Studies/Scenario1_OSG_Long","Ibc_online_1.csv"))$x
Ibc.long[,1]=a[1:length(id.ARP)]-a[-(1:length(id.ARP))]
a=read.csv(here("Case_Studies/MC","Ibc_long_1.csv"))$x
Ibc.long[,2]=a[1:length(id.ARP)]-a[-(1:length(id.ARP))]
a=read.csv(here("Case_Studies/MC","Ibc_long_2.csv"))$x
Ibc.long[,3]=a[1:length(id.ARP)]-a[-(1:length(id.ARP))]
a=read.csv(here("Case_Studies/MC","Ibc_long_3.csv"))$x
Ibc.long[,4]=a[1:length(id.ARP)]-a[-(1:length(id.ARP))]
a=read.csv(here("Case_Studies/MC","Ibc_long_4.csv"))$x
Ibc.long[,5]=a[1:length(id.ARP)]-a[-(1:length(id.ARP))]
a=read.csv(here("Case_Studies/MC","Ibc_long_5.csv"))$x
Ibc.long[,6]=a[1:length(id.ARP)]-a[-(1:length(id.ARP))]
a=read.csv(here("Case_Studies/MC","Ibc_long_6.csv"))$x
Ibc.long[,7]=a[1:length(id.ARP)]-a[-(1:length(id.ARP))]
a=read.csv(here("Case_Studies/MC","Ibc_long_7.csv"))$x
Ibc.long[,8]=a[1:length(id.ARP)]-a[-(1:length(id.ARP))]
a=read.csv(here("Case_Studies/MC","Ibc_long_8.csv"))$x
Ibc.long[,9]=a[1:length(id.ARP)]-a[-(1:length(id.ARP))]
a=read.csv(here("Case_Studies/MC","Ibc_long_9.csv"))$x
Ibc.long[,10]=a[1:length(id.ARP)]-a[-(1:length(id.ARP))]
# OSG-Short
Ibc.short=matrix(0,1215,10)
a=read.csv(here("Case_Studies/Scenario2_OSG_Short","Ibc_online_2.csv"))$x
Ibc.short[,1]=a[1:length(id.ARP)]-a[-(1:length(id.ARP))]
a=read.csv(here("Case_Studies/MC","Ibc_short_1.csv"))$x
Ibc.short[,2]=a[1:length(id.ARP)]-a[-(1:length(id.ARP))]
a=read.csv(here("Case_Studies/MC","Ibc_short_2.csv"))$x
Ibc.short[,3]=a[1:length(id.ARP)]-a[-(1:length(id.ARP))]
a=read.csv(here("Case_Studies/MC","Ibc_short_3.csv"))$x
Ibc.short[,4]=a[1:length(id.ARP)]-a[-(1:length(id.ARP))]
a=read.csv(here("Case_Studies/MC","Ibc_short_4.csv"))$x
Ibc.short[,5]=a[1:length(id.ARP)]-a[-(1:length(id.ARP))]
a=read.csv(here("Case_Studies/MC","Ibc_short_5.csv"))$x
Ibc.short[,6]=a[1:length(id.ARP)]-a[-(1:length(id.ARP))]
a=read.csv(here("Case_Studies/MC","Ibc_short_6.csv"))$x
Ibc.short[,7]=a[1:length(id.ARP)]-a[-(1:length(id.ARP))]
a=read.csv(here("Case_Studies/MC","Ibc_short_7.csv"))$x
Ibc.short[,8]=a[1:length(id.ARP)]-a[-(1:length(id.ARP))]
a=read.csv(here("Case_Studies/MC","Ibc_short_8.csv"))$x
Ibc.short[,9]=a[1:length(id.ARP)]-a[-(1:length(id.ARP))]
a=read.csv(here("Case_Studies/MC","Ibc_short_9.csv"))$x
Ibc.short[,10]=a[1:length(id.ARP)]-a[-(1:length(id.ARP))]

# Panel S14(c)
dataF=data.frame(lon=rep(Dat.loc[id.ARP,1],times=3),lat=rep(Dat.loc[id.ARP,2],times=3),
                 Ibc.mean=c(apply(Ibc.full,1,mean),apply(Ibc.long,1,mean),apply(Ibc.short,1,mean)),
                 Ibc.sd=c(apply(Ibc.full,1,sd),apply(Ibc.long,1,sd),apply(Ibc.short,1,sd)),
                 group=as.factor(rep(c("FSG","OSG-Long","OSG-Short"),each=1215)))
PT=ggplot() + facet_wrap(~ group, ncol = 3)+
  geom_raster(mapping=aes(lon, lat, fill = Ibc.mean),data=dataF)+
  scale_fill_gradient2(low = "#3288BD",mid="white",high = "#E41A1C",midpoint=0,limits=c(-0.23,0.23))+
  # geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_y_continuous(limits=c(12,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position = "right",
                   legend.title = element_text(),
                   legend.key.width=unit(0.5,"line"),
                   legend.key.height=unit(1,"line"))+
  labs(fill=expression(Mean(I[bc])))+
  ylab("Latitude")+xlab("Longitude")
print(PT)  # 7.30*2.67

# Panel S14(d)
PT=ggplot() + facet_wrap(~ group, ncol = 3)+
  geom_raster(mapping=aes(lon, lat, fill = Ibc.sd),data=dataF)+
  scale_fill_gradient(low = "white",high ="#E41A1C",limits=c(0,0.006))+
  # geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_y_continuous(limits=c(12,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position = "right",
                   legend.title = element_text(),
                   legend.key.width=unit(0.5,"line"),
                   legend.key.height=unit(1,"line"))+
  labs(fill=expression(~~~~SD(I[bc])))+
  ylab("Latitude")+xlab("Longitude")
print(PT)  # 7.30*2.67


###### Part 3. MC results for Itc1
# FSG
Itc.full=matrix(0,1215,10)
a=read.csv(here("Case_Studies/FSG","Itc1_u_full.csv"))$x
Itc.full[,1]=a[1:length(id.ARP)]-a[-(1:length(id.ARP))]
a=read.csv(here("Case_Studies/MC","Itc1_u_full_1.csv"))$x
Itc.full[,2]=a[1:length(id.ARP)]-a[-(1:length(id.ARP))]
a=read.csv(here("Case_Studies/MC","Itc1_u_full_2.csv"))$x
Itc.full[,3]=a[1:length(id.ARP)]-a[-(1:length(id.ARP))]
a=read.csv(here("Case_Studies/MC","Itc1_u_full_3.csv"))$x
Itc.full[,4]=a[1:length(id.ARP)]-a[-(1:length(id.ARP))]
a=read.csv(here("Case_Studies/MC","Itc1_u_full_4.csv"))$x
Itc.full[,5]=a[1:length(id.ARP)]-a[-(1:length(id.ARP))]
a=read.csv(here("Case_Studies/MC","Itc1_u_full_5.csv"))$x
Itc.full[,6]=a[1:length(id.ARP)]-a[-(1:length(id.ARP))]
a=read.csv(here("Case_Studies/MC","Itc1_u_full_6.csv"))$x
Itc.full[,7]=a[1:length(id.ARP)]-a[-(1:length(id.ARP))]
a=read.csv(here("Case_Studies/MC","Itc1_u_full_7.csv"))$x
Itc.full[,8]=a[1:length(id.ARP)]-a[-(1:length(id.ARP))]
a=read.csv(here("Case_Studies/MC","Itc1_u_full_8.csv"))$x
Itc.full[,9]=a[1:length(id.ARP)]-a[-(1:length(id.ARP))]
a=read.csv(here("Case_Studies/MC","Itc1_u_full_9.csv"))$x
Itc.full[,10]=a[1:length(id.ARP)]-a[-(1:length(id.ARP))]
# OSG-Long
Itc.long=matrix(0,1215,10)
a=read.csv(here("Case_Studies/Scenario1_OSG_Long","Itc1_u_online_1.csv"))$x
Itc.long[,1]=a[1:length(id.ARP)]-a[-(1:length(id.ARP))]
a=read.csv(here("Case_Studies/MC","Itc1_u_long_1.csv"))$x
Itc.long[,2]=a[1:length(id.ARP)]-a[-(1:length(id.ARP))]
a=read.csv(here("Case_Studies/MC","Itc1_u_long_2.csv"))$x
Itc.long[,3]=a[1:length(id.ARP)]-a[-(1:length(id.ARP))]
a=read.csv(here("Case_Studies/MC","Itc1_u_long_3.csv"))$x
Itc.long[,4]=a[1:length(id.ARP)]-a[-(1:length(id.ARP))]
a=read.csv(here("Case_Studies/MC","Itc1_u_long_4.csv"))$x
Itc.long[,5]=a[1:length(id.ARP)]-a[-(1:length(id.ARP))]
a=read.csv(here("Case_Studies/MC","Itc1_u_long_5.csv"))$x
Itc.long[,6]=a[1:length(id.ARP)]-a[-(1:length(id.ARP))]
a=read.csv(here("Case_Studies/MC","Itc1_u_long_6.csv"))$x
Itc.long[,7]=a[1:length(id.ARP)]-a[-(1:length(id.ARP))]
a=read.csv(here("Case_Studies/MC","Itc1_u_long_7.csv"))$x
Itc.long[,8]=a[1:length(id.ARP)]-a[-(1:length(id.ARP))]
a=read.csv(here("Case_Studies/MC","Itc1_u_long_8.csv"))$x
Itc.long[,9]=a[1:length(id.ARP)]-a[-(1:length(id.ARP))]
a=read.csv(here("Case_Studies/MC","Itc1_u_long_9.csv"))$x
Itc.long[,10]=a[1:length(id.ARP)]-a[-(1:length(id.ARP))]
# OSG-Short
Itc.short=matrix(0,1215,10)
a=read.csv(here("Case_Studies/Scenario2_OSG_Short","Itc1_u_online_2.csv"))$x
Itc.short[,1]=a[1:length(id.ARP)]-a[-(1:length(id.ARP))]
a=read.csv(here("Case_Studies/MC","Itc1_u_short_1.csv"))$x
Itc.short[,2]=a[1:length(id.ARP)]-a[-(1:length(id.ARP))]
a=read.csv(here("Case_Studies/MC","Itc1_u_short_2.csv"))$x
Itc.short[,3]=a[1:length(id.ARP)]-a[-(1:length(id.ARP))]
a=read.csv(here("Case_Studies/MC","Itc1_u_short_3.csv"))$x
Itc.short[,4]=a[1:length(id.ARP)]-a[-(1:length(id.ARP))]
a=read.csv(here("Case_Studies/MC","Itc1_u_short_4.csv"))$x
Itc.short[,5]=a[1:length(id.ARP)]-a[-(1:length(id.ARP))]
a=read.csv(here("Case_Studies/MC","Itc1_u_short_5.csv"))$x
Itc.short[,6]=a[1:length(id.ARP)]-a[-(1:length(id.ARP))]
a=read.csv(here("Case_Studies/MC","Itc1_u_short_6.csv"))$x
Itc.short[,7]=a[1:length(id.ARP)]-a[-(1:length(id.ARP))]
a=read.csv(here("Case_Studies/MC","Itc1_u_short_7.csv"))$x
Itc.short[,8]=a[1:length(id.ARP)]-a[-(1:length(id.ARP))]
a=read.csv(here("Case_Studies/MC","Itc1_u_short_8.csv"))$x
Itc.short[,9]=a[1:length(id.ARP)]-a[-(1:length(id.ARP))]
a=read.csv(here("Case_Studies/MC","Itc1_u_short_9.csv"))$x
Itc.short[,10]=a[1:length(id.ARP)]-a[-(1:length(id.ARP))]

# Panel S14(e)
dataFu=data.frame(lon=rep(Dat.loc[id.ARP,1],times=3),lat=rep(Dat.loc[id.ARP,2],times=3),
                 Itc.mean=c(apply(Itc.full,1,mean),apply(Itc.long,1,mean),apply(Itc.short,1,mean)),
                 Itc.sd=c(apply(Itc.full,1,sd),apply(Itc.long,1,sd),apply(Itc.short,1,sd)),
                 group=as.factor(rep(c("FSG","OSG-Long","OSG-Short"),each=1215)))
PT=ggplot() + facet_wrap(~ group, ncol = 3)+
  geom_raster(mapping=aes(lon, lat, fill = Itc.mean),data=dataFu)+
  scale_fill_gradient2(low = "#3288BD",mid="white",high = "#E41A1C",midpoint=0,limits=c(-0.1,0.1))+
  # geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_y_continuous(limits=c(12,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position = "right",
                   legend.title = element_text(),
                   legend.key.width=unit(0.5,"line"),
                   legend.key.height=unit(1,"line"))+
  labs(fill=expression(Mean(I[tc1])))+
  ylab("Latitude")+xlab("Longitude")
print(PT)  # 7.30*2.67

# Panel S14(f)
PT=ggplot() + facet_wrap(~ group, ncol = 3)+
  geom_raster(mapping=aes(lon, lat, fill = Itc.sd),data=dataFu)+
  scale_fill_gradient(low = "white",high ="#E41A1C",limits=c(0,0.005))+
  # geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_y_continuous(limits=c(12,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position = "right",
                   legend.title = element_text(),
                   legend.key.width=unit(0.5,"line"),
                   legend.key.height=unit(1,"line"))+
  labs(fill=expression(~~~~SD(I[tc1])))+
  ylab("Latitude")+xlab("Longitude")
print(PT)  # 7.30*2.67


###### Part 4. MC results for Iwdt
# FSG
Iwdt.full=matrix(0,1215,10)
Iwdt.full[,1]=read.csv(here("Case_Studies/FSG","Iwdt_u_full.csv"))$x
Iwdt.full[,2]=read.csv(here("Case_Studies/MC","Iwdt_u_full_1.csv"))$x
Iwdt.full[,3]=read.csv(here("Case_Studies/MC","Iwdt_u_full_2.csv"))$x
Iwdt.full[,4]=read.csv(here("Case_Studies/MC","Iwdt_u_full_3.csv"))$x
Iwdt.full[,5]=read.csv(here("Case_Studies/MC","Iwdt_u_full_4.csv"))$x
Iwdt.full[,6]=read.csv(here("Case_Studies/MC","Iwdt_u_full_5.csv"))$x
Iwdt.full[,7]=read.csv(here("Case_Studies/MC","Iwdt_u_full_6.csv"))$x
Iwdt.full[,8]=read.csv(here("Case_Studies/MC","Iwdt_u_full_7.csv"))$x
Iwdt.full[,9]=read.csv(here("Case_Studies/MC","Iwdt_u_full_8.csv"))$x
Iwdt.full[,10]=read.csv(here("Case_Studies/MC","Iwdt_u_full_9.csv"))$x
# OSG-Long
Iwdt.long=matrix(0,1215,10)
Iwdt.long[,1]=read.csv(here("Case_Studies/Scenario1_OSG_Long","Iwdt_u_online_1.csv"))$x
Iwdt.long[,2]=read.csv(here("Case_Studies/MC","Iwdt_u_long_1.csv"))$x
Iwdt.long[,3]=read.csv(here("Case_Studies/MC","Iwdt_u_long_2.csv"))$x
Iwdt.long[,4]=read.csv(here("Case_Studies/MC","Iwdt_u_long_3.csv"))$x
Iwdt.long[,5]=read.csv(here("Case_Studies/MC","Iwdt_u_long_4.csv"))$x
Iwdt.long[,6]=read.csv(here("Case_Studies/MC","Iwdt_u_long_5.csv"))$x
Iwdt.long[,7]=read.csv(here("Case_Studies/MC","Iwdt_u_long_6.csv"))$x
Iwdt.long[,8]=read.csv(here("Case_Studies/MC","Iwdt_u_long_7.csv"))$x
Iwdt.long[,9]=read.csv(here("Case_Studies/MC","Iwdt_u_long_8.csv"))$x
Iwdt.long[,10]=read.csv(here("Case_Studies/MC","Iwdt_u_long_9.csv"))$x
# OSG-Short
Iwdt.short=matrix(0,1215,10)
Iwdt.short[,1]=read.csv(here("Case_Studies/Scenario2_OSG_Short","Iwdt_u_online_2.csv"))$x
Iwdt.short[,2]=read.csv(here("Case_Studies/MC","Iwdt_u_short_1.csv"))$x
Iwdt.short[,3]=read.csv(here("Case_Studies/MC","Iwdt_u_short_2.csv"))$x
Iwdt.short[,4]=read.csv(here("Case_Studies/MC","Iwdt_u_short_3.csv"))$x
Iwdt.short[,5]=read.csv(here("Case_Studies/MC","Iwdt_u_short_4.csv"))$x
Iwdt.short[,6]=read.csv(here("Case_Studies/MC","Iwdt_u_short_5.csv"))$x
Iwdt.short[,7]=read.csv(here("Case_Studies/MC","Iwdt_u_short_6.csv"))$x
Iwdt.short[,8]=read.csv(here("Case_Studies/MC","Iwdt_u_short_7.csv"))$x
Iwdt.short[,9]=read.csv(here("Case_Studies/MC","Iwdt_u_short_8.csv"))$x
Iwdt.short[,10]=read.csv(here("Case_Studies/MC","Iwdt_u_short_9.csv"))$x

# Panel S14(g)
dataFu=data.frame(lon=rep(Dat.loc[id.ARP,1],times=3),lat=rep(Dat.loc[id.ARP,2],times=3),
                  Iwdt.mean=c(apply(Iwdt.full,1,mean),apply(Iwdt.long,1,mean),apply(Iwdt.short,1,mean)),
                  Iwdt.sd=c(apply(Iwdt.full,1,sd),apply(Iwdt.long,1,sd),apply(Iwdt.short,1,sd)),
                  group=as.factor(rep(c("FSG","OSG-Long","OSG-Short"),each=1215)))
PT=ggplot() + facet_wrap(~ group, ncol = 3)+
  geom_raster(mapping=aes(lon, lat, fill = Iwdt.mean),data=dataFu)+
  scale_fill_gradient(low = "white",high ="#E41A1C",limits=c(0,0.061))+
  # geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_y_continuous(limits=c(12,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position = "right",
                   legend.title = element_text(),
                   legend.key.width=unit(0.5,"line"),
                   legend.key.height=unit(1,"line"))+
  labs(fill=expression(Mean(I[wdt])))+
  ylab("Latitude")+xlab("Longitude")
print(PT)  # 7.30*2.67

# Panel S14(h)
PT=ggplot() + facet_wrap(~ group, ncol = 3)+
  geom_raster(mapping=aes(lon, lat, fill = Iwdt.sd),data=dataFu)+
  scale_fill_gradient(low = "white",high ="#E41A1C",limits=c(0,0.003))+
  # geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_y_continuous(limits=c(12,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position = "right",
                   legend.title = element_text(),
                   legend.key.width=unit(0.5,"line"),
                   legend.key.height=unit(1,"line"))+
  labs(fill=expression(~~~~SD(I[wdt])))+
  ylab("Latitude")+xlab("Longitude")
print(PT)  # 7.30*2.67



