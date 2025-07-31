################################################################################
# This file includes all steps to reproduce Figure 1                           #
################################################################################
# Necessary packages 
library(ggplot2)
library(maps)
library(moments)
cbPalette=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Necessary ARP information for ploting figures
world_map=map_data("world")                                                   # Extract the map data for the region
arabian_peninsula_countries=c("Saudi Arabia", "Yemen", 
                                 "Oman", "United Arab Emirates", 
                                 "Qatar", "Bahrain", "Kuwait")                # Define the countries of the Arabian Peninsula
arabian_peninsula=subset(world_map, region %in% arabian_peninsula_countries)  # Filter the map data for the Arabian Peninsula countries

# Specified time point and grid points for demonstration
t.choose=2920*8+(31+28+31+30+31+30+31+13)*8+1    # 25161, 00:00 am, August 14, 2022
loc.gsm=1122                                     # (45.5,15.5), South, Mountain 
loc.gep=750                                      # (57,21.0), East, Plain  
loc.gwc=671                                      # (39.5,21.5), West, Coastal  
loc.gcp=545                                      # (44.0,23.0), Center, Plateau 
loc.gnd=48                                       # (42.0,29.5), North, Desert 
loc.specified=c(loc.gsm,loc.gep,loc.gwc,loc.gcp,loc.gnd)



###### Part 1. Reproduce Figure 1(a)
# t1=proc.time()[[3]]
# Calculate ensemble mean for u- and v-components at specified time point t.choose
Windu.Enmean=apply(Windu.ARP[,,t.choose],2,mean)
Windv.Enmean=apply(Windv.ARP[,,t.choose],2,mean)
# Plot Figure 1(a)
dataF=data.frame(lon=rep(Dat.loc.arp[,1],times=2),lat=rep(Dat.loc.arp[,2],times=2),
                 WindSpeed=c(Windu.Enmean,Windv.Enmean),
                 group=as.factor(rep(c("U","V"),each=nrow(Dat.loc.arp))))
dataP=data.frame(lon=rep(Dat.loc.arp[c(loc.gsm,loc.gep,loc.gwc,loc.gcp,loc.gnd),1],times=2),
                 lat=rep(Dat.loc.arp[c(loc.gsm,loc.gep,loc.gwc,loc.gcp,loc.gnd),2],times=2),
                 group=as.factor(rep(c("U","V"),each=5)))
dataARP=data.frame(lon=rep(arabian_peninsula$long,times=2),lat=rep(arabian_peninsula$lat,times=2),
                   type=rep(arabian_peninsula$group,times=2),
                   group=as.factor(rep(c("U","V"),each=length(arabian_peninsula$long))))
PT.1=ggplot()+facet_wrap(~ group, ncol = 2)+
  geom_raster(mapping=aes(lon, lat, fill = WindSpeed),data=dataF)+
  geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  geom_point(mapping = aes(lon, lat),shape=4,data=dataP)+
  scale_fill_gradient2(low = "#3288BD",mid="white",high = "#E41A1C",midpoint=0,limits=c(-12,12))+
  scale_y_continuous(limits=c(12,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position =c(0.01,0.01),
                   legend.title = element_text(),
                   legend.key.width=unit(0.5,"line"),
                   legend.key.height=unit(0.7,"line"))+
  labs(fill=expression(ms^{-1}))+ylab("Latitude")+xlab("Longitude")
PT.1+annotate("text",x=46.5,y=14.5,label="GSM",size=2.5)+
  annotate("text",x=56,y=20,label="GEP",size=2.5)+
  annotate("text",x=40.5,y=20.5,label="GWC",size=2.5)+
  annotate("text",x=45,y=22,label="GCP",size=2.5)+
  annotate("text",x=43,y=28.5,label="GND",size=2.5)         # Save as 6.30*3.00 Winduv_EnMean.pdf
# t2=proc.time()[[3]]
# t2-t1=1.058



###### Part 2. Reproduce Figure 1(b)
# t1=proc.time()[[3]]
# Calculate ensemble standard deviation (sd) for u- and v-components at specified time point t.choose
Windu.Ensd=apply(Windu.ARP[,,t.choose],2,sd)
Windv.Ensd=apply(Windv.ARP[,,t.choose],2,sd)
# Plot Figure 1(b)
dataF=data.frame(lon=rep(Dat.loc.arp[,1],times=2),lat=rep(Dat.loc.arp[,2],times=2),
                 WindSpeed=c(Windu.Ensd,Windv.Ensd),
                 group=as.factor(rep(c("U","V"),each=nrow(Dat.loc.arp))))
PT.2=ggplot()+facet_wrap(~ group, ncol = 2)+
  geom_raster(mapping=aes(lon, lat, fill = WindSpeed),data=dataF)+
  geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  geom_point(mapping = aes(lon, lat),shape=4,data=dataP)+
  scale_fill_gradient2(low = "white",high = "#E41A1C",limits=c(0,max(dataF$WindSpeed)))+
  scale_y_continuous(limits=c(12,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position =c(0.01,0.01),
                   legend.title = element_text(),
                   legend.key.width=unit(0.5,"line"),
                   legend.key.height=unit(0.7,"line"))+
  labs(fill=expression(ms^{-1}))+ylab("Latitude")+xlab("Longitude")
PT.2+annotate("text",x=46.5,y=14.5,label="GSM",size=2.5)+
  annotate("text",x=56,y=20,label="GEP",size=2.5)+
  annotate("text",x=40.5,y=20.5,label="GWC",size=2.5)+
  annotate("text",x=45,y=22,label="GCP",size=2.5)+
  annotate("text",x=43,y=28.5,label="GND",size=2.5)        # Save to 6.30*3.00 Winduv_EnSD.pdf
# t2=proc.time()[[3]]
# t2-t1=1.217



###### Part 3. Reproduce Figure 1(c)
# t1=proc.time()[[3]]
# Calculate annual cycle on specifed grid points
R=dim(Windu.ARP)[1]
Windu.ARP.Daily=Windv.ARP.Daily=array(0,c(10,5,365*10))                             # Daily aggregate
for(r in 1:R){
  for(i in 1:5){
    Windu.ARP.Daily[r,i,]=colMeans(matrix(Windu.ARP[r,loc.specified[i],],8,3650))
    Windv.ARP.Daily[r,i,]=colMeans(matrix(Windv.ARP[r,loc.specified[i],],8,3650))
  }
}
Windu.ARP.Monthly=Windv.ARP.Monthly=array(0,c(10,5,12*10))                          # Monthly aggregate
for(t in 1:10){
  Windu.ARP.Monthly[,,1+(t-1)*12]=apply(Windu.ARP.Daily[,,1:31+(t-1)*365],c(1,2),mean)
  Windu.ARP.Monthly[,,2+(t-1)*12]=apply(Windu.ARP.Daily[,,32:59+(t-1)*365],c(1,2),mean)
  Windu.ARP.Monthly[,,3+(t-1)*12]=apply(Windu.ARP.Daily[,,60:90+(t-1)*365],c(1,2),mean)
  Windu.ARP.Monthly[,,4+(t-1)*12]=apply(Windu.ARP.Daily[,,91:120+(t-1)*365],c(1,2),mean)
  Windu.ARP.Monthly[,,5+(t-1)*12]=apply(Windu.ARP.Daily[,,121:151+(t-1)*365],c(1,2),mean)
  Windu.ARP.Monthly[,,6+(t-1)*12]=apply(Windu.ARP.Daily[,,152:181+(t-1)*365],c(1,2),mean)
  Windu.ARP.Monthly[,,7+(t-1)*12]=apply(Windu.ARP.Daily[,,182:212+(t-1)*365],c(1,2),mean)
  Windu.ARP.Monthly[,,8+(t-1)*12]=apply(Windu.ARP.Daily[,,213:243+(t-1)*365],c(1,2),mean)
  Windu.ARP.Monthly[,,9+(t-1)*12]=apply(Windu.ARP.Daily[,,244:273+(t-1)*365],c(1,2),mean)
  Windu.ARP.Monthly[,,10+(t-1)*12]=apply(Windu.ARP.Daily[,,274:304+(t-1)*365],c(1,2),mean)
  Windu.ARP.Monthly[,,11+(t-1)*12]=apply(Windu.ARP.Daily[,,305:334+(t-1)*365],c(1,2),mean)
  Windu.ARP.Monthly[,,12+(t-1)*12]=apply(Windu.ARP.Daily[,,335:365+(t-1)*365],c(1,2),mean)
  
  Windv.ARP.Monthly[,,1+(t-1)*12]=apply(Windv.ARP.Daily[,,1:31+(t-1)*365],c(1,2),mean)
  Windv.ARP.Monthly[,,2+(t-1)*12]=apply(Windv.ARP.Daily[,,32:59+(t-1)*365],c(1,2),mean)
  Windv.ARP.Monthly[,,3+(t-1)*12]=apply(Windv.ARP.Daily[,,60:90+(t-1)*365],c(1,2),mean)
  Windv.ARP.Monthly[,,4+(t-1)*12]=apply(Windv.ARP.Daily[,,91:120+(t-1)*365],c(1,2),mean)
  Windv.ARP.Monthly[,,5+(t-1)*12]=apply(Windv.ARP.Daily[,,121:151+(t-1)*365],c(1,2),mean)
  Windv.ARP.Monthly[,,6+(t-1)*12]=apply(Windv.ARP.Daily[,,152:181+(t-1)*365],c(1,2),mean)
  Windv.ARP.Monthly[,,7+(t-1)*12]=apply(Windv.ARP.Daily[,,182:212+(t-1)*365],c(1,2),mean)
  Windv.ARP.Monthly[,,8+(t-1)*12]=apply(Windv.ARP.Daily[,,213:243+(t-1)*365],c(1,2),mean)
  Windv.ARP.Monthly[,,9+(t-1)*12]=apply(Windv.ARP.Daily[,,244:273+(t-1)*365],c(1,2),mean)
  Windv.ARP.Monthly[,,10+(t-1)*12]=apply(Windv.ARP.Daily[,,274:304+(t-1)*365],c(1,2),mean)
  Windv.ARP.Monthly[,,11+(t-1)*12]=apply(Windv.ARP.Daily[,,305:334+(t-1)*365],c(1,2),mean)
  Windv.ARP.Monthly[,,12+(t-1)*12]=apply(Windv.ARP.Daily[,,335:365+(t-1)*365],c(1,2),mean)
}
Windu.ARP.Monthly.EnMean=apply(Windu.ARP.Monthly,c(2,3),mean)          
Windv.ARP.Monthly.EnMean=apply(Windv.ARP.Monthly,c(2,3),mean)
Windu.ARP.Monthly.EnTimeMean=Windv.ARP.Monthly.EnTimeMean=matrix(0,5,12)               # Annual aggregate
for(i in 1:12){
  Windu.ARP.Monthly.EnTimeMean[,i]=apply(Windu.ARP.Monthly.EnMean[,i+12*(0:9)],1,mean)
  Windv.ARP.Monthly.EnTimeMean[,i]=apply(Windv.ARP.Monthly.EnMean[,i+12*(0:9)],1,mean)
}
# Plot Figure 1(c)         
dataF=data.frame(Time=rep(1:12,times=5*2),
                 Temp=c(c(t(Windu.ARP.Monthly.EnTimeMean)),c(t(Windv.ARP.Monthly.EnTimeMean))),
                 Type=as.factor(rep(rep(c("GSM","GEP","GWC","GCP","GND"),each=12),times=2)),
                 group=as.factor(rep(c("U","V"),each=12*5)))
PT.3=ggplot()+facet_wrap(~ group, ncol = 2)+
  geom_line(aes(x=Time,y=Temp,color=Type),data=dataF)+
  geom_point(aes(x=Time,y=Temp,color=Type),data=dataF)+
  xlab("Month")+ylab(expression(Wind~speed~(ms^{-1})))+
  scale_x_continuous(breaks=1:12,labels=c("1", "2", "3", "4","5",
                                          "6","7","8","9","10","11","12"))+
  scale_colour_manual(values=cbPalette[c(1,2,4,6,8)])+
  geom_hline(yintercept = 0,linetype=3)+
  scale_y_continuous(limits = c(-3.4,3.4))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0.00,0.00),
                   legend.position =c(0.07,0.01),
                   #legend.position = "none",
                   legend.title = element_blank(),
                   legend.key.width=unit(1,"line"),
                   legend.key.height=unit(0.7,"line"))+
  guides(color = guide_legend(ncol = 2))
PT.3                       # Save to 6.30*3.00 Winduv_AnnualCycle.pdf
# t2=proc.time()[[3]]
# t2-t1=2.7



###### Part 4. Reproduce Figure 1(d)
# t1=proc.time()[[3]]
tseqw=2920*8+(31+28+31+30+31+30+31+12)*8+1:24        # Aug 13th-15th, 2022 
dataF=data.frame(Time=rep(tseqw,times=2*R),
                 Temp1=c(t(cbind(Windu.ARP[,loc.gsm,tseqw],Windv.ARP[,loc.gsm,tseqw]))),
                 Temp2=c(t(cbind(Windu.ARP[,loc.gep,tseqw],Windv.ARP[,loc.gep,tseqw]))),
                 Temp3=c(t(cbind(Windu.ARP[,loc.gwc,tseqw],Windv.ARP[,loc.gwc,tseqw]))),
                 Temp4=c(t(cbind(Windu.ARP[,loc.gcp,tseqw],Windv.ARP[,loc.gcp,tseqw]))),
                 Temp5=c(t(cbind(Windu.ARP[,loc.gnd,tseqw],Windv.ARP[,loc.gnd,tseqw]))),
                 LEN=as.factor(rep(c("1","2","3","4","5","6","7","8","9","10"),each=2*length(tseqw))),
                 group=as.factor(rep(rep(c("U","V"),each=length(tseqw)),times=R)))
dataL=data.frame(Time=rep(tseqw,times=2*5),
                 Temp=c(apply(cbind(Windu.ARP[,loc.gsm,tseqw],Windv.ARP[,loc.gsm,tseqw]),2,mean),
                        apply(cbind(Windu.ARP[,loc.gep,tseqw],Windv.ARP[,loc.gep,tseqw]),2,mean),
                        apply(cbind(Windu.ARP[,loc.gwc,tseqw],Windv.ARP[,loc.gwc,tseqw]),2,mean),
                        apply(cbind(Windu.ARP[,loc.gcp,tseqw],Windv.ARP[,loc.gcp,tseqw]),2,mean),
                        apply(cbind(Windu.ARP[,loc.gnd,tseqw],Windv.ARP[,loc.gnd,tseqw]),2,mean)),
                 Type=as.factor(rep(c("GSM","GEP","GWC","GCP","GND"),each=2*length(tseqw))),
                 group=as.factor(rep(rep(c("U","V"),each=length(tseqw)),times=5)))
PT.4=ggplot()+facet_wrap(~ group, ncol = 2)+
  geom_line(aes(x=Time,y=Temp1,linetype=LEN),colour=cbPalette[6],alpha=0.2,data=dataF)+
  geom_line(aes(x=Time,y=Temp2,linetype=LEN),colour=cbPalette[2],alpha=0.2,data=dataF)+
  geom_line(aes(x=Time,y=Temp3,linetype=LEN),colour=cbPalette[8],alpha=0.2,data=dataF)+
  geom_line(aes(x=Time,y=Temp4,linetype=LEN),colour=cbPalette[1],alpha=0.2,data=dataF)+
  geom_line(aes(x=Time,y=Temp5,linetype=LEN),colour=cbPalette[4],alpha=0.2,data=dataF)+
  xlab("Aug, 2022")+ylab(expression(Wind~speed~(ms^{-1})))+
  scale_x_continuous(breaks=c(tseqw[1]+3.5,tseqw[1]+11.5,tseqw[1]+19.5),
                     labels=c("13th", "14th", "15th"))+
  scale_linetype_manual(values=rep(1,10),guide="none")+
  geom_line(aes(x=Time,y=Temp,colour=Type),size=0.3,data=dataL)+
  scale_colour_manual(values=cbPalette[c(1,2,4,6,8)])+
  scale_y_continuous(limits = c(-8.5,8.5))+
  geom_hline(yintercept = 0,linetype=3)+
  geom_vline(xintercept = c(tseqw[1]+7,tseqw[1]+15,tseqw[1]+23),linetype=3)+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   # legend.position =c(0.51,0.7),
                   legend.position = "none",
                   legend.title = element_blank(),
                   legend.key.width=unit(0.8,"line"),
                   legend.key.height=unit(0.6,"line"))+
  guides(color = guide_legend(ncol = 2))
PT.4                      # 6.30*3.00 Winduv_TimeSeries.pdf
# t2=proc.time()[[3]]
# t2-t1=0.875



###### Part 5. Reproduce Figures 1(e) and 1(f)
# t1=proc.time()[[3]]
# Calculate skewness and kurtosis for detrend data on each grid point
Windu.EnMean=apply(Windu.ARP,c(2,3),mean)              
Windv.EnMean=apply(Windv.ARP,c(2,3),mean)
Skewfunc=function(i){return(skewness(c(Windu.ARP[,i,]-rep(1,R)%*%t(Windu.EnMean[i,]))))}
Kurtfunc=function(i){return(kurtosis(c(Windu.ARP[,i,]-rep(1,R)%*%t(Windu.EnMean[i,]))))}
SkewU=sapply(1:nrow(Dat.loc.arp),Skewfunc)
KurtU=sapply(1:nrow(Dat.loc.arp),Kurtfunc)
Skewfunc=function(i){return(skewness(c(Windv.ARP[,i,]-rep(1,R)%*%t(Windv.EnMean[i,]))))}
Kurtfunc=function(i){return(kurtosis(c(Windv.ARP[,i,]-rep(1,R)%*%t(Windv.EnMean[i,]))))}
SkewV=sapply(1:nrow(Dat.loc.arp),Skewfunc)
KurtV=sapply(1:nrow(Dat.loc.arp),Kurtfunc)
# Plot Figure 1(e)
dataF=data.frame(lon=rep(Dat.loc.arp[,1],times=2),lat=rep(Dat.loc.arp[,2],times=2),
                 WindSpeed=c(SkewU,SkewV),
                 group=as.factor(rep(c("U","V"),each=nrow(Dat.loc.arp))))
PT.5=ggplot()+facet_wrap(~ group, ncol = 2)+
  geom_raster(mapping=aes(lon, lat, fill = WindSpeed),data=dataF)+
  geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  geom_point(mapping = aes(lon, lat),shape=4,data=dataP)+
  scale_fill_gradient2(low = "#3288BD",mid="white",high = "#E41A1C",midpoint=0,limits=c(-0.65,0.65))+
  scale_y_continuous(limits=c(12,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position =c(0.01,0.01),
                   legend.title = element_text(),
                   legend.key.width=unit(0.5,"line"),
                   legend.key.height=unit(0.7,"line"))+
  labs(fill=" ")+ylab("Latitude")+xlab("Longitude")
PT.5+annotate("text",x=46.5,y=14.5,label="GSM",size=2.5)+
  annotate("text",x=56,y=20,label="GEP",size=2.5)+
  annotate("text",x=40.5,y=20.5,label="GWC",size=2.5)+
  annotate("text",x=45,y=22,label="GCP",size=2.5)+
  annotate("text",x=43,y=28.5,label="GND",size=2.5)        # Save to 6.30*3.00 Winduv_Skew.pdf
# Plot Figure 1(f)
dataF=data.frame(lon=rep(Dat.loc.arp[,1],times=2),lat=rep(Dat.loc.arp[,2],times=2),
                 WindSpeed=c(KurtU,KurtV)-3,
                 group=as.factor(rep(c("U","V"),each=nrow(Dat.loc.arp))))
PT.6=ggplot()+facet_wrap(~ group, ncol = 2)+
  geom_raster(mapping=aes(lon, lat, fill = WindSpeed),data=dataF)+
  geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  geom_point(mapping = aes(lon, lat),shape=4,data=dataP)+
  scale_fill_gradient2(low = "white",high = "#E41A1C",limits=c(0,20))+
  scale_y_continuous(limits=c(12,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   legend.justification = c(0,0),
                   legend.position =c(0.01,0.01),
                   legend.title = element_text(),
                   legend.key.width=unit(0.5,"line"),
                   legend.key.height=unit(0.7,"line"))+
  labs(fill=" ")+ylab("Latitude")+xlab("Longitude")
PT.6+annotate("text",x=46.5,y=14.5,label="GSM",size=2.5)+
  annotate("text",x=56,y=20,label="GEP",size=2.5)+
  annotate("text",x=40.5,y=20.5,label="GWC",size=2.5)+
  annotate("text",x=45,y=22,label="GCP",size=2.5)+
  annotate("text",x=43,y=28.5,label="GND",size=2.5)     # Save to 6.30*3.00 Winduv_Kurt.pdf
# t2=proc.time()[[3]]
# t2-t1=1583.655 seconds (26.39425 minutes)







