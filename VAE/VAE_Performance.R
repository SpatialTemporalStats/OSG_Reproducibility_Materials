################################################################################
# This file includes all steps to reproduce Figure S12                         #
################################################################################
#! Note. Before running the code, please run the file "Wind_VAE_Perform_Update.py" using "vae_support.sbatch".
#        Then, you will get file "test_recon_loss_1m.npy". Similarly, you can get "test_recon_loss_1y.npy" and "test_recon_loss_9y.npy".


# library(reticulate)
np=import("numpy")
# library(ggplot2)
# library(maps)
# source(here("Data","Data_Treatment.R"))


# Necessary ARP information for ploting figures
world_map=map_data("world")                                                   # Extract the map data for the region
arabian_peninsula_countries=c("Saudi Arabia", "Yemen", 
                              "Oman", "United Arab Emirates", 
                              "Qatar", "Bahrain", "Kuwait")                # Define the countries of the Arabian Peninsula
arabian_peninsula=subset(world_map, region %in% arabian_peninsula_countries)  # Filter the map data for the Arabian Peninsula countries
dataARP=data.frame(lon=rep(arabian_peninsula$long,times=2),lat=rep(arabian_peninsula$lat,times=2),
                   type=rep(arabian_peninsula$group,times=2),
                   group=as.factor(rep(c("U","V"),each=length(arabian_peninsula$long))))



###### Part 1. Load the performance of VAE trained by data from the first month
x=np$load(here("VAE","test_recon_loss_1m.npy"))
xu=c(t(x[,,1]))[id.ARP]
xv=c(t(x[,,2]))[id.ARP]
dataF=data.frame(lon=rep(Dat.loc.arp[,1],times=2),lat=rep(Dat.loc.arp[,2],times=2),
                 RMSE=c(sqrt(xu),sqrt(xv)),
                 group=as.factor(rep(c("U","V"),each=nrow(Dat.loc.arp))))
PT=ggplot()+facet_wrap(~ group, ncol = 2)+
  geom_raster(mapping=aes(lon, lat, fill = RMSE),data=dataF)+
  geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_fill_gradient2(low = "white",high = "#E41A1C",limits=c(0,7.6))+
  scale_y_continuous(limits=c(12,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   #legend.justification = c(0,0),
                   legend.position ="right",
                   legend.title = element_text(),
                   legend.key.width=unit(0.5,"line"),
                   legend.key.height=unit(0.7,"line"))+
  labs(fill=expression(ms^{-1}))+ylab("Latitude")+xlab("Longitude")
print(PT)          # Save to 6.30*3.00 VAE_Perform_1m.pdf




###### Part 2. Load the performance of VAE trained by data from the first year
x=np$load(here("VAE","test_recon_loss_1y.npy"))
xu=c(t(x[,,1]))[id.ARP]
xv=c(t(x[,,2]))[id.ARP]
dataF=data.frame(lon=rep(Dat.loc.arp[,1],times=2),lat=rep(Dat.loc.arp[,2],times=2),
                 RMSE=c(sqrt(xu),sqrt(xv)),
                 group=as.factor(rep(c("U","V"),each=nrow(Dat.loc.arp))))
PT=ggplot()+facet_wrap(~ group, ncol = 2)+
  geom_raster(mapping=aes(lon, lat, fill = RMSE),data=dataF)+
  geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_fill_gradient2(low = "white",high = "#E41A1C",limits=c(0,1.7))+
  scale_y_continuous(limits=c(12,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   #legend.justification = c(0,0),
                   legend.position ="right",
                   legend.title = element_text(),
                   legend.key.width=unit(0.5,"line"),
                   legend.key.height=unit(0.7,"line"))+
  labs(fill=expression(ms^{-1}))+ylab("Latitude")+xlab("Longitude")
print(PT)          # Save to 6.30*3.00 VAE_Perform_1y.pdf




###### Part 3. Load the performance of VAE trained by data from the first nine years
x=np$load(here("VAE","test_recon_loss_9y.npy"))
xu=c(t(x[,,1]))[id.ARP]
xv=c(t(x[,,2]))[id.ARP]
dataF=data.frame(lon=rep(Dat.loc.arp[,1],times=2),lat=rep(Dat.loc.arp[,2],times=2),
                 RMSE=c(sqrt(xu),sqrt(xv)),
                 group=as.factor(rep(c("U","V"),each=nrow(Dat.loc.arp))))
PT=ggplot()+facet_wrap(~ group, ncol = 2)+
  geom_raster(mapping=aes(lon, lat, fill = RMSE),data=dataF)+
  geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_fill_gradient2(low = "white",high = "#E41A1C",limits=c(0,1.7))+
  scale_y_continuous(limits=c(12,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   #legend.justification = c(0,0),
                   legend.position ="right",
                   legend.title = element_text(),
                   legend.key.width=unit(0.5,"line"),
                   legend.key.height=unit(0.7,"line"))+
  labs(fill=expression(ms^{-1}))+ylab("Latitude")+xlab("Longitude")
print(PT)          # Save to 6.30*3.00 VAE_Perform_9y.pdf


