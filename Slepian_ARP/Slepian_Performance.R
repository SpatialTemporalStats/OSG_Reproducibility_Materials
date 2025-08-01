################################################################################
# This file includes all steps to reproduce Figure 3                           #
################################################################################
# ! Note 1. Before running the code, please download the required Slepian bases and their eigenvalues.
#           First, please download "Slepian_ARP.zip" from https://zenodo.org/records/16655516.
#           Then, please extract the contents of "Slepian_ARP.zip" and save all .m files in the sub-repository "Slepian_ARP".
# ! Note 2. On the same website, we provide Matlab code in "Slepian_Code.zip" for
#           generating these Slepian bases or adapting the approach for related applications.

# Necessary packages
library(R.matlab)
library(ggplot2)
library(maps)

# Necessary ARP information for ploting figures
world_map=map_data("world")                                                   # Extract the map data for the region
arabian_peninsula_countries=c("Saudi Arabia", "Yemen", 
                              "Oman", "United Arab Emirates", 
                              "Qatar", "Bahrain", "Kuwait")                # Define the countries of the Arabian Peninsula
arabian_peninsula=subset(world_map, region %in% arabian_peninsula_countries)  # Filter the map data for the Arabian Peninsula countries
dataARP=data.frame(lon=rep(arabian_peninsula$long,times=2),lat=rep(arabian_peninsula$lat,times=2),
                   type=rep(arabian_peninsula$group,times=2),
                   group=as.factor(rep(c("U","V"),each=length(arabian_peninsula$long))))



###### Part 1. Randomly select 1000 and 500 time points for testing performance of bases
######         and computing empirical orthogonal functions (EOFs), respectively 
# source(here("Data","Data_Treatment.R"))
# Obtain testing dataset
# t1=proc.time()[[3]]
set.seed(100)
t.choose=sample(1:(365*8*10),1000,replace = FALSE)          
Windu.ARP1=Windu.ARP[,,t.choose]                            
Windv.ARP1=Windv.ARP[,,t.choose]           
Windu.EnMean1=apply(Windu.ARP1,c(2,3),mean)               
Windv.EnMean1=apply(Windv.ARP1,c(2,3),mean)
Windu.rsd1=Windv.rsd1=array(0,c(dim(Windu.ARP1)))
for(r in 1:R){
  Windu.rsd1[r,,]=Windu.ARP1[r,,]-Windu.EnMean1
  Windv.rsd1[r,,]=Windv.ARP1[r,,]-Windv.EnMean1
}
# Obtain data for calculating the EOFs
set.seed(200)
t.choose2=sample(1:(365*8*10)[-t.choose],500,replace = FALSE)
Windu.ARP2=Windu.ARP[,,t.choose2]
Windv.ARP2=Windv.ARP[,,t.choose2]
Windu.EnMean2=apply(Windu.ARP2,c(2,3),mean)
Windv.EnMean2=apply(Windv.ARP2,c(2,3),mean)
Windu.rsd2=Windv.rsd2=array(0,c(dim(Windu.ARP2)))
for(r in 1:R){
  Windu.rsd2[r,,]=Windu.ARP2[r,,]-Windu.EnMean2
  Windv.rsd2[r,,]=Windv.ARP2[r,,]-Windv.EnMean2
}
# t2=proc.time()[[3]]
# t2-t1=70.019 



###### Part 2. Reproduce Figure 3(a)
# t1=proc.time()[[3]]
# Load Slepian bases with Q=96
Eig.arp=Re(readMat(here("Slepian_ARP","Basis_reg_eig_value_96_real.mat"))$Basis.reg.eig.value)
Rerank.id=rank(-Eig.arp,ties.method = "first")
Eig.arp.rerank=rep(0,length(Eig.arp))
for(i in 1:length(Eig.arp)){
  id=which(Rerank.id==i)
  Eig.arp.rerank[i]=Eig.arp[id]
}
Q.sl=length(which(Re(Eig.arp)>=0.01))      # Q.sl represents the "A" in the manuscript
Basis.SLP.pre=readMat(here("Slepian_ARP","Slepian_spatial_96.mat"))$Slepian.spatial
Basis.SLP=matrix(0,nrow(Dat.loc),length(Eig.arp))
for(i in 1:length(Eig.arp)){
  id=which(Rerank.id==i)
  Basis.SLP[,i]=Basis.SLP.pre[,id]
}
Basis.SLP.pre=0
# Translate the data from spatial domain to Slepian domain with Q=96
A=solve(t(Basis.SLP[id.ARP,1:Q.sl])%*%Basis.SLP[id.ARP,1:Q.sl],t(Basis.SLP[id.ARP,1:Q.sl]))
Windu.SLP1=Windv.SLP1=array(0,c(R,Q.sl,length(t.choose)))
for(r in 1:R){
  Windu.SLP1[r,,]=A%*%Windu.rsd1[r,,]
  Windv.SLP1[r,,]=A%*%Windv.rsd1[r,,]
}
# Examine the performance using residuals
resv2.u=resv2.v=matrix(0,length(id.ARP),R)
for(r in 1:R){
  resv2.u[,r]=apply((Basis.SLP[id.ARP,1:Q.sl]%*%Windu.SLP1[r,,]-Windu.rsd1[r,,])^2,1,mean)
  resv2.v[,r]=apply((Basis.SLP[id.ARP,1:Q.sl]%*%Windv.SLP1[r,,]-Windv.rsd1[r,,])^2,1,mean)
}
v2hat.u=apply(resv2.u,1,mean)  # range(sqrt(v2hat.u)): 0.08234261 0.29738929   # mean(sqrt(v2hat.u)): 0.1910188
v2hat.v=apply(resv2.v,1,mean)  # range(sqrt(v2hat.v)): 0.09237752 0.31799793   # mean(sqrt(v2hat.v)): 0.1984032
# Plot Figure 3(a)
dataF=data.frame(lon=rep(Dat.loc.arp[,1],times=2),lat=rep(Dat.loc.arp[,2],times=2),
                 RMSE=c(sqrt(v2hat.u),sqrt(v2hat.v)),
                 group=as.factor(rep(c("U","V"),each=nrow(Dat.loc.arp))))
PT=ggplot()+facet_wrap(~ group, ncol = 2)+
  geom_raster(mapping=aes(lon, lat, fill = RMSE),data=dataF)+
  geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_fill_gradient2(low = "#3288BD",high = "#E41A1C",limits=c(0,0.32))+
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
print(PT)          # Save to 6.30*3.00 Slepian_Perform_A100.pdf
# t2=proc.time()[[3]]
# t2-t1=3.774 



###### Part 3. Reproduce Figure 3(b)
# t1=proc.time()[[3]]
# Load Slepian bases with Q=181
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
# Translate the data from spatial domain to Slepian domain with Q=181
A=solve(t(Basis.SLP[id.ARP,1:Q.sl])%*%Basis.SLP[id.ARP,1:Q.sl],t(Basis.SLP[id.ARP,1:Q.sl]))
Windu.SLP1=Windv.SLP1=array(0,c(R,Q.sl,length(t.choose)))
for(r in 1:R){
  Windu.SLP1[r,,]=A%*%Windu.rsd1[r,,]
  Windv.SLP1[r,,]=A%*%Windv.rsd1[r,,]
}
# Examine the performance using residuals
resv2.u=resv2.v=matrix(0,length(id.ARP),R)
for(r in 1:R){
  resv2.u[,r]=apply((Basis.SLP[id.ARP,1:Q.sl]%*%Windu.SLP1[r,,]-Windu.rsd1[r,,])^2,1,mean)
  resv2.v[,r]=apply((Basis.SLP[id.ARP,1:Q.sl]%*%Windv.SLP1[r,,]-Windv.rsd1[r,,])^2,1,mean)
}
v2hat.u=apply(resv2.u,1,mean)  # range(sqrt(v2hat.u)): 0.03388787 0.13814059    # mean(sqrt(v2hat.u)): 0.06842565
v2hat.v=apply(resv2.v,1,mean)  # range(sqrt(v2hat.v)): 0.0356744 0.1392944      # mean(sqrt(v2hat.v)): 0.06854974
# Plot Figure 3(b)
dataF=data.frame(lon=rep(Dat.loc.arp[,1],times=2),lat=rep(Dat.loc.arp[,2],times=2),
                 RMSE=c(sqrt(v2hat.u),sqrt(v2hat.v)),
                 group=as.factor(rep(c("U","V"),each=nrow(Dat.loc.arp))))
PT=ggplot()+facet_wrap(~ group, ncol = 2)+
  geom_raster(mapping=aes(lon, lat, fill = RMSE),data=dataF)+
  geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_fill_gradient2(low = "#3288BD",high = "#E41A1C",limits=c(0,0.32))+
  scale_y_continuous(limits=c(12,30))+
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
  labs(fill=expression(ms^{-1}))+ylab("Latitude")+xlab("Longitude")
print(PT)          # Save to 6.30*3.00 Slepian_Perform_A300.pdf
# t2=proc.time()[[3]]
# t2-t1=3.663 



###### Part 4. Reproduce Figure 3(c)
# t1=proc.time()[[3]]
# Load Slepian bases with Q=213
Eig.arp=Re(readMat(here("Slepian_ARP","Basis_reg_eig_value_213_real.mat"))$Basis.reg.eig.value)
Rerank.id=rank(-Eig.arp,ties.method = "first")
Eig.arp.rerank=rep(0,length(Eig.arp))
for(i in 1:length(Eig.arp)){
  id=which(Rerank.id==i)
  Eig.arp.rerank[i]=Eig.arp[id]
}
Q.sl=length(which(Re(Eig.arp)>=0.01)) 
Basis.SLP.pre=readMat(here("Slepian_ARP","Slepian_spatial_213.mat"))$Slepian.spatial
Basis.SLP=matrix(0,nrow(Dat.loc),length(Eig.arp))
for(i in 1:length(Eig.arp)){
  id=which(Rerank.id==i)
  Basis.SLP[,i]=Basis.SLP.pre[,id]
}
Basis.SLP.pre=0
# Translate the data from spatial domain to Slepian domain with Q=213
A=solve(t(Basis.SLP[id.ARP,1:Q.sl])%*%Basis.SLP[id.ARP,1:Q.sl],t(Basis.SLP[id.ARP,1:Q.sl]))
Windu.SLP1=Windv.SLP1=array(0,c(R,Q.sl,length(t.choose)))
for(r in 1:R){
  Windu.SLP1[r,,]=A%*%Windu.rsd1[r,,]
  Windv.SLP1[r,,]=A%*%Windv.rsd1[r,,]
}
# Examine the performance using residuals
resv2.u=resv2.v=matrix(0,length(id.ARP),R)
for(r in 1:R){
  resv2.u[,r]=apply((Basis.SLP[id.ARP,1:Q.sl]%*%Windu.SLP1[r,,]-Windu.rsd1[r,,])^2,1,mean)
  resv2.v[,r]=apply((Basis.SLP[id.ARP,1:Q.sl]%*%Windv.SLP1[r,,]-Windv.rsd1[r,,])^2,1,mean)
}
v2hat.u=apply(resv2.u,1,mean)  # range(sqrt(v2hat.u)): 0.02348988 0.09714367    # mean(sqrt(v2hat.u)): 0.04751215
v2hat.v=apply(resv2.v,1,mean)  # range(sqrt(v2hat.v)): 0.02208108 0.09687152    # mean(sqrt(v2hat.v)): 0.04753363
# Plot Figure 3(c)
dataF=data.frame(lon=rep(Dat.loc.arp[,1],times=2),lat=rep(Dat.loc.arp[,2],times=2),
                 RMSE=c(sqrt(v2hat.u),sqrt(v2hat.v)),
                 group=as.factor(rep(c("U","V"),each=nrow(Dat.loc.arp))))
PT=ggplot()+facet_wrap(~ group, ncol = 2)+
  geom_raster(mapping=aes(lon, lat, fill = RMSE),data=dataF)+
  geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_fill_gradient2(low = "#3288BD",high = "#E41A1C",limits=c(0,0.32))+
  scale_y_continuous(limits=c(12,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   #legend.justification = c(0,0),
                   legend.position ="none",
                   legend.title = element_text(),
                   legend.key.width=unit(0.5,"line"),
                   legend.key.height=unit(0.7,"line"))+
  labs(fill=expression(ms^{-1}))+ylab("Latitude")+xlab("Longitude")
print(PT)          # Save to 6.30*3.00 Slepian_Perform_A400.pdf
# t2=proc.time()[[3]]
# t2-t1=4.347 



###### Part 5. Reproduce Figure 3(d)
# t1=proc.time()[[3]]
# Compute bivariate EOFs for u- and v-components
Windu.rsd.rtmean=Windv.rsd.rtmean=rep(0,length(id.ARP))    # Detrend across t and r
for(r in 1:R){
  for(t in 1:length(t.choose2)){
    Windu.rsd.rtmean=Windu.rsd.rtmean+Windu.rsd2[r,,t]
    Windv.rsd.rtmean=Windv.rsd.rtmean+Windv.rsd2[r,,t]
  }
}
Windu.rsd.rtmean=Windu.rsd.rtmean/R/length(t.choose2)
Windv.rsd.rtmean=Windv.rsd.rtmean/R/length(t.choose2)
K.sample.uv=matrix(0,2*length(id.ARP),2*length(id.ARP))                      # Calculate the covariance matrix
for(r in 1:R){
  for(t in 1:length(t.choose2)){
    K.sample.uv=K.sample.uv+crossprod(t(c(Windu.rsd2[r,,t],Windv.rsd2[r,,t])-c(Windu.rsd.rtmean,Windv.rsd.rtmean)))
  }
}
K.sample.uv=K.sample.uv/R/length(t.choose2)
K.sample.uv.svd=svd(K.sample.uv)                                             # Find the first 2*300 EOFs
EOFs.uv=K.sample.uv.svd$u
Q.EOFs=2*300
# sum(K.sample.uv.svd$d[1:Q.EOFs])/sum(K.sample.uv.svd$d)                    # The first 300 EOFs would take 98.94918% variance
Auv=solve(t(EOFs.uv[,1:Q.EOFs])%*%EOFs.uv[,1:Q.EOFs],t(EOFs.uv[,1:Q.EOFs]))  # Calculate the principle components 
Winduv.EOFs=array(0,c(R,Q.EOFs,length(t.choose)))
for(r in 1:R){
  Winduv.EOFs[r,,]=Auv%*%rbind(Windu.rsd1[r,,],Windv.rsd1[r,,])
}
# Examine the performance using residuals
resv2.uv=matrix(0,2*length(id.ARP),R)                                        
for(r in 1:R){
  resv2.uv[,r]=apply((EOFs.uv[,1:Q.EOFs]%*%Winduv.EOFs[r,,]-rbind(Windu.rsd1[r,,],Windv.rsd1[r,,]))^2,1,mean)
}
v2hat.EOFs.uv=apply(resv2.uv,1,mean)         # range(sqrt(v2hat.EOFs.uv[1:length(id.ARP)]))     # 0.03937198 0.10238104   # mean(sqrt(v2hat.EOFs.uv[1:length(id.ARP)]))=0.05816222
                                             # range(sqrt(v2hat.EOFs.uv[-(1:length(id.ARP))]))  # 0.03858439 0.09236448   # mean(sqrt(v2hat.EOFs.uv[-(1:length(id.ARP))]))=0.05830579
# Plot Figure 3(d)
dataF=data.frame(lon=rep(Dat.loc.arp[,1],times=2),lat=rep(Dat.loc.arp[,2],times=2),
                 # RMSE=c(EOFs.uv[1:length(id.ARP),1],EOFs.uv[-(1:length(id.ARP)),1]), # used to test the shape of EOFs
                 RMSE=c(sqrt(v2hat.EOFs.uv[1:length(id.ARP)]),sqrt(v2hat.EOFs.uv[-(1:length(id.ARP))])),
                 group=as.factor(rep(c("U","V"),each=nrow(Dat.loc.arp))))
PT=ggplot()+facet_wrap(~ group, ncol = 2)+
  geom_raster(mapping=aes(lon, lat, fill = RMSE),data=dataF)+
  geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_fill_gradient2(low = "#3288BD",high = "#E41A1C",limits=c(0,0.32))+
  scale_y_continuous(limits=c(12,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=12),
                   #legend.justification = c(0,0),
                   legend.position ="none",
                   legend.title = element_text(),
                   legend.key.width=unit(0.5,"line"),
                   legend.key.height=unit(0.7,"line"))+
  labs(fill=expression(ms^{-1}))+ylab("Latitude")+xlab("Longitude")
print(PT)          # Save to 6.30*3.00 EOFs_Perform_A300_1.pdf
# t2=proc.time()[[3]]
# t2-t1=214.911 




