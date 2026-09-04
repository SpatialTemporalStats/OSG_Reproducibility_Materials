################################################################################
# This file includes all steps to reproduce Figure S3                          #
################################################################################
# ! Note 1. Before running the code, please download the required Slepian bases and their eigenvalues.
#           First, please download "Slepian_ARP.zip" from https://zenodo.org/records/16655516.
#           Then, please extract the contents of "Slepian_ARP.zip" and save all .m files in the sub-repository "Slepian_ARP".
# ! Note 2. On the same website, we provide Matlab code in "Slepian_Code.zip" for
#           generating these Slepian bases or adapting the approach for related applications.
#           The code applies the method proposed by [Bates et al. (2017)](https://dl.acm.org/doi/abs/10.1109/TSP.2017.2712122) to ARP.

# Necessary packages and step
# library(R.matlab)
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


# Obtain testing dataset
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


###### Part 1. Randomly select 1000 time points for testing performance of Slepian bases 
######         with the same Q and \lambda_A=0.1, 0.01, 0.001, 0.0001, 0.00001, 0.000001
# Load Slepian bases with Q=181
Eig.arp=Re(readMat(here("Slepian_ARP","Basis_reg_eig_value_181_real.mat"))$Basis.reg.eig.value)
Rerank.id=rank(-Eig.arp,ties.method = "first")
Eig.arp.rerank=rep(0,length(Eig.arp))
for(i in 1:length(Eig.arp)){
  id=which(Rerank.id==i)
  Eig.arp.rerank[i]=Eig.arp[id]
}
lamb.A=c(0.1,0.01,0.001,0.0001,0.00001,0.000001)
VHatU=VHatV=matrix(0,1215,length(lamb.A))
for(j in 1:length(lamb.A)){
  Q.sl=length(which(Re(Eig.arp)>=lamb.A[j]))      # Q.sl represents the "A" in the manuscript
  Basis.SLP.pre=readMat(here("Slepian_ARP","Slepian_spatial_181.mat"))$Slepian.spatial
  Basis.SLP=matrix(0,nrow(Dat.loc),length(Eig.arp))
  for(i in 1:length(Eig.arp)){
    id=which(Rerank.id==i)
    Basis.SLP[,i]=Basis.SLP.pre[,id]
  }
  Basis.SLP.pre=0
  # Translate the data from spatial domain to Slepian domain with Q.sl Slepian bases
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
  VHatU[,j]=apply(resv2.u,1,mean)  
  VHatV[,j]=apply(resv2.v,1,mean)  
}

# Plot Figure S3(a)/(b)/(c)/(d)/(e)
dataF=data.frame(lon=rep(Dat.loc.arp[,1],times=2),lat=rep(Dat.loc.arp[,2],times=2),
                 RMSE=c(sqrt(VHatU[,6])-sqrt(VHatU[,2]),sqrt(VHatV[,6])-sqrt(VHatV[,2])),
                 group=as.factor(rep(c("U","V"),each=nrow(Dat.loc.arp))))
PT=ggplot()+facet_wrap(~ group, ncol = 2)+
  geom_raster(mapping=aes(lon, lat, fill = RMSE),data=dataF)+
  geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_fill_gradient2(low = "#3288BD",high = "#E41A1C", mid=0,limits=c(-0.08,0.08))+
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
print(PT)  



###### Part 2. Randomly select 1000 time points for testing performance of Slepian bases 
######         with the combinations of Q and \lambda_A such that there are A\approx 300
Eig.arp=Re(readMat(here("Slepian_ARP","Basis_reg_eig_value_144_real.mat"))$Basis.reg.eig.value)
Rerank.id=rank(-Eig.arp,ties.method = "first")
Eig.arp.rerank=rep(0,length(Eig.arp))
for(i in 1:length(Eig.arp)){
  id=which(Rerank.id==i)
  Eig.arp.rerank[i]=Eig.arp[id]
}
Q.sl=length(which(Re(Eig.arp)>=0.000001)) 
Basis.SLP.pre=readMat(here("Slepian_ARP","Slepian_spatial_144.mat"))$Slepian.spatial
Basis.SLP=matrix(0,nrow(Dat.loc),length(Eig.arp))
for(i in 1:length(Eig.arp)){
  id=which(Rerank.id==i)
  Basis.SLP[,i]=Basis.SLP.pre[,id]
}
Basis.SLP.pre=0
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
v2hat.v=apply(resv2.v,1,mean)

# Plot Figure S3(f)
dataF=data.frame(lon=rep(Dat.loc.arp[,1],times=2),lat=rep(Dat.loc.arp[,2],times=2),
                 RMSE=c(sqrt(v2hat.u)-sqrt(VHatU[,2]),sqrt(v2hat.v)-sqrt(VHatV[,2])),
                 group=as.factor(rep(c("U","V"),each=nrow(Dat.loc.arp))))
PT=ggplot()+facet_wrap(~ group, ncol = 2)+
  geom_raster(mapping=aes(lon, lat, fill = RMSE),data=dataF)+
  geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_fill_gradient2(low = "#3288BD",high = "#E41A1C",limits=c(-0.08,0.08))+
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
print(PT)  

Eig.arp=Re(readMat(here("Slepian_ARP","Basis_reg_eig_value_213_real.mat"))$Basis.reg.eig.value)
Rerank.id=rank(-Eig.arp,ties.method = "first")
Eig.arp.rerank=rep(0,length(Eig.arp))
for(i in 1:length(Eig.arp)){
  id=which(Rerank.id==i)
  Eig.arp.rerank[i]=Eig.arp[id]
}
Q.sl=length(which(Re(Eig.arp)>=0.5)) 
Basis.SLP.pre=readMat(here("Slepian_ARP","Slepian_spatial_213.mat"))$Slepian.spatial
Basis.SLP=matrix(0,nrow(Dat.loc),length(Eig.arp))
for(i in 1:length(Eig.arp)){
  id=which(Rerank.id==i)
  Basis.SLP[,i]=Basis.SLP.pre[,id]
}
Basis.SLP.pre=0
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
v2hat.u=apply(resv2.u,1,mean)  
v2hat.v=apply(resv2.v,1,mean)
dataF=data.frame(lon=rep(Dat.loc.arp[,1],times=2),lat=rep(Dat.loc.arp[,2],times=2),
                 RMSE=c(sqrt(v2hat.u)-sqrt(VHatU[,2]),sqrt(v2hat.v)-sqrt(VHatV[,2])),
                 group=as.factor(rep(c("U","V"),each=nrow(Dat.loc.arp))))

# Plot Figure S3(g)
PT=ggplot()+facet_wrap(~ group, ncol = 2)+
  geom_raster(mapping=aes(lon, lat, fill = RMSE),data=dataF)+
  geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_fill_gradient2(low = "#3288BD",high = "#E41A1C",limits=c(-0.08,0.42))+
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
print(PT)  



###### Part 3. Randomly select 1000 time points for testing performance of Slepian bases 
######         with the fixed \lambda_A=0.01 under Q=213
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
v2hat.u=apply(resv2.u,1,mean)  
v2hat.v=apply(resv2.v,1,mean)
dataF=data.frame(lon=rep(Dat.loc.arp[,1],times=2),lat=rep(Dat.loc.arp[,2],times=2),
                 RMSE=c(sqrt(v2hat.u)-sqrt(VHatU[,2]),sqrt(v2hat.v)-sqrt(VHatV[,2])),
                 group=as.factor(rep(c("U","V"),each=nrow(Dat.loc.arp))))

# Plot Figure 3(h)
PT=ggplot()+facet_wrap(~ group, ncol = 2)+
  geom_raster(mapping=aes(lon, lat, fill = RMSE),data=dataF)+
  geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_fill_gradient2(low = "#3288BD",high = "#E41A1C",limits=c(-0.08,0.42))+
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
print(PT)  






