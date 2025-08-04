################################################################################
# This file includes all steps to reproduce Figures S2 and S3                  #
################################################################################
# Packages, functions, and data
# library(R.matlab)
# library(ggplot2)
# library(LambertW)
# library(fdaoutlier)
# library(approxOT)
# library(matrixStats)
# source(here("Functions","InverseTH.R"))
# source(here("Functions","Assessment_Indices.R"))
# source(here("Data","Data_Treatment.R"))
Col=c("#3288BD","#66C2A5","#ABDDA4","#E6F598","#FEE08B","#FDAE61","#F46D43","#D53E4F")


# ARP information for ploting figures
world_map=map_data("world")                                                   # Extract the map data for the region
arabian_peninsula_countries=c("Saudi Arabia", "Yemen", 
                              "Oman", "United Arab Emirates", 
                              "Qatar", "Bahrain", "Kuwait")                   # Define the countries of the Arabian Peninsula
arabian_peninsula=subset(world_map, region %in% arabian_peninsula_countries)  # Filter the map data for the Arabian Peninsula countries
dataARP=data.frame(lon=rep(arabian_peninsula$long,times=2),lat=rep(arabian_peninsula$lat,times=2),
                   type=rep(arabian_peninsula$group,times=2),
                   group=as.factor(rep(c("A=100","A=300"),each=length(arabian_peninsula$long))))



###### Part 1. Develop SG with various A values using data from the first year and evaluate their emulation performance
# Process data
# t1=proc.time()[[3]]
R=10
T0=365*8
P=2
Windu.ARP1=Windu.ARP[,,1:T0]
Windv.ARP1=Windv.ARP[,,1:T0]
Windu.EnMean1=apply(Windu.ARP1,c(2,3),mean)
Windv.EnMean1=apply(Windv.ARP1,c(2,3),mean)
Windu.rsd1=Windv.rsd1=array(0,c(R,length(id.ARP),T0))
for(r in 1:R){
  Windu.rsd1[r,,]=Windu.ARP1[r,,]-Windu.EnMean1
  Windv.rsd1[r,,]=Windv.ARP1[r,,]-Windv.EnMean1
}

# Calculate some values in advance to avoid repeat operations
NCPGT.dat=get.ITP(Windu.ARP1,Windv.ARP1)
Windu.EnSD1=matrix(colSds(matrix(Windu.ARP1,nrow=R,ncol=length(id.ARP)*T0)),length(id.ARP),T0)
Windv.EnSD1=matrix(colSds(matrix(Windv.ARP1,nrow=R,ncol=length(id.ARP)*T0)),length(id.ARP),T0)
# t2=proc.time()[[3]]
# t2-t1=223.627

# Obtain results for all Q values
Qseq=c(96,109,122,133,144,154,163,172,181,190,198,205,213)   
Iuq.u=Iuq.v=Iwdt.u=Iwdt.v=matrix(0,length(id.ARP),length(Qseq))
Iwds.u=Iwds.v=matrix(0,T0,length(Qseq))
Ibc=Itc1.u=Itc1.v=Itc2.u=Itc2.v=Imd=Isdd=matrix(0,2*length(id.ARP),length(Qseq))
Itp=array(0,c(length(Qseq),4,2*length(id.ARP)))
for(k in 1:length(Qseq)){
  t1=proc.time()[[3]]
  Q=Qseq[k]
  
  # Develop SG
  Eig.arp=Re(readMat(here("Slepian_ARP",paste0("Basis_reg_eig_value_",Q,"_real.mat")))$Basis.reg.eig.value)
  Rerank.id=rank(-Eig.arp,ties.method = "first")
  Eig.arp.rerank=rep(0,length(Eig.arp))
  for(i in 1:length(Eig.arp)){
    id=which(Rerank.id==i)
    Eig.arp.rerank[i]=Eig.arp[id]
  }
  Q.sl=length(which(Re(Eig.arp)>=0.01)) 
  Basis.SLP.pre=readMat(here("Slepian_ARP",paste0("Slepian_spatial_",Q,".mat")))$Slepian.spatial
  Basis.SLP=matrix(0,nrow(Dat.loc),length(Eig.arp))
  for(i in 1:length(Eig.arp)){
    id=which(Rerank.id==i)
    Basis.SLP[,i]=Basis.SLP.pre[,id]
  }
  Basis.SLP.pre=0
  A=solve(t(Basis.SLP[id.ARP,1:Q.sl])%*%Basis.SLP[id.ARP,1:Q.sl],t(Basis.SLP[id.ARP,1:Q.sl]))
  Windu.SLP1=Windv.SLP1=array(0,c(R,Q.sl,T0))
  for(r in 1:R){
    Windu.SLP1[r,,]=A%*%Windu.rsd1[r,,]
    Windv.SLP1[r,,]=A%*%Windv.rsd1[r,,]
  }
  v2hat.u1=v2hat.v1=matrix(0,length(id.ARP),T0)
  for(r in 1:R){
    v2hat.u1=v2hat.u1+(Basis.SLP[id.ARP,1:Q.sl]%*%Windu.SLP1[r,,]-Windu.rsd1[r,,])^2
    v2hat.v1=v2hat.v1+(Basis.SLP[id.ARP,1:Q.sl]%*%Windv.SLP1[r,,]-Windv.rsd1[r,,])^2
  }
  v2hat.u1=v2hat.u1/R
  v2hat.v1=v2hat.v1/R
  
  Gamma.u1=Gamma.v1=matrix(0,Q.sl,2)
  TGHpara.u1=TGHpara.v1=matrix(0,Q.sl,2)
  Windu.SLP.TGH1=Windv.SLP.TGH1=array(0,c(R,Q.sl,T0))
  for(i in 1:Q.sl){
    Gamma.u1[i,1]=mean((Windu.SLP1[,i,])^2)
    Gamma.v1[i,1]=mean((Windv.SLP1[,i,])^2)
    Gamma.u1[i,2]=max(mean((Windu.SLP1[,i,])^4)/(Gamma.u1[i,1])^2,162/66)
    Gamma.v1[i,2]=max(mean((Windv.SLP1[,i,])^4)/(Gamma.v1[i,1])^2,162/66)
    
    TGHpara.u1[i,2]=max(sqrt(66*Gamma.u1[i,2]-162)-6,0)/66
    TGHpara.v1[i,2]=max(sqrt(66*Gamma.v1[i,2]-162)-6,0)/66
    TGHpara.u1[i,1]=sqrt(Gamma.u1[i,1])*((1-2*TGHpara.u1[i,2])^(3/4))
    TGHpara.v1[i,1]=sqrt(Gamma.v1[i,1])*((1-2*TGHpara.v1[i,2])^(3/4))
    
    Windu.SLP.TGH1[,i,]=THinvfunc(Windu.SLP1[,i,],TGHpara.u1[i,1],TGHpara.u1[i,2])
    Windv.SLP.TGH1[,i,]=THinvfunc(Windv.SLP1[,i,],TGHpara.v1[i,1],TGHpara.v1[i,2])
  }
  
  YY=matrix(0,R*(T0-P),2*Q.sl)
  for(i in 1:Q.sl){
    YY[,i]=c(t(Windu.SLP.TGH1[,i,(P+1):T0]))
    YY[,i+Q.sl]=c(t(Windv.SLP.TGH1[,i,(P+1):T0]))
  }
  YX=matrix(0,R*(T0-P),2*Q.sl*P)
  for(p in 1:P){
    for(i in 1:Q.sl){
      YX[,(p-1)*2*Q.sl+i]=c(t(Windu.SLP.TGH1[,i,(P+1-p):(T0-p)]))
      YX[,(p-1)*2*Q.sl+i+Q.sl]=c(t(Windv.SLP.TGH1[,i,(P+1-p):(T0-p)]))
    }
  }
  Phi.hat=solve(t(YX)%*%YX,t(YX)%*%YY)             
  K=crossprod(YY-YX%*%Phi.hat)/R/(T0-P) 
  t2=proc.time()[[3]]
  # t2-t1=7.447(96), 7.96(109), 6.679(122), 8.172(133), 11.036(144), 9.594(154), 11.324(163), 
  #       10.089(172), 10.66(181), 13.133(190), 11.633(198), 12.719(205), 17.224(213)
  
  # Generate emulations
  t1=proc.time()[[3]]
  LL=t(chol(K))                                
  EPS.u=EPS.v=array(0,c(R,length(id.ARP),T0))       
  for(i in 1:length(id.ARP)){
    for(t in 1:T0){
      set.seed(t+(i-1)*T0)
      EPS.u[,i,t]=rnorm(R,mean=0,sd=sqrt(v2hat.u1[i,t]))
      EPS.v[,i,t]=rnorm(R,mean=0,sd=sqrt(v2hat.v1[i,t]))
    }
  }
  Gen.Dat.U=Gen.Dat.V=array(0,c(R,length(id.ARP),T0)) 
  for(r in 1:R){
    Z=matrix(0,2*Q.sl,T0+P)
    for(t in 1:(T0+P)){
      set.seed(t+(r-1)*(T0+P))
      Z[,t]=rnorm(2*Q.sl)
    }
    Z[,1:P]=LL%*%Z[,1:P]
    for(t in (P+1):(T0+P)){
      Z[,t]=LL%*%Z[,t]+t(c(Z[,t-1],Z[,t-2])%*%Phi.hat)
    }
    Z=Z[,-(1:P)]
    
    for(i in 1:Q.sl){
      taughu=function(z){return(TGHpara.u1[i,1]*z*exp(TGHpara.u1[i,2]*z^2/2))}
      Z[i,]=taughu(Z[i,])
      taughv=function(z){return(TGHpara.v1[i,1]*z*exp(TGHpara.v1[i,2]*z^2/2))}
      Z[Q.sl+i,]=taughv(Z[Q.sl+i,])
    }
    
    Gen.Dat.U[r,,]=Basis.SLP[id.ARP,1:Q.sl]%*%Z[1:Q.sl,]
    Gen.Dat.V[r,,]=Basis.SLP[id.ARP,1:Q.sl]%*%Z[-(1:Q.sl),]
    
    Gen.Dat.U[r,,]=Windu.EnMean1+Gen.Dat.U[r,,]+EPS.u[r,,]
    Gen.Dat.V[r,,]=Windv.EnMean1+Gen.Dat.V[r,,]+EPS.v[r,,]
  }
  t2=proc.time()[[3]]
  # t2-t1=61.952(96), 56.302(109), 54.492(122), 60.599(133), 66.649(144), 66.546(154), 70.505(163), 
  #       70.873(172), 68.067(181), 77.818(190), 80.339(198), 82.074(205), 82.645(213)
  
  # Evaluation
  t1=proc.time()[[3]]
  Iuq.u[,k]=get.IUQ(Gen.Dat.U,Windu.ARP1)
  Iuq.v[,k]=get.IUQ(Gen.Dat.V,Windv.ARP1)
  
  GenDu.EnMean=matrix(colMeans2(matrix(Gen.Dat.U,nrow=R,ncol=length(id.ARP)*T0)),length(id.ARP),T0)   
  GenDv.EnMean=matrix(colMeans2(matrix(Gen.Dat.V,nrow=R,ncol=length(id.ARP)*T0)),length(id.ARP),T0)   
  Genu.rsd=Genv.rsd=array(0,c(R,length(id.ARP),T0))
  for(r in 1:R){
    Genu.rsd[r,,]=Gen.Dat.U[r,,]-GenDu.EnMean
    Genv.rsd[r,,]=Gen.Dat.V[r,,]-GenDv.EnMean
  }
  Ibc[,k]=get.IBC(Genu.rsd,Genv.rsd,Windu.rsd1,Windv.rsd1)
  
  Itc1.u[,k]=get.ITC1(Genu.rsd,Windu.rsd1)
  Itc1.v[,k]=get.ITC1(Genv.rsd,Windv.rsd1)
  Itc2.u[,k]=get.ITC2(Genu.rsd,Windu.rsd1)
  Itc2.v[,k]=get.ITC2(Genv.rsd,Windv.rsd1)
  
  NCPGT.emu=get.ITP(Gen.Dat.U,Gen.Dat.V)
  Itp[k,,]=cbind(NCPGT.emu,NCPGT.dat)
  
  Iwdt.u[,k]=get.IWDT(Gen.Dat.U,Windu.ARP1)
  Iwdt.v[,k]=get.IWDT(Gen.Dat.V,Windv.ARP1)
  Iwds.u[,k]=get.IWDS(Gen.Dat.U,Windu.ARP1)
  Iwds.v[,k]=get.IWDS(Gen.Dat.V,Windv.ARP1)
  
  Imd[,k]=c(apply(sqrt((Windu.EnMean1-GenDu.EnMean)^2),1,mean),apply(sqrt((Windv.EnMean1-GenDv.EnMean)^2),1,mean))
  
  GenDu.EnSD=matrix(colSds(matrix(Gen.Dat.U,nrow=R,ncol=length(id.ARP)*T0)),length(id.ARP),T0)
  GenDv.EnSD=matrix(colSds(matrix(Gen.Dat.V,nrow=R,ncol=length(id.ARP)*T0)),length(id.ARP),T0)
  Isdd[,k]=c(apply(sqrt((Windu.EnSD1-GenDu.EnSD)^2),1,mean),apply(sqrt((Windv.EnSD1-GenDv.EnSD)^2),1,mean))
  t2=proc.time()[[3]]
  # t2-t1=363.72
}



###### Part 2. Reproduce Figures S2 and S3
Aseq=25*(0:12)+100
# Plot Figure S2(a)
dataF=data.frame(Iuq=c(as.matrix((Iuq.u+Iuq.v)/2)),aseq=as.factor(rep(Aseq,each=length(id.ARP))))
PT=ggplot(data = dataF,aes(x=aseq,y=Iuq),color="#3288BD")+geom_boxplot()+
  theme_bw()+theme(panel.border = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.text.y = element_text(angle=90,size=12),
                   axis.title=element_text(size=14),
                   #legend.justification=c(0,1),
                   legend.position ="right",
                   legend.title = element_blank(),
                   legend.background = element_blank(),
                   legend.key.width=unit(1,"line"),
                   legend.key.height=unit(1,"line"))+
  xlab(expression(A))+ylab(expression(I[uq]))+
  geom_hline(yintercept = 1,linetype=2,col="red")
print(PT)

# Plot Figure S2(b)
dataF=data.frame(Ibd=c(as.matrix(Ibc[1:length(id.ARP),]/Ibc[-(1:length(id.ARP)),])),aseq=as.factor(rep(Aseq,times=length(id.ARP))))
PT=ggplot(data = dataF,aes(x=aseq,y=Ibd))+geom_boxplot()+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.border = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.text.y = element_text(angle=90,size=12),
                   axis.title=element_text(size=14),
                   legend.justification=c(1,1),
                   legend.position =c(1,1),
                   legend.title = element_blank(),
                   legend.background = element_blank(),
                   legend.key.width=unit(2,"line"),
                   legend.key.height=unit(1,"line"))+
  xlab(expression(A))+ylab(expression(I[bc]))+
  geom_hline(yintercept = 1,linetype=2,colour="red")
print(PT)

# Plot Figure S2(c)
dataF=data.frame(Itc1=c(as.matrix(0.5*Itc1.u[1:length(id.ARP),]/Itc1.u[-(1:length(id.ARP)),]+
                          0.5*Itc1.v[1:length(id.ARP),]/Itc1.v[-(1:length(id.ARP)),])),
                 aseq=as.factor(rep(Aseq,each=length(id.ARP))))
PT=ggplot(data = dataF,aes(x=aseq,y=Itc1))+geom_boxplot()+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.border = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.text.y = element_text(angle=90,size=12),
                   axis.title=element_text(size=14),
                   legend.justification=c(1,1),
                   legend.position =c(1,1),
                   legend.title = element_blank(),
                   legend.background = element_blank(),
                   legend.key.width=unit(2,"line"),
                   legend.key.height=unit(1,"line"))+
  xlab(expression(A))+ylab(expression(I[tc1]))+
  geom_hline(yintercept = 1,linetype=2,colour="red")
print(PT)

# Plot Figure S2(d)
dataF=data.frame(Itc2=c(as.matrix(0.5*Itc2.u[1:length(id.ARP),]/Itc2.u[-(1:length(id.ARP)),]+
                          0.5*Itc2.v[1:length(id.ARP),]/Itc2.v[-(1:length(id.ARP)),])),
                 aseq=as.factor(rep(Aseq,each=length(id.ARP))))
PT=ggplot(data = dataF,aes(x=aseq,y=Itc2))+geom_boxplot()+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.border = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.text.y = element_text(angle=90,size=12),
                   axis.title=element_text(size=14),
                   legend.justification=c(1,1),
                   legend.position =c(1,1),
                   legend.title = element_blank(),
                   legend.background = element_blank(),
                   legend.key.width=unit(2,"line"),
                   legend.key.height=unit(1,"line"))+
  xlab(expression(A))+ylab(expression(I[tc2]))+
  geom_hline(yintercept = 1,linetype=2,colour="red")
print(PT)

# Plot Figure S2(e)
dataF=data.frame(Itp=c(Itp[,1,1:length(id.ARP)]/Itp[,1,-(1:length(id.ARP))]),
                 aseq=as.factor(rep(Aseq,times=length(id.ARP))))
PT=ggplot(data = dataF,aes(x=aseq,y=Itp))+geom_boxplot()+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.border = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.text.y = element_text(angle=90,size=12),
                   axis.title=element_text(size=14),
                   legend.justification=c(1,1),
                   legend.position =c(1,1),
                   legend.title = element_blank(),
                   legend.background = element_blank(),
                   legend.key.width=unit(2,"line"),
                   legend.key.height=unit(1,"line"))+
  geom_hline(yintercept = 1,linetype=2,colour="red")+
  xlab(expression(A))+ylab(expression(I[tp]^{"[0,5)"}))
print(PT)

# Plot Figure S2(f)
dataF=data.frame(Itp=c(Itp[,2,1:length(id.ARP)]/Itp[,2,-(1:length(id.ARP))]),
                 aseq=as.factor(rep(Aseq,times=length(id.ARP))))
PT=ggplot(data = dataF,aes(x=aseq,y=Itp))+geom_boxplot()+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.border = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.text.y = element_text(angle=90,size=12),
                   axis.title=element_text(size=14),
                   legend.justification=c(1,1),
                   legend.position =c(1,1),
                   legend.title = element_blank(),
                   legend.background = element_blank(),
                   legend.key.width=unit(2,"line"),
                   legend.key.height=unit(1,"line"))+
  geom_hline(yintercept = 1,linetype=2,colour="red")+
  xlab(expression(A))+ylab(expression(I[tp]^{"[5,10)"}))
print(PT)

# Plot Figure S2(g)
dataF=data.frame(Itp=c(Itp[,3,1:length(id.ARP)]/Itp[,3,-(1:length(id.ARP))]),
                 aseq=as.factor(rep(Aseq,times=length(id.ARP))))
PT=ggplot(data = dataF,aes(x=aseq,y=Itp))+geom_boxplot()+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.border = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.text.y = element_text(angle=90,size=12),
                   axis.title=element_text(size=14),
                   legend.justification=c(1,1),
                   legend.position =c(1,1),
                   legend.title = element_blank(),
                   legend.background = element_blank(),
                   legend.key.width=unit(2,"line"),
                   legend.key.height=unit(1,"line"))+
  geom_hline(yintercept = 1,linetype=2,colour="red")+
  xlab(expression(A))+ylab(expression(I[tp]^{"[10,20)"}))
print(PT)

# Plot Figure S2(h)
dataF=data.frame(Itp=c(Itp[,4,1:length(id.ARP)]/Itp[,4,-(1:length(id.ARP))]),
                 aseq=as.factor(rep(Aseq,times=length(id.ARP))))
PT=ggplot(data = dataF,aes(x=aseq,y=Itp))+geom_boxplot()+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.border = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.text.y = element_text(angle=90,size=12),
                   axis.title=element_text(size=14),
                   legend.justification=c(1,1),
                   legend.position =c(1,1),
                   legend.title = element_blank(),
                   legend.background = element_blank(),
                   legend.key.width=unit(2,"line"),
                   legend.key.height=unit(1,"line"))+
  geom_hline(yintercept = 1,linetype=2,colour="red")+
  xlab(expression(A))+ylab(expression(I[tp]^{"[20,"*infinity*")"}))
print(PT)

# Plot Figure S3(a)
dataF=data.frame(Iwdt=c(as.matrix((Iwdt.u+Iwdt.v)/2)),aseq=as.factor(rep(Aseq,each=length(id.ARP))))
PT=ggplot(data = dataF,aes(x=aseq,y=Iwdt))+geom_boxplot()+
  #scale_y_continuous(limits=c(0.004,0.075))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.border = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.text.y = element_text(angle=90,size=12),
                   axis.title=element_text(size=14),
                   legend.justification=c(1,1),
                   legend.position ="none",
                   legend.title = element_blank(),
                   legend.background = element_blank(),
                   legend.key.width=unit(1,"line"),
                   legend.key.height=unit(1,"line"))+
  xlab(expression(A))+ylab(expression(I[wdt]))+
  geom_hline(yintercept = 0,linetype=2,col="red")
print(PT)

# Plot Figure S3(b)
dataF=data.frame(Iwds=c(as.matrix((Iwds.u+Iwds.v)/2)),aseq=as.factor(rep(Aseq,each=T0)))
PT=ggplot(data = dataF,aes(x=aseq,y=Iwds))+geom_boxplot()+
  #scale_y_continuous(limits=c(0.004,0.075))+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.border = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.text.y = element_text(angle=90,size=12),
                   axis.title=element_text(size=14),
                   legend.justification=c(1,1),
                   legend.position ="none",
                   legend.title = element_blank(),
                   legend.background = element_blank(),
                   legend.key.width=unit(1,"line"),
                   legend.key.height=unit(1,"line"))+
  xlab(expression(A))+ylab(expression(I[wds]))+
  geom_hline(yintercept = 0,linetype=2,col="red")
print(PT)

# Plot Figure S3(c)
dataF=data.frame(Imd=c(0.5*Imd[1:length(id.ARP),]+0.5*Imd[-(1:length(id.ARP)),]),
                 aseq=as.factor(rep(Aseq,each=length(id.ARP))))
PT=ggplot(data = dataF,aes(x=aseq,y=Imd))+geom_boxplot()+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.border = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.text.y = element_text(angle=90,size=12),
                   axis.title=element_text(size=14),
                   legend.justification=c(1,1),
                   legend.position =c(1,1),
                   legend.title = element_blank(),
                   legend.background = element_blank(),
                   legend.key.width=unit(2,"line"),
                   legend.key.height=unit(1,"line"))+
  xlab(expression(A))+ylab(expression(I[md]))+
  geom_hline(yintercept = 0,linetype=2,colour="red")
print(PT)

# Plot Figure S3(d)
dataF=data.frame(Isdd=c(0.5*Isdd[1:length(id.ARP),]+0.5*Isdd[-(1:length(id.ARP)),]),
                 aseq=as.factor(rep(Aseq,each=length(id.ARP))))
PT=ggplot(data = dataF,aes(x=aseq,y=Isdd))+geom_boxplot()+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.border = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.text.y = element_text(angle=90,size=12),
                   axis.title=element_text(size=14),
                   legend.justification=c(1,1),
                   legend.position =c(1,1),
                   legend.title = element_blank(),
                   legend.background = element_blank(),
                   legend.key.width=unit(2,"line"),
                   legend.key.height=unit(1,"line"))+
  xlab(expression(A))+ylab(expression(I[sdd]))+
  geom_hline(yintercept = 0,linetype=2,colour="red")
print(PT)

# Plot Figure S3(e)
dataF=data.frame(lon=rep(Dat.loc.arp[,1],times=2),lat=rep(Dat.loc.arp[,2],times=2),
                 Iuq=c(Iuq.u[,1],Iuq.u[,9]),
                 group=as.factor(rep(c("A=100","A=300"),each=nrow(Dat.loc.arp))))
PT=ggplot()+facet_wrap(~ group, ncol = 2)+
  geom_raster(mapping=aes(lon, lat, fill = Iuq),data=dataF)+
  geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_fill_gradient2(low = "#3288BD",mid="white",high = "#E41A1C",midpoint=1,limits=c(0.9,2.15))+
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
  labs(fill=expression(I[uq]))+ylab("Latitude")+xlab("Longitude")
print(PT)

# Plot Figure S3(f)
dataF=data.frame(lon=rep(Dat.loc.arp[,1],times=2),lat=rep(Dat.loc.arp[,2],times=2),
                 Iuq=c(Iwdt.u[,1],Iwdt.u[,9]),
                 group=as.factor(rep(c("A=100","A=300"),each=nrow(Dat.loc.arp))))
PT=ggplot()+facet_wrap(~ group, ncol = 2)+
  geom_raster(mapping=aes(lon, lat, fill = Iuq),data=dataF)+
  geom_path(mapping=aes(x = lon, y = lat, group = type), data=dataARP,size=0.3,color="darkgray")+
  scale_fill_gradient2(low = "white",high = "#E41A1C",limits=c(0,0.056))+
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
  labs(fill=expression(I[wdt]))+ylab("Latitude")+xlab("Longitude")
print(PT)



