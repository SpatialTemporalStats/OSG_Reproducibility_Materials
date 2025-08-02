################################################################################################
# This file provide the constrtcution of an OSG, following Algorithm 3                         #
################################################################################################

# Packages, functions, and data
# library(R.matlab)
# library(ggplot2)
# library(LambertW)
# library(fdaoutlier)
# library(approxOT)
# library(matrixStats)
# library(abind)
# source(here("Functions","InverseTH.R"))
# source(here("Functions","Assessment_Indices.R"))
# source(here("Data","Data_Treatment.R"))


# Slepian Bases with Q=181 on the ARP
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


# OSG information
R=10            # Number of ensembles
B=9             # Number of data blocks -1
T0=365*8        # Length of initial data block
Tt=365*8        # Length of subsequent data blocks


# T1=proc.time()[[3]]
####### Part I. Develop the OSG-Long  #########
### Step I. Process initial data block using Algorithm S1
# Step I(1). Get m_t(L_i,l_j) and detrend
# t1=proc.time()[[3]]
Windu.EnMean=apply(Windu.ARP[,,1:T0],c(2,3),mean)
Windv.EnMean=apply(Windv.ARP[,,1:T0],c(2,3),mean)
Windu.rsd=Windv.rsd=array(0,c(R,length(id.ARP),T0))
for(r in 1:R){
  Windu.rsd[r,,]=Windu.ARP[r,,1:T0]-Windu.EnMean
  Windv.rsd[r,,]=Windv.ARP[r,,1:T0]-Windv.EnMean
}
# t2=proc.time()[[3]]
# t2-t1=143.601, 47.859

# Step I(2). Translate the data to the Slepian domain and obtain \sigma_t(L_i,l_j)
# t1=proc.time()[[3]]
A=solve(t(Basis.SLP[id.ARP,1:Q.sl])%*%Basis.SLP[id.ARP,1:Q.sl],t(Basis.SLP[id.ARP,1:Q.sl]))
Windu.SLP=Windv.SLP=array(0,c(R,Q.sl,T0))
for(r in 1:R){
  Windu.SLP[r,,]=A%*%Windu.rsd[r,,]
  Windv.SLP[r,,]=A%*%Windv.rsd[r,,]
}
v2hat.u=v2hat.v=matrix(0,length(id.ARP),T0)
for(r in 1:R){
  v2hat.u=v2hat.u+(Basis.SLP[id.ARP,1:Q.sl]%*%Windu.SLP[r,,]-Windu.rsd[r,,])^2
  v2hat.v=v2hat.v+(Basis.SLP[id.ARP,1:Q.sl]%*%Windv.SLP[r,,]-Windv.rsd[r,,])^2
}
v2hat.u=v2hat.u/R
v2hat.v=v2hat.v/R
# t2=proc.time()[[3]]
# t2-t1=5.231, 9.163

# Step I(3). Tukey h transformation 
# t1=proc.time()[[3]] 
Gamma.u.online=Gamma.v.online=matrix(0,Q.sl,2)           # Store cumulative estimates at each b
TGHpara.u.online=TGHpara.v.online=matrix(0,Q.sl,2)
Windu.SLP.TGH=Windv.SLP.TGH=array(0,c(R,Q.sl,T0))
for(i in 1:Q.sl){
  ### Evaluate \gamma_1 and \gamma_2
  Gamma.u.online[i,1]=mean((Windu.SLP[,i,1:T0])^2)
  Gamma.v.online[i,1]=mean((Windv.SLP[,i,1:T0])^2)
  Gamma.u.online[i,2]=max(mean((Windu.SLP[,i,1:T0])^4)/(Gamma.u.online[i,1])^2,162/66)
  Gamma.v.online[i,2]=max(mean((Windv.SLP[,i,1:T0])^4)/(Gamma.v.online[i,1])^2,162/66)
  ### Evaluate \omega and h
  TGHpara.u.online[i,2]=max(sqrt(66*Gamma.u.online[i,2]-162)-6,0)/66
  TGHpara.v.online[i,2]=max(sqrt(66*Gamma.v.online[i,2]-162)-6,0)/66
  TGHpara.u.online[i,1]=sqrt(Gamma.u.online[i,1])*((1-2*TGHpara.u.online[i,2])^(3/4))
  TGHpara.v.online[i,1]=sqrt(Gamma.v.online[i,1])*((1-2*TGHpara.v.online[i,2])^(3/4))
  ### Tukey h transformation
  Windu.SLP.TGH[,i,]=THinvfunc(Windu.SLP[,i,],TGHpara.u.online[i,1],TGHpara.u.online[i,2])
  Windv.SLP.TGH[,i,]=THinvfunc(Windv.SLP[,i,],TGHpara.v.online[i,1],TGHpara.v.online[i,2])
}
# t2=proc.time()[[3]]
# t2-t1=4.241, 2.904

# Step I(4). VAR(2) model
# t1=proc.time()[[3]]
P=2
YY=matrix(0,R*(T0-P),2*Q.sl)
for(i in 1:Q.sl){
  YY[,i]=c(t(Windu.SLP.TGH[,i,(P+1):T0]))
  YY[,i+Q.sl]=c(t(Windv.SLP.TGH[,i,(P+1):T0]))
}
YX=matrix(0,R*(T0-P),2*Q.sl*P)
for(p in 1:P){
  for(i in 1:Q.sl){
    YX[,(p-1)*2*Q.sl+i]=c(t(Windu.SLP.TGH[,i,(P+1-p):(T0-p)]))
    YX[,(p-1)*2*Q.sl+i+Q.sl]=c(t(Windv.SLP.TGH[,i,(P+1-p):(T0-p)]))
  }
}
X.online=t(YX)%*%YX                                   # Store cumulative estimates at each b
Phi.hat.online=solve(X.online,t(YX)%*%YY)
K.online=crossprod(YY-YX%*%Phi.hat.online)
# t2=proc.time()[[3]]
# t2-t1=3.82, 3.99


### Step II. Process subsequent data blocks (Here we have to store all cumulative estimates )
for(b in 1:B){
  # Step II(1). Get m_t(L_i,l_j) and detrend
  # t1=proc.time()[[3]]
  Windu.EnMean=cbind(Windu.EnMean,apply(Windu.ARP[,,(T0+(b-1)*Tt+1):(T0+b*Tt)],c(2,3),mean))
  Windv.EnMean=cbind(Windv.EnMean,apply(Windv.ARP[,,(T0+(b-1)*Tt+1):(T0+b*Tt)],c(2,3),mean))
  Windu.rsd=Windv.rsd=array(0,c(R,length(id.ARP),Tt))
  for(r in 1:R){
    Windu.rsd[r,,]=Windu.ARP[r,,(T0+(b-1)*Tt+1):(T0+b*Tt)]-Windu.EnMean[,(T0+(b-1)*Tt+1):(T0+b*Tt)]
    Windv.rsd[r,,]=Windv.ARP[r,,(T0+(b-1)*Tt+1):(T0+b*Tt)]-Windv.EnMean[,(T0+(b-1)*Tt+1):(T0+b*Tt)]
  }
  # t2=proc.time()[[3]]
  # t2-t1=141.963, 45.54
  
  # Step II(2). Translate the data to the Slepian domain and obtain \sigma_t(L_i,l_j) 
  # t1=proc.time()[[3]]
  A=solve(t(Basis.SLP[id.ARP,1:Q.sl])%*%Basis.SLP[id.ARP,1:Q.sl],t(Basis.SLP[id.ARP,1:Q.sl]))
  Windu.SLP=Windv.SLP=array(0,c(R,Q.sl,Tt))
  for(r in 1:R){
    Windu.SLP[r,,]=A%*%Windu.rsd[r,,]
    Windv.SLP[r,,]=A%*%Windv.rsd[r,,]
  }
  v2hat.u.online=v2hat.v.online=matrix(0,length(id.ARP),Tt)
  for(r in 1:R){
    v2hat.u.online=v2hat.u.online+(Basis.SLP[id.ARP,1:Q.sl]%*%Windu.SLP[r,,]-Windu.rsd[r,,])^2
    v2hat.v.online=v2hat.v.online+(Basis.SLP[id.ARP,1:Q.sl]%*%Windv.SLP[r,,]-Windv.rsd[r,,])^2
  }
  v2hat.u=cbind(v2hat.u,v2hat.u.online/R)
  v2hat.v=cbind(v2hat.v,v2hat.v.online/R)
  # t2=proc.time()[[3]]
  # t2-t1=5.255, 8.609
  
  # Step II(3). Tukey h transformation with Lambert function
  # t1=proc.time()[[3]]
  Gamma.u.new=Gamma.v.new=matrix(0,Q.sl,2)
  TGHpara.u.new=TGHpara.v.new=matrix(0,Q.sl,2)
  Windu.SLP.TGH=Windv.SLP.TGH=array(0,c(R,Q.sl,Tt))
  for(i in 1:Q.sl){
    # Estimate \gamma_{*,\alpha}^{b} and \kappa_{*,\alpha}^{b} by Eq.~(7)
    Gamma.u.new[i,1]=mean((Windu.SLP[,i,])^2)                                     
    Gamma.v.new[i,1]=mean((Windv.SLP[,i,])^2)
    Gamma.u.new[i,2]=max(mean((Windu.SLP[,i,])^4)/((Gamma.u.new[i,1])^2),162/66)
    Gamma.v.new[i,2]=max(mean((Windv.SLP[,i,])^4)/((Gamma.v.new[i,1])^2),162/66)
    # Evaluate \omega_{*,\alpha}^{b} and h_{*,\alpha}^{b} by Eq.~(6)
    TGHpara.u.new[i,2]=max(sqrt(66*Gamma.u.new[i,2]-162)-6,0)/66                  
    TGHpara.v.new[i,2]=max(sqrt(66*Gamma.v.new[i,2]-162)-6,0)/66
    TGHpara.u.new[i,1]=sqrt(Gamma.u.new[i,1])*((1-2*TGHpara.u.new[i,2])^(3/4))
    TGHpara.v.new[i,1]=sqrt(Gamma.v.new[i,1])*((1-2*TGHpara.v.new[i,2])^(3/4))
    # Tukey h transformation for Block b
    Windu.SLP.TGH[,i,]=THinvfunc(Windu.SLP[,i,],TGHpara.u.new[i,1],TGHpara.u.new[i,2])
    Windv.SLP.TGH[,i,]=THinvfunc(Windv.SLP[,i,],TGHpara.v.new[i,1],TGHpara.v.new[i,2])
    # Update \gamma_{*,\alpha}^[b] and \kappa_{*,\alpha}^[b] using \gamma_{*,\alpha}^{b}, \kappa_{*,\alpha}^{b}, \gamma_{*,\alpha}^[b-1] and \kappa_{*,\alpha}^[b-1] by Eqs.(11) and (12) 
    wt.u=Gamma.u.online[i,1]
    wt.v=Gamma.v.online[i,1]
    Gamma.u.online[i,1]=((T0+(b-1)*Tt)*Gamma.u.online[i,1]+Tt*Gamma.u.new[i,1])/(T0+b*Tt)
    Gamma.v.online[i,1]=((T0+(b-1)*Tt)*Gamma.v.online[i,1]+Tt*Gamma.v.new[i,1])/(T0+b*Tt)
    Gamma.u.online[i,2]=((T0+(b-1)*Tt)*(wt.u^2)*Gamma.u.online[i,2]+Tt*((Gamma.u.new[i,1])^2)*Gamma.u.new[i,2])/(T0+b*Tt)/((Gamma.u.online[i,1])^2)
    Gamma.v.online[i,2]=((T0+(b-1)*Tt)*(wt.v^2)*Gamma.v.online[i,2]+Tt*((Gamma.v.new[i,1])^2)*Gamma.v.new[i,2])/(T0+b*Tt)/((Gamma.v.online[i,1])^2)
    # Calculate \omega^[b] and h^[b]
    TGHpara.u.online[i,2]=max(sqrt(66*Gamma.u.online[i,2]-162)-6,0)/66
    TGHpara.v.online[i,2]=max(sqrt(66*Gamma.v.online[i,2]-162)-6,0)/66
    TGHpara.u.online[i,1]=sqrt(Gamma.u.online[i,1])*((1-2*TGHpara.u.online[i,2])^(3/4))
    TGHpara.v.online[i,1]=sqrt(Gamma.v.online[i,1])*((1-2*TGHpara.v.online[i,2])^(3/4))
  }
  # t2=proc.time()[[3]]
  # t2-t1=4.982
  
  # Step II(4). VAR(2) model
  # t1=proc.time()[[3]]
  # Estimate \Phi^{b} and K^{b} by Eqs. (9) and (10)
  YY=matrix(0,R*(Tt-P),2*Q.sl)
  for(i in 1:Q.sl){
    YY[,i]=c(t(Windu.SLP.TGH[,i,(P+1):Tt]))
    YY[,i+Q.sl]=c(t(Windv.SLP.TGH[,i,(P+1):Tt]))
  }
  YX=matrix(0,R*(Tt-P),2*Q.sl*P)
  for(p in 1:P){
    for(i in 1:Q.sl){
      YX[,(p-1)*2*Q.sl+i]=c(t(Windu.SLP.TGH[,i,(P+1-p):(Tt-p)]))
      YX[,(p-1)*2*Q.sl+i+Q.sl]=c(t(Windv.SLP.TGH[,i,(P+1-p):(Tt-p)]))
    }
  }
  X.new=t(YX)%*%YX          
  Phi.hat.new=ginv(X.new)%*%t(YX)%*%YY    ## Here use generailized inverse
  K.new=crossprod(YY-YX%*%Phi.hat.new)
  # Update \Phi^[b] and K^[b] using \Phi^{b}, K^{b}, \Phi^[b-1], and K^[b-1] by Eqs. (13) and (14)
  M1=X.online%*%Phi.hat.online
  M2=t(Phi.hat.online)%*%M1
  X.online=X.online+X.new
  Phi.hat.online=solve(X.online,M1+X.new%*%Phi.hat.new)
  K.online=K.online+K.new+M2+t(Phi.hat.new)%*%X.new%*%Phi.hat.new-t(Phi.hat.online)%*%X.online%*%Phi.hat.online
  # t2=proc.time()[[3]]
  # t2-t1=6.693
}

