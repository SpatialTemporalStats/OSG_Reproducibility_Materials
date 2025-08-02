################################################################################################
# OSG-Long in Scenario 1: Construction, Emulation, and Evaluation                              #
################################################################################################
# ! Note that in this file, we have to store the updates of cumulative estimates to plot e.g, Figure 8(c) and Table~S3. 
# ! OSG_Algorithm3.R strictly follows Algorithm 3 and shows the real idea of online updating

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


# OSG-Long information
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
TGHpara.u.all=TGHpara.v.all=array(0,c(B+1,Q.sl,2))       # Store cumulative estimates at all block indices b=0,...,B
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
TGHpara.u.all[1,,]=TGHpara.u.online
TGHpara.v.all[1,,]=TGHpara.v.online
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
Phi.hat.all=array(0,c(B+1,2*P*Q.sl,2*Q.sl))           # Store cumulative estimates at all block indices b=0,...,B
K.all=array(0,c(B+1,2*Q.sl,2*Q.sl))
Phi.hat.all[1,,]=Phi.hat.online
K.all[1,,]=K.online
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
  TGHpara.u.all[b+1,,]=TGHpara.u.online
  TGHpara.v.all[b+1,,]=TGHpara.v.online
  # t2=proc.time()[[3]]
  # t2-t1=4.982, 2.818
  
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
  Phi.hat.all[b+1,,]=Phi.hat.online
  K.all[b+1,,]=K.online
  # t2=proc.time()[[3]]
  # t2-t1=6.693, 6.36
}
writeMat(here("Case_Studies/Scenario1_OSG_Long","TGHpara_u_all_1.mat"),TGHparauall=TGHpara.u.all)
writeMat(here("Case_Studies/Scenario1_OSG_Long","TGHpara_v_all_1.mat"),TGHparavall=TGHpara.v.all)
writeMat(here("Case_Studies/Scenario1_OSG_Long","Phihat_online_1.mat"),Phihatonline=Phi.hat.all[c(1,B+1),,])
writeMat(here("Case_Studies/Scenario1_OSG_Long","K_online_1.mat"),Konline=K.all[c(1,B+1),,])
# T2=proc.time()[[3]]
# T2-T1


### Calculate the relative Frobenius distances (RFDs) between cumulative estimates at each b and estimates of FSG for figures and plots
# t1=proc.time()[[3]]
Phi.hat.full=readMat(here("Case_Studies/FSG","Phihatfull.mat"))$Phihatfull
K.full=readMat(here("Case_Studies/FSG","Kfull.mat"))$Kfull
RFD.Phi.1=matrix(0,B+1,2)
RFD.K.1=rep(0,B+1)
for(b in 1:(B+1)){
  RFD.Phi.1[b,1]=sqrt(sum((Phi.hat.all[b,1:(2*Q.sl),]-Phi.hat.full[1:(2*Q.sl),])^2))/sqrt(sum(Phi.hat.full[1:(2*Q.sl),]^2))
  RFD.Phi.1[b,2]=sqrt(sum((Phi.hat.all[b,-(1:(2*Q.sl)),]-Phi.hat.full[-(1:(2*Q.sl)),])^2))/sqrt(sum(Phi.hat.full[-(1:(2*Q.sl)),]^2))
  RFD.K.1[b]=sqrt(sum((K.all[b,,]/R/(365*8+(b-1)*365*8-b*P)-K.full)^2))/sqrt(sum(K.full^2))
}
write.csv(cbind(RFD.Phi.1,RFD.K.1),here("Case_Studies/Scenario1_OSG_Long","RFDs_OSG_Long.csv"))
# t2=proc.time()[[3]]
# t2-t1=0.443



####### Part II. Generate emulations using OSG-Long by Algorithm S2 #########
# t1=proc.time()[[3]]
LL=t(chol(K.online/R/(T0+B*Tt-(B+1)*P)))
EPS.u=EPS.v=array(0,c(R,length(id.ARP),T0+B*Tt))
for(i in 1:length(id.ARP)){
  for(t in 1:(T0+B*Tt)){
    set.seed(t+(i-1)*(T0+B*Tt))
    EPS.u[,i,t]=rnorm(R,mean=0,sd=sqrt(v2hat.u[i,t]))
    EPS.v[,i,t]=rnorm(R,mean=0,sd=sqrt(v2hat.v[i,t]))
  }
}
Gen.Dat.U.online=Gen.Dat.V.online=array(0,c(R,length(id.ARP),T0+B*Tt))
for(r in 1:R){
  # Step 1. Generate transformed coefficients
  Z=matrix(0,2*Q.sl,T0+B*Tt+P)
  for(t in 1:(T0+B*Tt+P)){
    set.seed(t+(r-1)*(T0+B*Tt+P))
    Z[,t]=rnorm(2*Q.sl)
  }
  Z[,1:P]=LL%*%Z[,1:P]
  for(t in (P+1):(T0+B*Tt+P)){
    Z[,t]=LL%*%Z[,t]+t(c(Z[,t-1],Z[,t-2])%*%Phi.hat.online)
  }
  Z=Z[,-(1:P)]
  
  # Step2. Inverse Tukey h
  for(i in 1:Q.sl){
    taughu=function(z){return(TGHpara.u.online[i,1]*z*exp(TGHpara.u.online[i,2]*z^2/2))}
    Z[i,]=taughu(Z[i,])
    taughv=function(z){return(TGHpara.v.online[i,1]*z*exp(TGHpara.v.online[i,2]*z^2/2))}
    Z[Q.sl+i,]=taughv(Z[Q.sl+i,])
  }
  
  # Step 3. Inverse Slepian
  Gen.Dat.U.online[r,,]=Basis.SLP[id.ARP,1:Q.sl]%*%Z[1:Q.sl,]
  Gen.Dat.V.online[r,,]=Basis.SLP[id.ARP,1:Q.sl]%*%Z[-(1:Q.sl),]
  
  # Step 4. Add mean and residuals
  Gen.Dat.U.online[r,,]=Windu.EnMean+Gen.Dat.U.online[r,,]+EPS.u[r,,]
  Gen.Dat.V.online[r,,]=Windv.EnMean+Gen.Dat.V.online[r,,]+EPS.v[r,,]
}
# t2=proc.time()[[3]]
# t2-t1=828.718



####### Part III. Evaluate emulation performance of OSG-Long #########
### Calculate Iuq for Uncertainty 
Iuq.u.online.1=get.IUQ(Gen.Dat.U.online,Windu.ARP)
Iuq.v.online.1=get.IUQ(Gen.Dat.V.online,Windv.ARP)
write.csv(Iuq.u.online.1,here("Case_Studies/Scenario1_OSG_Long","Iuq_u_online_1.csv"))
write.csv(Iuq.v.online.1,here("Case_Studies/Scenario1_OSG_Long","Iuq_v_online_1.csv"))


### Prepare for calculating Ibc by calculating bivariate correlations
GenDu.EnMean=matrix(colMeans2(matrix(Gen.Dat.U.online,nrow=R,ncol=length(id.ARP)*(T0+B*Tt))),length(id.ARP),T0+B*Tt)   # Use colMeans2 to calculate ensemble mean.
GenDv.EnMean=matrix(colMeans2(matrix(Gen.Dat.V.online,nrow=R,ncol=length(id.ARP)*(T0+B*Tt))),length(id.ARP),T0+B*Tt)   # It makes computation much faster but takes more storage
Genu.rsd=Genv.rsd=array(0,c(R,length(id.ARP),T0+B*Tt))
for(r in 1:R){
  Genu.rsd[r,,]=Gen.Dat.U.online[r,,]-GenDu.EnMean
  Genv.rsd[r,,]=Gen.Dat.V.online[r,,]-GenDv.EnMean
}
Windu.rsd=Windv.rsd=array(0,c(R,length(id.ARP),T0+B*Tt))            
for(r in 1:R){
  Windu.rsd[r,,]=Windu.ARP[r,,]-Windu.EnMean
  Windv.rsd[r,,]=Windv.ARP[r,,]-Windv.EnMean
}
Ibc.online.1=get.IBC(Genu.rsd,Genv.rsd,Windu.rsd,Windv.rsd)
write.csv(Ibc.online.1,here("Case_Studies/Scenario1_OSG_Long","Ibc_online_1.csv"))


### Prepare for calculating Itc1 and Itc2 by calculating temporal correlations at lag=1 and 2
Itc1.u.online.1=get.ITC1(Genu.rsd,Windu.rsd)
Itc1.v.online.1=get.ITC1(Genv.rsd,Windv.rsd)
Itc2.u.online.1=get.ITC2(Genu.rsd,Windu.rsd)
Itc2.v.online.1=get.ITC2(Genv.rsd,Windv.rsd)
write.csv(Itc1.u.online.1,here("Case_Studies/Scenario1_OSG_Long","Itc1_u_online_1.csv"))
write.csv(Itc1.v.online.1,here("Case_Studies/Scenario1_OSG_Long","Itc1_v_online_1.csv"))
write.csv(Itc2.u.online.1,here("Case_Studies/Scenario1_OSG_Long","Itc2_u_online_1.csv"))
write.csv(Itc2.v.online.1,here("Case_Studies/Scenario1_OSG_Long","Itc2_v_online_1.csv"))


### Prepare for calculating Itp by calculating the NCPGT
NCPGT.emu=get.ITP(Gen.Dat.U.online,Gen.Dat.V.online)
NCPGT.dat=get.ITP(Windu.ARP,Windv.ARP)
write.csv(cbind(NCPGT.emu,NCPGT.dat),here("Case_Studies/Scenario1_OSG_Long","Itp_online_1.csv"))


### Calculate Iwdt and Iwds for marginal distribution
Iwdt.u.online.1=get.IWDT(Gen.Dat.U.online,Windu.ARP)
Iwdt.v.online.1=get.IWDT(Gen.Dat.V.online,Windv.ARP)
Iwds.u.online.1=get.IWDS(Gen.Dat.U.online,Windu.ARP)
Iwds.v.online.1=get.IWDS(Gen.Dat.V.online,Windv.ARP)
write.csv(Iwdt.u.online.1,here("Case_Studies/Scenario1_OSG_Long","Iwdt_u_online_1.csv"))
write.csv(Iwdt.v.online.1,here("Case_Studies/Scenario1_OSG_Long","Iwdt_v_online_1.csv"))
write.csv(Iwds.u.online.1,here("Case_Studies/Scenario1_OSG_Long","Iwds_u_online_1.csv"))
write.csv(Iwds.v.online.1,here("Case_Studies/Scenario1_OSG_Long","Iwds_v_online_1.csv"))


### Calculate Imd for mean shift
Imd.online.1=c(apply(sqrt((Windu.EnMean-GenDu.EnMean)^2),1,mean),
           apply(sqrt((Windv.EnMean-GenDv.EnMean)^2),1,mean))
write.csv(Imd.online.1,here("Case_Studies/Scenario1_OSG_Long","Imd_online_1.csv"))


### Calculate Isdd for SD shift
Windu.EnSD=matrix(colSds(matrix(Windu.ARP,nrow=R,ncol=length(id.ARP)*(T0+B*Tt))),length(id.ARP),T0+B*Tt)
Windv.EnSD=matrix(colSds(matrix(Windv.ARP,nrow=R,ncol=length(id.ARP)*(T0+B*Tt))),length(id.ARP),T0+B*Tt)
GenDu.EnSD=matrix(colSds(matrix(Gen.Dat.U.online,nrow=R,ncol=length(id.ARP)*(T0+B*Tt))),length(id.ARP),T0+B*Tt)
GenDv.EnSD=matrix(colSds(matrix(Gen.Dat.V.online,nrow=R,ncol=length(id.ARP)*(T0+B*Tt))),length(id.ARP),T0+B*Tt)
Isdd.online.1=c(apply(sqrt((Windu.EnSD-GenDu.EnSD)^2),1,mean),
            apply(sqrt((Windv.EnSD-GenDv.EnSD)^2),1,mean))
write.csv(Isdd.online.1,here("Case_Studies/Scenario1_OSG_Long","Isdd_online_1.csv"))


### Calculate Iqd^0.75 and Iqd^0.25 for 0.75-th and 0.25-th quantile shifts
Windu.En75=matrix(colQuantiles(matrix(Windu.ARP,nrow=R,ncol=length(id.ARP)*(T0+B*Tt)),probs=0.75),length(id.ARP),T0+B*Tt)
Windv.En75=matrix(colQuantiles(matrix(Windv.ARP,nrow=R,ncol=length(id.ARP)*(T0+B*Tt)),probs=0.75),length(id.ARP),T0+B*Tt)
GenDu.En75=matrix(colQuantiles(matrix(Gen.Dat.U.online,nrow=R,ncol=length(id.ARP)*(T0+B*Tt)),probs=0.75),length(id.ARP),T0+B*Tt)
GenDv.En75=matrix(colQuantiles(matrix(Gen.Dat.V.online,nrow=R,ncol=length(id.ARP)*(T0+B*Tt)),probs=0.75),length(id.ARP),T0+B*Tt)
I75qd.online.1=c(apply(sqrt((Windu.En75-GenDu.En75)^2),1,mean),apply(sqrt((Windv.En75-GenDv.En75)^2),1,mean))
write.csv(I75qd.online.1,here("Case_Studies/Scenario1_OSG_Long","I75qd_online_1.csv"))

Windu.En25=matrix(colQuantiles(matrix(Windu.ARP,nrow=R,ncol=length(id.ARP)*(T0+B*Tt)),probs=0.25),length(id.ARP),T0+B*Tt)
Windv.En25=matrix(colQuantiles(matrix(Windv.ARP,nrow=R,ncol=length(id.ARP)*(T0+B*Tt)),probs=0.25),length(id.ARP),T0+B*Tt)
GenDu.En25=matrix(colQuantiles(matrix(Gen.Dat.U.online,nrow=R,ncol=length(id.ARP)*(T0+B*Tt)),probs=0.25),length(id.ARP),T0+B*Tt)
GenDv.En25=matrix(colQuantiles(matrix(Gen.Dat.V.online,nrow=R,ncol=length(id.ARP)*(T0+B*Tt)),probs=0.25),length(id.ARP),T0+B*Tt)
I25qd.online.1=c(apply(sqrt((Windu.En25-GenDu.En25)^2),1,mean),apply(sqrt((Windv.En25-GenDv.En25)^2),1,mean))
write.csv(I25qd.online.1,here("Case_Studies/Scenario1_OSG_Long","I25qd_online_1.csv"))






