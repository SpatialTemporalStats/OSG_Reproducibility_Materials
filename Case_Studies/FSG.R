################################################################################################
# FSG (an SG directly developed from the full data): Construction, Emulation, and Evaluation   #
################################################################################################
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


# FSG information
R=10            # Number of ensembles
TT=10*365*8     # Number of time points



####### Part I. Develop the FSG using Algorithm~S1 #########
### Step 1. Get m_t(L_i,l_j) and detrend
# t1=proc.time()[[3]]
Windu.EnMean=apply(Windu.ARP,c(2,3),mean)                      # Obtain m_{*,t}(L_i,l_j)
Windv.EnMean=apply(Windv.ARP,c(2,3),mean)
Windu.rsd=Windv.rsd=array(0,c(R,length(id.ARP),TT))            # Detrend to get random effects z_{*,t}^{(r)}(L_i,l_j)
for(r in 1:R){
  Windu.rsd[r,,]=Windu.ARP[r,,]-Windu.EnMean
  Windv.rsd[r,,]=Windv.ARP[r,,]-Windv.EnMean
}
# t2=proc.time()[[3]]
# t2-t1=1500.172

### Step 2. Translate the data to the Slepian domain and obtain \sigma_t(L_i,l_j)
# t1=proc.time()[[3]]
A=solve(t(Basis.SLP[id.ARP,1:Q.sl])%*%Basis.SLP[id.ARP,1:Q.sl],t(Basis.SLP[id.ARP,1:Q.sl]))
Windu.SLP=Windv.SLP=array(0,c(R,Q.sl,TT))                # Obtain Slepian coefficients s_{*,t}^{(r)}(\alpha)
for(r in 1:R){
  Windu.SLP[r,,]=A%*%Windu.rsd[r,,]
  Windv.SLP[r,,]=A%*%Windv.rsd[r,,]
}
v2hat.u=v2hat.v=matrix(0,length(id.ARP),TT)              # Obtain the variance of the residuals, i.e., \sigma_{*,t}^2(L_i,l_j)
for(r in 1:R){
  v2hat.u=v2hat.u+(Basis.SLP[id.ARP,1:Q.sl]%*%Windu.SLP[r,,]-Windu.rsd[r,,])^2
  v2hat.v=v2hat.v+(Basis.SLP[id.ARP,1:Q.sl]%*%Windv.SLP[r,,]-Windv.rsd[r,,])^2
}
v2hat.u=v2hat.u/R
v2hat.v=v2hat.v/R
# t2=proc.time()[[3]]
# t2-t1=51.369

### Step 3. Tukey h transformation with Lambert function
# t1=proc.time()[[3]]
Gamma.u.full=Gamma.v.full=matrix(0,Q.sl,2)                    # Store \hat{\gamma}_{*,\alpha} and \hat{\kappa}_{*,\alpha}
TGHpara.u.full=TGHpara.v.full=matrix(0,Q.sl,2)                # Store \hat{\omega}_{*,\alpha} and \hat{h}_{*,\alpha} 
Windu.SLP.TGH=Windv.SLP.TGH=array(0,c(R,Q.sl,TT))             # Store Gaussianzed coefficients \tilde{s}_{*,t}^{(r)}(\alpha)
for(i in 1:Q.sl){
  Gamma.u.full[i,1]=mean((Windu.SLP[,i,])^2)                                            # Estimate \gamma_{*,\alpha} and \kappa_{*,\alpha} by Eq.~(7)
  Gamma.v.full[i,1]=mean((Windv.SLP[,i,])^2)
  Gamma.u.full[i,2]=max(mean((Windu.SLP[,i,])^4)/(Gamma.u.full[i,1])^2,162/66)
  Gamma.v.full[i,2]=max(mean((Windv.SLP[,i,])^4)/(Gamma.v.full[i,1])^2,162/66)
  
  TGHpara.u.full[i,2]=max(sqrt(66*Gamma.u.full[i,2]-162)-6,0)/66                        # Estimate \omega_{*,\alpha} and h_{*,\alpha} by Eq.~(6)
  TGHpara.v.full[i,2]=max(sqrt(66*Gamma.v.full[i,2]-162)-6,0)/66
  TGHpara.u.full[i,1]=sqrt(Gamma.u.full[i,1])*((1-2*TGHpara.u.full[i,2])^(3/4))
  TGHpara.v.full[i,1]=sqrt(Gamma.v.full[i,1])*((1-2*TGHpara.v.full[i,2])^(3/4))
  
  Windu.SLP.TGH[,i,]=THinvfunc(Windu.SLP[,i,],TGHpara.u.full[i,1],TGHpara.u.full[i,2])  # Gaussianze Slepian coefficients
  Windv.SLP.TGH[,i,]=THinvfunc(Windv.SLP[,i,],TGHpara.v.full[i,1],TGHpara.v.full[i,2])
}
write.csv(TGHpara.u.full,here("Case_Studies/FSG","TGHparaufull.csv"))                   # Save \hat{\omega}_{*,\alpha} and \hat{h}_{*,\alpha} for future use
write.csv(TGHpara.v.full,here("Case_Studies/FSG","TGHparavfull.csv"))
# t2=proc.time()[[3]]
# t2-t1=36.979

### Step 4. VAR(2) model
# t1=proc.time()[[3]]
P=2                                                   # Order of VAR model
YY=matrix(0,R*(TT-P),2*Q.sl)
for(i in 1:Q.sl){
  YY[,i]=c(t(Windu.SLP.TGH[,i,(P+1):TT]))
  YY[,i+Q.sl]=c(t(Windv.SLP.TGH[,i,(P+1):TT]))
}
YX=matrix(0,R*(TT-P),2*Q.sl*P)
for(p in 1:P){
  for(i in 1:Q.sl){
    YX[,(p-1)*2*Q.sl+i]=c(t(Windu.SLP.TGH[,i,(P+1-p):(TT-p)]))
    YX[,(p-1)*2*Q.sl+i+Q.sl]=c(t(Windv.SLP.TGH[,i,(P+1-p):(TT-p)]))
  }
}
Phi.hat.full=solve(t(YX)%*%YX,t(YX)%*%YY)             # Estimate \Phi by (9)
K.full=crossprod(YY-YX%*%Phi.hat.full)/R/(TT-P)  # Estimate K by (10)
writeMat(here("Case_Studies/FSG","Phihatfull.mat"),Phihatfull=Phi.hat.full)
writeMat(here("Case_Studies/FSG","Kfull.mat"),Kfull=K.full)
# t2=proc.time()[[3]]
# t2-t1=35.795



####### Part II. Generate emulations using FSG by Algorithm~S2 #########
# t1=proc.time()[[3]]
LL=t(chol(K.full))                                # Cholesky decomposition of K
EPS.u=EPS.v=array(0,c(R,length(id.ARP),TT))       # Generate residual \epsilon_{*,t}^{(r)}(L_i,l_j)
for(i in 1:length(id.ARP)){
  for(t in 1:TT){
    set.seed(t+(i-1)*TT)
    EPS.u[,i,t]=rnorm(R,mean=0,sd=sqrt(v2hat.u[i,t]))
    EPS.v[,i,t]=rnorm(R,mean=0,sd=sqrt(v2hat.v[i,t]))
  }
}
Gen.Dat.U.full=Gen.Dat.V.full=array(0,c(R,length(id.ARP),TT))  # Store emulations \hat{y}_{*,t}^{(r)}(L_i,l_j)
for(r in 1:R){
  # Step 1. Generate transformed coefficients
  Z=matrix(0,2*Q.sl,TT+P)
  for(t in 1:(TT+P)){
    set.seed(t+(r-1)*(TT+P))
    Z[,t]=rnorm(2*Q.sl)
  }
  Z[,1:P]=LL%*%Z[,1:P]
  for(t in (P+1):(TT+P)){
    Z[,t]=LL%*%Z[,t]+t(c(Z[,t-1],Z[,t-2])%*%Phi.hat.full)
  }
  Z=Z[,-(1:P)]
  
  # Step2. Inverse Tukey h
  for(i in 1:Q.sl){
    taughu=function(z){return(TGHpara.u.full[i,1]*z*exp(TGHpara.u.full[i,2]*z^2/2))}
    Z[i,]=taughu(Z[i,])
    taughv=function(z){return(TGHpara.v.full[i,1]*z*exp(TGHpara.v.full[i,2]*z^2/2))}
    Z[Q.sl+i,]=taughv(Z[Q.sl+i,])
  }
  
  # Step 3. Inverse Slepian
  Gen.Dat.U.full[r,,]=Basis.SLP[id.ARP,1:Q.sl]%*%Z[1:Q.sl,]
  Gen.Dat.V.full[r,,]=Basis.SLP[id.ARP,1:Q.sl]%*%Z[-(1:Q.sl),]
  
  # Step 4. Add mean and residuals
  Gen.Dat.U.full[r,,]=Windu.EnMean+Gen.Dat.U.full[r,,]+EPS.u[r,,]
  Gen.Dat.V.full[r,,]=Windv.EnMean+Gen.Dat.V.full[r,,]+EPS.v[r,,]
}
# t2=proc.time()[[3]]
# t2-t1=782.412



####### Part III. Evaluate emulation performance of FSG #########
### Calculate Iuq for Uncertainty 
# t1=proc.time()[[3]]
Iuq.u.full=get.IUQ(Gen.Dat.U.full,Windu.ARP)
Iuq.v.full=get.IUQ(Gen.Dat.V.full,Windv.ARP)
write.csv(Iuq.u.full,here("Case_Studies/FSG","Iuq_u_full.csv"))
write.csv(Iuq.v.full,here("Case_Studies/FSG","Iuq_v_full.csv"))
# t2=proc.time()[[3]]
# t2-t1=2320.68


### Prepare for calculating Ibc by calculating bivariate correlations
# t1=proc.time()[[3]]
GenDu.EnMean=matrix(colMeans2(matrix(Gen.Dat.U.full,nrow=R,ncol=length(id.ARP)*TT)),length(id.ARP),TT)   # Use colMeans2 to calculate ensemble mean.
GenDv.EnMean=matrix(colMeans2(matrix(Gen.Dat.V.full,nrow=R,ncol=length(id.ARP)*TT)),length(id.ARP),TT)   # It makes computation much faster but takes more storage
Genu.rsd=Genv.rsd=array(0,c(R,length(id.ARP),TT))
for(r in 1:R){
  Genu.rsd[r,,]=Gen.Dat.U.full[r,,]-GenDu.EnMean
  Genv.rsd[r,,]=Gen.Dat.V.full[r,,]-GenDv.EnMean
}
Ibc.full=get.IBC(Genu.rsd,Genv.rsd,Windu.rsd,Windv.rsd)
write.csv(Ibc.full,here("Case_Studies/FSG","Ibc_full.csv"))
# t2=proc.time()[[3]]
# t2-t1=114.924


### Prepare for calculating Itc1 and Itc2 by calculating temporal correlations at lag=1 and 2
# t1=proc.time()[[3]]
Itc1.u.full=get.ITC1(Genu.rsd,Windu.rsd)
Itc1.v.full=get.ITC1(Genv.rsd,Windv.rsd)
Itc2.u.full=get.ITC2(Genu.rsd,Windu.rsd)
Itc2.v.full=get.ITC2(Genv.rsd,Windv.rsd)
write.csv(Itc1.u.full,here("Case_Studies/FSG","Itc1_u_full.csv"))
write.csv(Itc1.v.full,here("Case_Studies/FSG","Itc1_v_full.csv"))
write.csv(Itc2.u.full,here("Case_Studies/FSG","Itc2_u_full.csv"))
write.csv(Itc2.v.full,here("Case_Studies/FSG","Itc2_v_full.csv"))
# t2=proc.time()[[3]]
# t2-t1=185.782


### Prepare for calculating Itp by calculating the NCPGT
# t1=proc.time()[[3]]
NCPGT.dat=get.ITP(Windu.ARP,Windv.ARP)
NCPGT.emu=get.ITP(Gen.Dat.U.full,Gen.Dat.V.full)
write.csv(cbind(NCPGT.emu,NCPGT.dat),here("Case_Studies/FSG","Itp_full.csv"))
# t2=proc.time()[[3]]
# t2-t1=8735.004


### Calculate Iwdt and Iwds for marginal distribution
# t1=proc.time()[[3]]
Iwdt.u.full=get.IWDT(Gen.Dat.U.full,Windu.ARP)
Iwdt.v.full=get.IWDT(Gen.Dat.V.full,Windv.ARP)
Iwds.u.full=get.IWDS(Gen.Dat.U.full,Windu.ARP)
Iwds.v.full=get.IWDS(Gen.Dat.V.full,Windv.ARP)
write.csv(Iwdt.u.full,here("Case_Studies/FSG","Iwdt_u_full.csv"))
write.csv(Iwdt.v.full,here("Case_Studies/FSG","Iwdt_v_full.csv"))
write.csv(Iwds.u.full,here("Case_Studies/FSG","Iwds_u_full.csv"))
write.csv(Iwds.v.full,here("Case_Studies/FSG","Iwds_v_full.csv"))
# t2=proc.time()[[3]]
# t2-t1=916.057


### Calculate Imd for mean shift
# t1=proc.time()[[3]]
Imd.full=c(apply(sqrt((Windu.EnMean-GenDu.EnMean)^2),1,mean),
           apply(sqrt((Windv.EnMean-GenDv.EnMean)^2),1,mean))
write.csv(Imd.full,here("Case_Studies/FSG","Imd_full.csv"))
# t2=proc.time()[[3]]
# t2-t1=2.516


### Calculate Isdd for SD shift
# t1=proc.time()[[3]]
Windu.EnSD=matrix(colSds(matrix(Windu.ARP,nrow=R,ncol=length(id.ARP)*TT)),length(id.ARP),TT)
Windv.EnSD=matrix(colSds(matrix(Windv.ARP,nrow=R,ncol=length(id.ARP)*TT)),length(id.ARP),TT)
GenDu.EnSD=matrix(colSds(matrix(Gen.Dat.U.full,nrow=R,ncol=length(id.ARP)*TT)),length(id.ARP),TT)
GenDv.EnSD=matrix(colSds(matrix(Gen.Dat.V.full,nrow=R,ncol=length(id.ARP)*TT)),length(id.ARP),TT)
Isdd.full=c(apply(sqrt((Windu.EnSD-GenDu.EnSD)^2),1,mean),
            apply(sqrt((Windv.EnSD-GenDv.EnSD)^2),1,mean))
write.csv(Isdd.full,here("Case_Studies/FSG","Isdd_full.csv"))
# t2=proc.time()[[3]]
# t2-t1=21.637


### Calculate Iqd^0.75 and Iqd^0.25 for 0.75-th and 0.25-th quantile shifts
# t1=proc.time()[[3]]
Windu.En75=matrix(colQuantiles(matrix(Windu.ARP,nrow=R,ncol=length(id.ARP)*TT),probs=0.75),length(id.ARP),TT)
Windv.En75=matrix(colQuantiles(matrix(Windv.ARP,nrow=R,ncol=length(id.ARP)*TT),probs=0.75),length(id.ARP),TT)
GenDu.En75=matrix(colQuantiles(matrix(Gen.Dat.U.full,nrow=R,ncol=length(id.ARP)*TT),probs=0.75),length(id.ARP),TT)
GenDv.En75=matrix(colQuantiles(matrix(Gen.Dat.V.full,nrow=R,ncol=length(id.ARP)*TT),probs=0.75),length(id.ARP),TT)
I75qd.full=c(apply(sqrt((Windu.En75-GenDu.En75)^2),1,mean),apply(sqrt((Windv.En75-GenDv.En75)^2),1,mean))
write.csv(I75qd.full,here("Case_Studies/FSG","I75qd_full.csv"))
# t2=proc.time()[[3]]
# t2-t1=2356.691

# t1=proc.time()[[3]]
Windu.En25=matrix(colQuantiles(matrix(Windu.ARP,nrow=R,ncol=length(id.ARP)*TT),probs=0.25),length(id.ARP),TT)
Windv.En25=matrix(colQuantiles(matrix(Windv.ARP,nrow=R,ncol=length(id.ARP)*TT),probs=0.25),length(id.ARP),TT)
GenDu.En25=matrix(colQuantiles(matrix(Gen.Dat.U.full,nrow=R,ncol=length(id.ARP)*TT),probs=0.25),length(id.ARP),TT)
GenDv.En25=matrix(colQuantiles(matrix(Gen.Dat.V.full,nrow=R,ncol=length(id.ARP)*TT),probs=0.25),length(id.ARP),TT)
I25qd.full=c(apply(sqrt((Windu.En25-GenDu.En25)^2),1,mean),apply(sqrt((Windv.En25-GenDv.En25)^2),1,mean))
write.csv(I25qd.full,here("Case_Studies/FSG","I25qd_full.csv"))
# t2=proc.time()[[3]]
# t2-t1=2242.986


