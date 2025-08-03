################################################################################
# This file includes all steps to reproduce Table 2 and Figure S5              #
################################################################################
# Necessary packages
# library(ggplot2)

# Necessary information
R=10                  # Number of ensembles 
Q.sl=300              # Number of Slepian bases   
loclen=length(id.ARP) # Number of grid points
P=2                   # Order of VAR model


###### Part 1. Reproduce Table 2 using Tables S1 and S2
# Calculate C1(t),..., C4(t) in Table S1 and transfer them to GB
C1=function(t){return((2+Q.sl*loclen+2*(R+1)*loclen*t)*8/1e9)}
C2=function(t){return((2+2*Q.sl*loclen+2*(loclen+R*Q.sl+R*loclen)*t)*8/1e9)}
C3=function(t){return((2+8*Q.sl+2*R*Q.sl*t)*8/1e9)}
C4=function(t){return((2+4*(P+1)*(P+1)*300^2-2*P*(P+1)*R*Q.sl+2*(P+2)*R*Q.sl*t)*8/1e9)}

# Calculate storage for FSG
print("Storage of FSG")
print(C1(10*365*8))
print(C2(10*365*8))
print(C3(10*365*8))
print(C1(10*365*8))

# Calculate storage for OSG-Long 
C.cp=(4*Q.sl+4*P*Q.sl^2+4*Q.sl^2+4*P*P*Q.sl^2)*8/1e9
print("Storage for OSG-Long")
print(C.cp+C1(365*8))
print(C.cp+C2(365*8))
print(C.cp+(Q.sl*loclen+2*Q.sl)*8/1e9+C3(365*8))
print(C.cp+(Q.sl*loclen+4*(P+1)*Q.sl^2)*8/1e9+C4(365*8))

# Calculate storage for OSG-Short
print("Storage for OSG-Short")
print(C.cp+C1(31*8))
print(C.cp+C2(31*8))
print(C.cp+(Q.sl*loclen+2*Q.sl)*8/1e9+C3(31*8))
print(C.cp+(Q.sl*loclen+4*(P+1)*Q.sl^2)*8/1e9+C4(31*8))



###### Part 2. Reproduce Figure S5
# Storage of training data with R ensembles and TT time points
RAD.str=function(R,TT){return(2*R*loclen*TT*8/1e9)}
# Storage of SGs with TT time points
SG.str=function(TT){return((4*loclen*TT+4*Q.sl+4*(P+1)*Q.sl*Q.sl)*8/1e9)}

# Plot Figure S5(a)
TTseq=(1:80)*365*8
dataF=data.frame(Time=rep(1:80,times=2),
                 Storage=c(sapply(TTseq,function(TT){return(RAD.str(10,TT))}),
                           sapply(TTseq, SG.str)),
                 Type=as.factor(rep(c("ERA5 ensembles","FSG or OSG"),each=length(TTseq))))
PT=ggplot()+
  geom_point(aes(x=Time,y=Storage,color=Type),data=dataF)+
  geom_line(aes(x=Time,y=Storage,color=Type),data=dataF)+
  scale_color_manual(values=c("#0072B2","#E41A1C"))+
  xlab("Number of years T/365/8")+ylab("Storage (GB)")+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=14),
                   legend.key = element_blank(),
                   legend.justification = c(0,1),
                   legend.position = c(0.01,0.98),
                   legend.title = element_blank(),
                   legend.key.width=unit(1,"line"),
                   legend.text = element_text(size=12),
                   legend.key.height=unit(1.3,"line"))
print(PT)          # 4.83*2.63

# Plot Figure S5(b)
Rseq=2:100
dataF=data.frame(Time=rep(Rseq,times=2),
                 Storage=c(sapply(Rseq,function(R){return(RAD.str(R,10*365*8))}),
                           rep(SG.str(10*365*8),length(Rseq))),
                 Type=as.factor(rep(c("ERA5 ensembles","FSG or OSG"),each=length(Rseq))))
PT=ggplot()+
  geom_point(aes(x=Time,y=Storage,color=Type),data=dataF)+
  geom_line(aes(x=Time,y=Storage,color=Type),data=dataF)+
  scale_color_manual(values=c("#0072B2","#E41A1C"))+
  xlab("Number of ensembles R")+ylab("Storage (GB)")+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(colour = "black"),
                   axis.text=element_text(size=12),
                   axis.title = element_text(size=14),
                   legend.key = element_blank(),
                   legend.justification = c(0,1),
                   legend.position = "none",
                   legend.title = element_blank(),
                   legend.key.width=unit(1,"line"),
                   legend.key.height=unit(1.3,"line"))
print(PT)           # 4.83*2.63

