################  Wrapper file for sequentially implementing each figure and table  ################
# This file outlines the reproducibility workflow of the article. 
# The computation time reported below was recorded using (R 3.6.3) running on machine equipped with Intel(R) Xeon(R) CPU E5-2680 v4 @ 2.40GHz and 125 GB RAM.

# Before running the code, please complete the following steps:
# 1. Download the entire repository as "OSG_Reproducibility_Materials.zip", 
#    extract it as a folder named "OSG_Reproducibility_Materials", 
#    and set your working directory to this folder. 
# 2. Download the ERA5 ensembles from https://zenodo.org/records/16618043,
#    and save them in the sub-directory "OSG_Reproducibility_Materials/Data".
# 3. Download "Slepian_ARP.zip" file from https://zenodo.org/records/16655516, 
#    extract all .m files inside, and save them in the sub-repository "OSG_Reproducibility_Materials/Slepian_ARP".



###### Load necessary R packages and functions
library(here)      # ! Please load this package after setting the working directory to folder "OSG_Reproducibility_Materials".
library(ncdf4)
library(sp)
library(ggplot2)
library(maps)
library(moments)
library(R.matlab)
library(LambertW)
library(fdaoutlier)
library(matrixStats)
library(approxOT)
library(patchwork)
library(scales)
library(dplyr)
library(reticulate)
source(here("Functions","InverseTH.R"))
source(here("Functions","Assessment_Indices.R"))
Col=c("#3288BD","#66C2A5","#ABDDA4","#E6F598","#FEE08B","#FDAE61","#F46D43","#D53E4F")
cbPalette=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")




######   Process the data   #############################################################################
# Code and detailed computational time can be found in "Data/Data_Treatment.R".                         #
#########################################################################################################
source(here("Data","Data_Treatment.R"))
# Computational time: 87.578 seconds



######   Figure 1 in Section 2   ########################################################################
# Figure 1 demonstrates the u- and v-component wind speed ERA5 ensembles.                               #
# Code and detailed computational time can be found in "Data_Demontration/Data_Demontration.R".         #
#########################################################################################################
source(here("Data_Demonstration","Data_Demonstration.R"))
# Computational time: 1589.505 seconds



######   Figure 2 in Section 3.1   ######################################################################
# Figure 2 demonstrates the Slepian concentration problem for ARP.                                      #
# Code and detailed computational time can be found in "Slepian_ARP/Slepian_Demonstration.R".           #
#########################################################################################################
source(here("Slepian_ARP","Slepian_Demonstration.R"))
# Computational time: 2.78 seconds



######   Figure 3 in Section 3.1   ######################################################################
# Figure 3 demonstrates the performance of Slepian bases.                                               #
# Code and detailed computational time can be found in "Slepian_ARP/Slepian_Performance.R".             #
#########################################################################################################
source(here("Slepian_ARP","Slepian_Performance.R"))
# Computational time: 296.714 seconds



######   Figures 4-5 in Section 4.2 (and Figure S8 and Table S3 in Section S4.2)   ######################
# Figures 4-5 and S8 demonstrate the estimates and updates of parameters in OSG-Long.                   # 
# Table S3 shows relative Frobenius distances (RFDs) between estimates in OSG-Long and those in FSG.    #
# Detailed Code and computational time for FSG's construction, emulation, and assessment can be found   #
# in "Case_Studies/FSG/FSG.R".                                                                          # 
# Detailed code and computational time for OSG-Long's construction, emulation, and assessment can be    #         #
# found in "Case_Studies/Scenario1_OSG_Long/Scenario1_OSG_Long.R".                                      # 
# Code for ploting figures can be found in "Case_Studies/Scenario1_OSG_Long/Plot_estimates_OSG_Long.R". # 
#########################################################################################################
# First, construct the FSG, use it to generate emulations, evaluate its emulation performance,
#        and save required values for ploting figures in the sub-repository "Case_Studies/FSG".
source(here("Case_Studies/FSG","FSG.R"))
# Computational time for FSG construction: 1637.935 seconds (averaged value over 10 replicates)
# Computational time for FSG emulation:    784.4847 seconds (averaged value over 10 replicates)
# Computational time for FSG evaluation:   16896.28 seconds
# Total computational time: about 5.37 hours


# Second, construct the OSG-Long, use it to generate emulations, evaluate its emulation performance,
#        and save required values for ploting figures in the sub-repository "Case_Studies/Scenario1_OSG_Long".
source(here("Case_Studies/Scenario1_OSG_Long","Scenario1_OSG_Long.R"))
# Computational time for OSG-Long construction: 1654.414 seconds (averaged value over 10 replicates)
# Computational time for OSG-Long emulation:    see that of FSG
# Computational time for OSG-Long evaluation:   see that of FSG
# Total computational time: about 5.37 hours 


# Finally, reproduce Figures 4-5, Figure S8 and Table S3
source(here("Case_Studies/Scenario1_OSG_Long","Plot_estimates_OSG_Long.R"))
# Computational time: 12.559 seconds



######   Figures 7-8 in Section 4.3 (and Figures S10-11 in Section S4.3)   ##############################
# Figures 7-8 and S10-11 demonstrate the estimates and updates of parameters in OSG-Short.              #                                           # 
# Detailed code and computational time for OSG-Short's construction, emulation, and assessment can be   #
# found in "Case_Studies/Scenario2_OSG_Short/Scenario2_OSG_Short.R".                                    # 
# Code for ploting figures can be found in "Case_Studies/Scenario2_OSG_Short/Plot_estimates_OSG_Short.R".# 
#########################################################################################################
# First, construct the OSG-Short, use it to generate emulations, evaluate its emulation performance,
#        and save required values for ploting figures in the sub-repository "Case_Studies/Scenario2_OSG_Short".
source(here("Case_Studies/Scenario2_OSG_Short","Scenario2_OSG_Short.R"))
# Computational time for OSG-Short construction: 2152.603 seconds (averaged value over 10 replicates)
# Computational time for OSG-Short emulation:    see that of FSG
# Computational time for OSG-Short evaluation:   see that of FSG
# Total computational time: about 5.51 hours 


# Then, reproduce Figures 7-8 and S10-11
source(here("Case_Studies/Scenario2_OSG_Short","Plot_estimates_OSG_Short.R"))
# Computational time: 15.631 seconds



######   Figure 6 in Section 4.2, Figure 9 in Section 4.3   #############################################
######   (and Figure S9 in Section S4.2 and Figures S12-13 in Section S4.3)   ###########################
# Figures 6 and S9 demonstrate the emulation performance of OSG-Long.                                   # 
# Figures 9 and S12 demonstrate the emulation performance of OSG-Short.                                 #    
# Figure S13 compares emulation performances of FSG, OSG-Long, and OSG-Short using boxplots.            #  
# Code for ploting figures can be found in "Case_Studies/Plot_emulation_performance.R".                 # 
#########################################################################################################
source(here("Case_Studies","Plot_emulation_performance.R"))
# Computational time: 30.074 seconds



######   Table 2 in Section 4.3 (and Figure S7 in Section S4.1)   #######################################
# Table 2 demonstrates the storage demands of FSG, OSG-Long, and OSG-Short's construction.              #
# Figure S7 compares storage of training data and SG parameters.                                        #
# Code can be found in "Case_Studies/Storage.R".                                                        #
#########################################################################################################
source(here("Case_Studies","Storage.R"))
# Computational time: 1.309 seconds



######   (Figures S2 in Section S3.1)   ################################################################
# Figure S2 shows the empirical directional variogram of random effects.                               #
# Code can be found in "Data_Demonstration/Variogram.R".                                               #
########################################################################################################
source(here("Data_Demonstration","Variogram.R"))
# Computational time: 9.344 seconds (by MacBook Pro 14 with an Apple M1 Pro chip and 16 GB of memory)



######   (Figures S3 in Section S3.2)   ################################################################
# Figure S3 provides a sensitivity analysis of Slepian concentration level threshold \lambda_A.        #
# Code can be found in "Slepian_ARP/Threshold.R".                                                      #
# ######################################################################################################
source(here("Slepian_ARP","Threshold.R"))
# Computational time:



######   (Figures S4-5 in Section S4.1)   ###############################################################
# Figures S4-5 help to select the number of Slepian bases A.                                            #
# Detailed code and computational time can be found in "Case_Studies/Tuning_Parameters/ChooseA.R".      #
#########################################################################################################
source(here("Case_Studies/Tuning_Parameters","ChooseA.R"))
# Computational time: 5998.518 seconds



######   (Figure S6 in Section S4.1)   ##################################################################
# Figure S6 helps to select the order P of VAR model.                                                   #
# Detailed code and computational time can be found in "Case_Studies/Tuning_Parameters/ChooseP.R".      #
#########################################################################################################
source(here("Case_Studies/Tuning_Parameters","ChooseP.R"))
# Computational time: 4519.506 seconds



######   (Figures S14 in Section S4.4)   ################################################################
# Figure S14 illustrates Monte Carlo assessment of emulation variability.                               #
# Code can be found in "Case_Studies/MC/MC_FSG.R", "Case_Studies/MC/MC_OSG_Long.R", and                 #
# "Case_Studies/MC/MC_OSG_Short.R". Figure S14 can be reproduced by "Case_Studies/MC/MC_Plot.R".        #                                                      #
# #######################################################################################################
# Please run the following three files nine times to obtain additional emulation results by changing seeds.
source(here("Case_Studies/MC","MC_FSG.R")) 
source(here("Case_Studies/MC","MC_OSG_Long.R"))
source(here("Case_Studies/MC","MC_OSG_Short.R"))
# Computational time: about 1 hour 20 mins for running each file each time


# Then, reproduce Figure S14
source(here("Case_Studies/MC","MC_Plot.R"))



######   (Figure S15 in Section S5)   ##################################################################
# Figure S15 demonstrates the performance of VAE trained by initial data block from the first month,   #
# the first year, and the first nine years, respectively.                                              #
# Detailed code can be found in "VAE/VAE_Performance.R".                                               #
########################################################################################################
source(here("VAE","VAE_Performance.R"))
