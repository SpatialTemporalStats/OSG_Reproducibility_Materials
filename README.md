# Reproducibility Instrucion for "Online stochastic generators using Slepian bases for regional bivariate wind speed ensembles from ERA5"
This file documents the artifacts associated with the article (i.e., the data and code supporting the computational findings) and describes how to reproduce all figures and results.

## Article Overview
As essential resources for studying climate change and forecasting weather, reanalysis data provide detailed and comprehensive descriptions of the Earth’s climate system. However, their generation, storage, and management are resource-intensive. For 3-hourly bivariate wind speed ensembles from the fifth generation European Centre for Medium-Range Weather Forecasts Reanalysis [(ERA5)](https://cds.climate.copernicus.eu/datasets/reanalysis-era5-single-levels?tab=overview), which face these challenges, this paper proposes an online stochastic generator (OSG). It serves as a practical surrogate to the reanalysis generation and storage, producing fast stochastic approximations while requiring storage of only a limited number of parameters. The OSG introduces two key innovations: 1) the integration of Slepian concentration, enabling efficient modeling and emulation of complex multivariate climate ensembles over any region on the globe, and 2) the incorporation of an online updating strategy, allowing data to enter the model sequentially in time blocks to update parameters, thereby reducing storage demands during construction and supporting near real-time emulation. In our case study, two OSGs are developed specifically for the Arabian-Peninsula region, using long and short blocks to accommodate different needs. Shorter blocks allow for more frequent updates and greater storage savings during construction. Emulations generated by both OSGs closely resemble the training data ensembles, as comprehensively evaluated using various indices. This work offers a promising complement to reanalysis, operable on desktop computers and supporting flexible, efficient emulations.

## Contents
#### "Wrapper.R"
This file outlines the reproducibility workflow of the article.  

#### Functions
This sub-repository contains R scripts for several frequently used functions.
* "Assessment_Indices.R": functions for calculating assessment indices in Table 1
* "InverseTH.R": function for performing the inverse Tukey h transformation

#### Data
This sub-repository is used to store the downloaded ERA5 ensemble data and to provide detailed instructions for processing them.
* "Data_Treatment.R": R script for processing the downloaded data
* "IPCC-WGI-reference-regions-v4_R.rda": an R data file for identifying the Arabian Pennisula region (ARP), provided by [Iturbide et al. (2020)](https://essd.copernicus.org/articles/12/2959/2020/essd-12-2959-2020-assets.html)
>*"WindSpeeduv_ARP_2014.nc" -- "WindSpeeduv_ARP_2023.nc": downloaded ERA5 ensemble data used in this work

#### Data_Demonstration
This sub-repository provides the R script for reproducing Figure 1, which demonstrates several statistical characteristics of the u- and v-component wind speed ERA5 ensembles.
* "Data_Demonstration.R": R script for reproducing Figure 1

#### Slepian_ARP
This sub-repository provides the R scripts for reproducing Figures 2 and 3, which demonstrates Slepian bases in the Slepian concentration problem and their performance. Additionally, this sub-repository is used to store the downloaded Slepian bases.
* "Slepian_Demonstration.R": R script for reproducing Figure 2
* "Slepian_Performance.R": R script for reproducing Figure 3
>*"Basis_reg_eig_value_181_real.mat" and others: downloaded eigenvalues in the Slepian concentration problem
>*"Slepian_spatial_181.mat" and others: downloaded Slepian bases in the Slepian concentration problem

#### Case_Studies
This sub-repository contains code for implementing the case studies (Sections 4.2 and 4.3, and Sections S4.2 and S4.3) and reproducing all associated figures and tables.
* "**FSG**": sub-repository for the stochastic generator directly derived from the full data (FSG)
  * "FSG.R": R script for the FSG construction, emulation, and evaluation
  >*"TGHparaufull.csv", "TGHparavfull.csv", "Phihatfull.mat", "Kfull.mat", "Iuq_u_full.csv", "Iuq_v_full.csv", "Ibc_full.csv", "Itc1_u_full.csv", "Itc1_v_full.csv", "Itc2_u_full.csv", "Itc2_v_full.csv", "Itp_full.csv", "Iwdt_u_full.csv", "Iwdt_v_full.csv", "Iwds_u_full.csv", "Iwds_v_full.csv", "Imd_full.csv", "Isdd_full.csv", "I75qd_full.csv", "I25qd_full.csv": outputs of "FSG.R" used to plot figures


## Data
### Abstract
ERA5 is the fifth-generation ECMWF reanalysis, providing comprehensive and consistent descriptions of Earth’s climate and weather across atmospheric, land, and oceanic variables. The ERA5 ensemble consists of 10 members at a horizontal spatial resolution of 0.5 degree and a temporal resolution of 3 hours, representing ERA5 uncertainty through perturbed observations and model physics. This work uses 10m u- and v-component wind speed ERA5 ensembles over the Arabian Peninsula for the period 2014–2023, which were downloaded in 2024.

### Availability 
The data used in this work are in netCDF format and are available online at: https://zenodo.org/records/16618043. 

### Description
ERA5 data and ERA5 ensembles are continuously updated by incorporating new data and revising historical data to enhance their quality. Therefore, the ERA5 ensembles used in this work, downloaded in 2024, differ slightly from those currently available on the Climate Data Store (CDS) website. Although these differences are minimal, we recommend using the [2024 download](https://zenodo.org/records/16618043) to reproduce the results.

Alternatively, readers may download the latest version from the [CDS download page](https://cds.climate.copernicus.eu/datasets/reanalysis-era5-single-levels?tab=download). Taking the 2014 data as an example, we provide the download procedure below.

(1) Log in to the above CDS download page. 

(2) Select *Product type* as *Ensemble members*, *Variable* as *10m u-component of wind* and *10m v-component of wind*, and *Year* as *2014*. For *Month*, *Day*, and *Time*, click *Select all*. Specify *Sub-region extraction* with *North=30*, *South=12*, *West=33*, and *East=60*. Choose *Data format* as *NetCDF4(Experimental)*, *Download format* as *Unarchieved (not zipped if single file)*. Click *Submit form*. 

(3) Check *Your requests*. Once the *Status* changes to *Complete*, download the file. 

(4) After downloading, rename the file as "WindSpeeduv_ARP_2014.nc". Save the file to the sub-repository "Data".


## Reproducibility Workflow
The "Wrapper.R" file outlines the reproducibility workflow of the article, including loading necessary R packages and functions, processing the data, and reproducing each figure and table sequentially. The computation time reported below was recorded using (R 3.6.3) running on machine equipped with Intel(R) Xeon(R) CPU E5-2680 v4 @ 2.40GHz and 125 GB RAM.

Before running the code, please complete the following steps:
1. Download the entire repository as "OSG_Reproducibility_Materials.zip", extract it as a folder named "OSG_Reproducibility_Materials", and set your working directory to this folder. 
2. Download the ERA5 ensembles from https://zenodo.org/records/16618043, and save them in the sub-directory "OSG_Reproducibility_Materials/Data".
3. Download "Slepian_ARP.zip" file from https://zenodo.org/records/16655516, extract all .m files inside, and save them in the sub-repository "OSG_Reproducibility_Materials/Slepian_ARP". 

#### Process the data
Please refer to the "Wrapper.R" file to process the downloaded data. The total computational time is approximately 1.46 minutes. For more detailed code and computational time, please refer to the file "Data_Treatment.R" in the sub-repository "Data".

#### Reproduce Figure 1 in Section 2
Figure 1 demonstrates several statistical characteristics of the u- and v-component wind speed ERA5 ensembles. Please refer to the "Wrapper.R" file for its reproducibility command. The total computational time is approximately 26.49 minutes. For more detailed code and computational time, please refer to the file "Data_Demonstration.R" in the sub-repository "Data_Demonstration".

#### Reproduce Figure 2 in Section 3.1
Figure 2 illustrates Slepian bases and eigenvalues in the Slepian concentration problem. Please refer to the "Wrapper.R" file for its reproducibility command. The total computational time is approximately 2.78 seconds. For more detailed code and computational time, please refer to the file "Slepian_Demonstration.R" in the sub-repository "Slepian_ARP". 

#### Reproduce Figure 3 in Section 3.1
Figure 3 demonstrates the performance of Slepian bases. Please refer to the "Wrapper.R" file for its reproducibility command. The total computational time is approximately 4.95 minutes. For more detailed code and computational time, please refer to the file "Slepian_Performance.R" in the sub-repository "Slepian_ARP". 






