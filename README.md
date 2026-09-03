# Reproducibility Instrucion for "An online climate emulator for regional wind ensembles from ERA5"
This file documents the artifacts associated with the article (i.e., the data and code supporting the computational findings) and describes how to reproduce all figures and results.

## Article Overview
Reanalysis data provide detailed, comprehensive, and near real-time descriptions of the Earth's climate system. However, their generation, storage, and management are resource-intensive. For 3-hourly wind ensembles from the fifth generation European Centre for Medium-Range Weather Forecasts Reanalysis [(ERA5)](https://cds.climate.copernicus.eu/datasets/reanalysis-era5-single-levels?tab=overview), which face these challenges, this paper develops climate emulators, called stochastic generators (SGs). Specifically, complex multivariate climate ensembles over any region on the globe can be compactly modeled, stored as model parameters, and rapidly emulated while preserving key statistical characteristics. Furthermore, as data arrive sequentially in time blocks, the SG can be updated without retaining preceding data, thereby reducing construction memory demands and supporting near real-time emulation. For wind ensembles over the Arabian Peninsula, we develop three SGs: one trained on full data at once and two online SGs (OSGs) trained sequentially using yearly and weekly blocks to accommodate different needs. All SGs require only about 20\% of the storage of the training ensembles while closely reproducing their statistical characteristics across a comprehensive set of evaluation indices. The two OSGs further reduce construction memory demands by about 90\% and 99\%, respectively. Shorter blocks allow more frequent updates and greater memory savings, at the cost of slightly longer training time and modestly reduced emulation performance. These results demonstrate a practical approach for compactly representing, sequentially learning, and rapidly emulating large climate ensembles under limited computing resources.

## Contents
#### Case_Studies
This sub-repository contains code for implementing the case studies (Sections 4 and S4) and reproducing all associated figures and tables.
* "**FSG**": sub-repository for the stochastic generator directly derived from the full data (FSG)
  * "FSG.R": R script for FSG's construction, emulation, and evaluation
  <!-- >* "TGHparaufull.csv", "TGHparavfull.csv", "Phihatfull.mat", "Kfull.mat", "Iuq_u_full.csv", "Iuq_v_full.csv", "Ibc_full.csv", "Itc1_u_full.csv", "Itc1_v_full.csv", "Itc2_u_full.csv", "Itc2_v_full.csv", "Itp_full.csv", "Iwdt_u_full.csv", "Iwdt_v_full.csv", "Iwds_u_full.csv", "Iwds_v_full.csv", "Imd_full.csv", "Isdd_full.csv", "I75qd_full.csv", "I25qd_full.csv": outputs of "FSG.R" used to plot figures -->

* "**MC**": sub-repository for Monte Carlo assessment of emulation variability
  * "MC_FSG.R", "MC_OSG_Long.R", and "MC_OSG_Short.R": R scripts for experiments on FSG, OSG-Long, and OSG-Short, respectively. 
  * "MC_Plot.R": R script for reproducing Figure S14

* "**Scenario1_OSG_Long**": sub-repository for the OSG-Long
  * "Scenario1_OSG_Long.R": R script for OSG-Long's construction, emulation, and evaluation
  * "Plot_estimates_OSG_Long.R": R script for reproducing Figures 4-5, S6, and Table S3
  <!-- >* "RFDs_OSG_Long.csv", "TGHpara_u_all_1.mat", "TGHpara_v_all_1.mat", "Phihat_online_1.mat", "K_online_1.mat", "Iuq_u_online_1.csv", "Iuq_v_online_1.csv", "Ibc_online_1.csv", "Itc1_u_online_1.csv", "Itc1_v_online_1.csv", "Itc2_u_online_1.csv", "Itc2_v_online_1.csv", "Itp_online_1.csv", "Iwdt_u_online_1.csv", "Iwdt_v_online_1.csv", "Iwds_u_online_1.csv", "Iwds_v_online_1.csv", "Imd_online_1.csv", "Isdd_online_1.csv", "I75qd_online_1.csv", "I25qd_online_1.csv": outputs of "Scenario1_OSG_Long.R" used to plot figures -->

* "**Scenario2_OSG_Short**": sub-repository for the OSG-Short
  * "Scenario2_OSG_Short.R": R script for OSG-Short's construction, emulation, and evaluation
  * "Plot_estimates_OSG_Short.R": R script for reproducing Figures 7-8 and S8-9
  <!-- >* "RFDs_OSG_Short.csv", "TGHpara_u_all_2.mat", "TGHpara_v_all_2.mat", "Phihat_online_2.mat", "K_online_2.mat", "Iuq_u_online_2.csv", "Iuq_v_online_2.csv", "Ibc_online_2.csv", "Itc1_u_online_2.csv", "Itc1_v_online_2.csv", "Itc2_u_online_2.csv", "Itc2_v_online_2.csv", "Itp_online_2.csv", "Iwdt_u_online_2.csv", "Iwdt_v_online_2.csv", "Iwds_u_online_2.csv", "Iwds_v_online_2.csv", "Imd_online_2.csv", "Isdd_online_2.csv", "I75qd_online_2.csv", "I25qd_online_2.csv": outputs of "Scenario2_OSG_Short.R" used to plot figures -->

* "**Tuning_Parameters**": sub-repository for tuning parameters
  * "ChooseA.R": R script for choosing the number of Slepian bases $A$
  * "ChooseP.R": R script for choosing the order $P$ of the vecror autoregressive model (VAR)

* "Plot_emulation_performance.R": R script for reproducing Figures 6, 9, S7, S10, and S11
* "Storage.R": R script for reproducing Table 2 and Figure S5

#### Data
This sub-repository is used to store the downloaded ERA5 ensemble data and to provide detailed instructions for processing them.
* "Data_Treatment.R": R script for processing the downloaded data
* "IPCC-WGI-reference-regions-v4_R.rda": R data file for identifying the Arabian Pennisula region (ARP), provided by [Iturbide et al. (2020)](https://essd.copernicus.org/articles/12/2959/2020/essd-12-2959-2020-assets.html)
<!-- >* "WindSpeeduv_ARP_2014.nc" -- "WindSpeeduv_ARP_2023.nc": downloaded ERA5 ensemble data used in this work -->

#### Data_Demonstration
This sub-repository provides the R scripts for reproducing Figures 1 and S2, which demonstrates several statistical characteristics of the u- and v-component wind speed ERA5 ensembles.
* "Data_Demonstration.R": R script for reproducing Figure 1
* "Variogram.R": R script for reproducing Figure S2

#### Functions
This sub-repository contains R scripts for several frequently used functions.
* "Assessment_Indices.R": functions for calculating assessment indices in Table 1
* "InverseTH.R": function for performing the inverse Tukey h transformation

#### Slepian_ARP
This sub-repository provides the R scripts for reproducing Figures 2 and 3, which demonstrates Slepian bases in the Slepian concentration problem and their performance. It also provides the R script for Figure S3. Additionally, this sub-repository is used to store the downloaded Slepian bases.
* "Slepian_Demonstration.R": R script for reproducing Figure 2
* "Slepian_Performance.R": R script for reproducing Figure 3
* "Threshold.R": R script for reproducing Figure S3
<!-- >* "Basis_reg_eig_value_181_real.mat" and others: downloaded eigenvalues in the Slepian concentration problem -->
<!-- >* "Slepian_spatial_181.mat" and others: downloaded Slepian bases in the Slepian concentration problem -->

#### VAE
This sub-repository provides the R and python scripts for reproducing Figure S12, which demonstrates the performance of variational autoencoder (VAE) trained by initial data blocks of various lengths. 
* "VAE_Performance.R": R script for reproducing Figure S12
* "Wind_VAE_Perform_Update.py": python script for geting the performance of VAE
* "Wind_VAE_Tuning_Parameters.py": python script for selecting tuning parameters in VAE
* "test_recon_loss_1m.npy", "test_recon_loss_1y.npy", "test_recon_loss_9y.npy": outputs of "Wind_VAE_Perform_Update.py" used to plot Figure S12 

#### "Wrapper.R"
This file outlines the reproducibility workflow of the article.  


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

*Note: If segmentation faults occur, you may try limiting BLAS to a single thread by setting `OPENBLAS_NUM_THREADS=1`, `MKL_NUM_THREADS=1`, and `OMP_NUM_THREADS=1` before launching R.*

#### Process the data
Please refer to the "Wrapper.R" file to process the downloaded data. The total computational time is approximately 1.46 minutes. For more detailed code and computational time, please refer to the file "Data_Treatment.R" in the sub-repository "Data".

#### Reproduce Figure 1 in Section 2
Figure 1 demonstrates several statistical characteristics of the u- and v-component wind speed ERA5 ensembles. Please refer to the "Wrapper.R" file for its reproducibility command. The total computational time is approximately 26.49 minutes. For more detailed code and computational time, please refer to the file "Data_Demonstration.R" in the sub-repository "Data_Demonstration".

#### Reproduce Figure 2 in Section 3.1
Figure 2 illustrates Slepian bases and eigenvalues in the Slepian concentration problem. Please refer to the "Wrapper.R" file for its reproducibility command. The total computational time is approximately 2.78 seconds. For more detailed code and computational time, please refer to the file "Slepian_Demonstration.R" in the sub-repository "Slepian_ARP". 

#### Reproduce Figure 3 in Section 3.1
Figure 3 demonstrates the performance of Slepian bases. Please refer to the "Wrapper.R" file for its reproducibility command. The total computational time is approximately 4.95 minutes. For more detailed code and computational time, please refer to the file "Slepian_Performance.R" in the sub-repository "Slepian_ARP". 

#### Reproduce Figures 4-5 in Section 4.2 (and Figure S8 and Table S3 in Section S4.2)
Figures 4-5, S8, and Table S3 demonstrate the estimates and updates of parameters in OSG-Long. Please refer to the "Wrapper.R" file for its reproducibility command. The total computational time is approximately 10.75 hours, with most of the time spent on evaluating FSG and OSG‑Long. Constructing both SGs and generating emulations require only about 1.35 hours. For more detailed code and computational time, please refer to the files "Case_Studies/FSG/FSG.R", "Case_Studies/Scenario1_OSG_Long/Scenario1_OSG_Long.R", and "Case_Studies/Scenario1_OSG_Long/Plot_estimates_OSG_Long.R".

#### Reproduce Figures 7-8 in Section 4.3 (and Figures S10-11 in Section S4.3)
Figures 7-8 and S10-11 demonstrate the estimates and updates of parameters in OSG-Short. Please refer to the "Wrapper.R" file for its reproducibility command. The total computational time is approximately 5.51 hours, with most of the time spent on evaluating OSG‑Short. Constructing OSG-Short and generating emulations require only about 48.95 minutes. For more detailed code and computational time, please refer to the files "Case_Studies/Scenario2_OSG_Short/Scenario2_OSG_Short.R", and "Case_Studies/Scenario2_OSG_Short/Plot_estimates_OSG_Short.R".

#### Reproduce Figure 6 in Section 4.2 and Figure 9 in Section 4.3 (and Figure S9 in Section S4.2 and Figures S12-13 in Section S4.3)
Figures 6, 9, S9, S12, and S13 demonstrate the emulation performance of FSG, OSG-Long, and OSG-Short. Please refer to the "Wrapper.R" file for its reproducibility command. With outputs in sub-repositories "Case_Studies/FSG", "Case_Studies/Scenario1_OSG_Long", and "Case_Studies/Scenario2_OSG_Short", the total computational time is approximately 30.07 seconds. For more detailed code, please refer to the files "Case_Studies/Plot_emulation_performance.R".

#### Reproduce Table 2 in Section 4.3 (and Figure S7 in Section S4.1)
Table 2 demonstrates the storage demands of FSG, OSG-Long, and OSG-Short construction. Figure S7 compares storage of training data and SG parameters. Please refer to the "Wrapper.R" file for its reproducibility command. The total computational time is approximately 1.31 seconds. For more detailed code, please refer to the file "Storage.R" in the sub-repository "Case_Studies". 

#### Reproduce (Figures S2 in Section S3.1)
Figure S2 shows the empirical directional variogram of random effects. Please refer to the "Wrapper.R" file for its reproducibility command. The total computational time is approximately 9.34 seconds. For detailed code, please refer to the file "Variogram.R" in the sub-repository "Data_Demonstration".

#### Reproduce (Figures S3 in Section S3.2)
Figure S3 provides a sensitivity analysis of Slepian concentration level threshold $\lambda_A$. Please refer to the "Wrapper.R" file for its reproducibility command. The total computational time is approximately X seconds. For detailed code, please refer to the file "Threshold.R" in the sub-repository "Slepian_ARP".

#### Reproduce (Figures S4-5 in Section S4.1)
Figures S4-5 help to select the number of Slepian bases $A$. Please refer to the "Wrapper.R" file for its reproducibility command. The total computational time is approximately 1.67 hours. For more detailed code and computational time, please refer to the file "ChooseA.R" in the sub-repository "Case_Studies/Tuning_Parameters".  

#### Reproduce (Figure S6 in Section S4.1)
Figure S6 helps to select the order $P$ of the VAR model. Please refer to the "Wrapper.R" file for its reproducibility command. The total computational time is approximately 1.26 hours. For more detailed code and computational time, please refer to the file "ChooseP.R" in the sub-repository "Case_Studies/Tuning_Parameters".    

#### Reproduce (Figure S14 in Section S4.4)
Figure S14 illustrates Monte Carlo assessment of emulation variability. Please refer to the "Wrapper.R" file for its reproducibility command. The total computational time is approximately 36 hours. For detailed code, please refer to the files "MC_FSG.R", "MC_OSG_Long.R", "MC_OSG_Short.R", and "MC_Plot.R" in the sub-repository "Case_Studies/MC".

#### Reproduce (Figure S15 in Section S5)
Figure S15 demonstrates the performance of VAE trained by initial data blocks covering the first month, the first year, and the first nine years, respectively. Please refer to the "Wrapper.R" file for its reproducibility command. For more detailed code, please refer to the file "VAE_Performance.R" in the sub-repository "VAE". 

## Supporting Software Requirement
#### Version of primary software used
R version 3.6.3 (2020-02-29)

#### Libraries and dependencies used by the code
here(1.0.1), ncdf4(1.19), sp(1.4-6), ggplot2(3.5.1), maps(3.4.0), moments(0.14.1), R.matlab(3.6.2), LambertW(0.6.9-1), fdaoutlier(0.2.0), matrixStats(1.5.0), approxOT(1.0.2), patchwork(1.2.0), scales(1.3.0), dplyr(1.1.4), reticulate(1.14)

#### Reproducibility Testing on Different Computing Environments
The code and workflow have been tested across two additional computing environments: (1) a MacBook Pro 14 with an Apple M1 Pro chip and 16 GB of memory, and (2) a university computing server allocated 18 CPUs and approximately 72 GB of memory in total. Both environments used recent versions of R and the required packages. Detailed system and software configurations for these two new environments, together with those of the previously used workstation, are provided in the table below. 
| | MacBook Pro | University Server | Previous Workstation |
| -------- | -------- | -------- | -------- |
| Processor | Apple M1 Pro | Intel(R) Xeon(R) Gold 6230 CPU @ 2.10GHz | Intel(R) Xeon(R) CPU E5-2680 v4 @ 2.40GHz |
| Memory (GB) | 16  | 72  | 125 |
| R version | 4.6.1 (2026-06-24) | 4.5.0 (2025-04-11) | 3.6.3 (2020-02-29) |
| Package version| here(1.0.2), ncdf4(1.24), sp(2.2-3), ggplot2(4.0.3), maps(3.4.3), dplyr(1.2.1), R.matlab(3.7.0), LambertW(0.6.9-2), fdaoutlier(0.2.1), matrixStats(1.5.0), approxOT(1.2), patchwork(1.3.2), scales(1.4.0),moments(0.14.1) | here(1.0.1), ncdf4(1.24), sp(2.2-0), ggplot2(3.5.2), maps(3.4.2.1), dplyr(1.1.4), R.matlab(3.6.2), LambertW(0.6.9-2), fdaoutlier(0.2.1), matrixStats(1.5.0), approxOT(1.2), patchwork(1.3.2), scales(1.4.0), moments(0.14.1)| here(1.0.1), ncdf4(1.19), sp(1.4-6), ggplot2(3.5.1), maps(3.4.0), dplyr(1.1.4), R.matlab(3.6.2), LambertW(0.6.9-1), fdaoutlier(0.2.0), matrixStats(1.5.0), approxOT(1.0.2), patchwork(1.2.0), scales(1.3.0), moments(0.14.1)|

The computational times (seconds) for the main steps of the workflow obtained under different computational environments are reported below. Since all SGs share the same emulation and evaluation procedures, the reported times are averaged across all SGs. 
| Step (*line in ``Wrapper.R"*) | MacBook Pro | University Server | Previous Workstation |
| -------- | -------- | -------- | -------- |
| Data demonstration (*52*) | $481.65$  | $528.71$  | $1589.51$ |
| Slepian demonstration (*61*) | $2.90$ | $2.62$ | $2.78$ |
| Slepian performance (*70*) | $151.01$ | $180.09$ | $296.71$ |
| FSG construction (*86*) | NA | $603.86$ | $1637.94$ |
| OSG-Long construction (*95*) | $5934.93$ | $650.71$ | $1654.41$ |
| OSG-Short construction (*116*) | $7632.07$ | $1012.96$ | $2152.60$ |
| Emulation (*86*, *95*, *116*) | NA | $578.48$ | $784.49$ |
| Evaluation (*86*, *95*, *116*) | NA | $12288.83$ | $16896.28$ |

As shown in the table, the MacBook Pro with 16 GB of memory fails to construct the FSG because of the memory limitation. In contrast, both OSG-Long and OSG-Short are successfully constructed under this memory-constrained environment. However, the workflow stops at the emulation stage because of the same memory limitation. On the university server, all three SGs are successfully constructed, emulated, and evaluated.

