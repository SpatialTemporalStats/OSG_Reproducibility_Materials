################################################################################
# This file includes all steps to process the data                             #
# Input: Downloaded ERA5 ensembles named e.g., "WindSpeeduv_ARP_2014.nc"       #
# Output: Processed data and related information used in this work,            #
#         e.g., Windu.ARP, Windv.ARP, Dat.loc.arp                              #   
################################################################################
# Necessary packages
library(sp)
library(ncdf4)

###### Part 1. Load and Combine ERA5 Data from 2014 to 2023
# The dataset consists of bivariate 3‑hourly wind speed ensembles spanning ten years. 
# Note that the last day of February in the leap years 2016 and 2020 should be removed.
# t1=proc.time()[[3]]
daT=nc_open("Data/WindSpeeduv_ARP_2014.nc")
lon=ncvar_get(daT,varid="longitude")                                 # lon: Longitude
lat=ncvar_get(daT,varid="latitude")                                  # lat: Latitude
Dat.loc=cbind(rep(lon,times=length(lat)),rep(lat,each=length(lon)))  # Dat.loc: Data location
R=length(ncvar_get(daT,varid="number"))                              # R: Number of ensembles 
NT=length(ncvar_get(daT,varid="time"))                               # NT: Number of time points in 2014 
Dat.ru=ncvar_get(daT,varid = "u10")                                  # Dat.ru: 10m u-component wind speed for 2014
Dat.rv=ncvar_get(daT,varid = "v10")                                  # Dat.rv: 10m v-component wind speed for 2014
Windu.ARP=Windv.ARP=array(0,c(R,nrow(Dat.loc),10*NT))                # Windu.ARP/Windv.ARP: Array to store u/v-component from all ten years

# Write Dat.ru/Dat.rv for 2014 into Windu.ARP/Windv.ARP
for(r in 1:R){
  for(t in 1:NT){
    Windu.ARP[r,,t]=c(Dat.ru[,,r,t])
    Windv.ARP[r,,t]=c(Dat.rv[,,r,t])
  }
}
# Read and write Dat.ru/Dat.rv for 2015 into Windu.ARP/Windv.ARP
daT=nc_open("Data/WindSpeeduv_ARP_2015.nc")
Dat.ru=ncvar_get(daT,varid = "u10")
Dat.rv=ncvar_get(daT,varid = "v10")
for(r in 1:R){
  for(t in 1:NT){
    Windu.ARP[r,,NT*1+t]=c(Dat.ru[,,r,t])
    Windv.ARP[r,,NT*1+t]=c(Dat.rv[,,r,t])
  }
}
# Read and write Dat.ru/Dat.rv for 2016 into Windu.ARP/Windv.ARP
daT=nc_open("Data/WindSpeeduv_ARP_2016.nc")
Dat.ru=ncvar_get(daT,varid = "u10")
Dat.rv=ncvar_get(daT,varid = "v10")
Dat.ru=Dat.ru[,,,-(473:480)]  #(31+28)*8+1=473  
Dat.rv=Dat.rv[,,,-(473:480)]  #(31+29)*8=480
for(r in 1:R){
  for(t in 1:NT){
    Windu.ARP[r,,NT*2+t]=c(Dat.ru[,,r,t])
    Windv.ARP[r,,NT*2+t]=c(Dat.rv[,,r,t])
  }
}
# Read and write Dat.ru/Dat.rv for 2017 into Windu.ARP/Windv.ARP
daT=nc_open("Data/WindSpeeduv_ARP_2017.nc")
Dat.ru=ncvar_get(daT,varid = "u10")
Dat.rv=ncvar_get(daT,varid = "v10")
for(r in 1:R){
  for(t in 1:NT){
    Windu.ARP[r,,NT*3+t]=c(Dat.ru[,,r,t])
    Windv.ARP[r,,NT*3+t]=c(Dat.rv[,,r,t])
  }
}
# Read and write Dat.ru/Dat.rv for 2018 into Windu.ARP/Windv.ARP
daT=nc_open("Data/WindSpeeduv_ARP_2018.nc")
Dat.ru=ncvar_get(daT,varid = "u10")
Dat.rv=ncvar_get(daT,varid = "v10")
for(r in 1:R){
  for(t in 1:NT){
    Windu.ARP[r,,NT*4+t]=c(Dat.ru[,,r,t])
    Windv.ARP[r,,NT*4+t]=c(Dat.rv[,,r,t])
  }
}
# Read and write Dat.ru/Dat.rv for 2019 into Windu.ARP/Windv.ARP
daT=nc_open("Data/WindSpeeduv_ARP_2019.nc")
Dat.ru=ncvar_get(daT,varid = "u10")
Dat.rv=ncvar_get(daT,varid = "v10")
for(r in 1:R){
  for(t in 1:NT){
    Windu.ARP[r,,NT*5+t]=c(Dat.ru[,,r,t])
    Windv.ARP[r,,NT*5+t]=c(Dat.rv[,,r,t])
  }
}
# Read and write Dat.ru/Dat.rv for 2020 into Windu.ARP/Windv.ARP
daT=nc_open("Data/WindSpeeduv_ARP_2020.nc")
Dat.ru=ncvar_get(daT,varid = "u10")[,,,-(473:480)]
Dat.rv=ncvar_get(daT,varid = "v10")[,,,-(473:480)]
for(r in 1:R){
  for(t in 1:NT){
    Windu.ARP[r,,NT*6+t]=c(Dat.ru[,,r,t])
    Windv.ARP[r,,NT*6+t]=c(Dat.rv[,,r,t])
  }
}
# Read and write Dat.ru/Dat.rv for 2021 into Windu.ARP/Windv.ARP
daT=nc_open("Data/WindSpeeduv_ARP_2021.nc")
Dat.ru=ncvar_get(daT,varid = "u10")
Dat.rv=ncvar_get(daT,varid = "v10")
for(r in 1:R){
  for(t in 1:NT){
    Windu.ARP[r,,NT*7+t]=c(Dat.ru[,,r,t])
    Windv.ARP[r,,NT*7+t]=c(Dat.rv[,,r,t])
  }
}
# Read and write Dat.ru/Dat.rv for 2022 into Windu.ARP/Windv.ARP
daT=nc_open("Data/WindSpeeduv_ARP_2022.nc")
Dat.ru=ncvar_get(daT,varid = "u10")
Dat.rv=ncvar_get(daT,varid = "v10")
for(r in 1:R){
  for(t in 1:NT){
    Windu.ARP[r,,NT*8+t]=c(Dat.ru[,,r,t])
    Windv.ARP[r,,NT*8+t]=c(Dat.rv[,,r,t])
  }
}
# Read and write Dat.ru/Dat.rv for 2023 into Windu.ARP/Windv.ARP
daT=nc_open("Data/WindSpeeduv_ARP_2023.nc")
Dat.ru=ncvar_get(daT,varid = "u10")
Dat.rv=ncvar_get(daT,varid = "v10")
for(r in 1:R){
  for(t in 1:NT){
    Windu.ARP[r,,NT*9+t]=c(Dat.ru[,,r,t])
    Windv.ARP[r,,NT*9+t]=c(Dat.rv[,,r,t])
  }
}
# set Dat.ru and Datrv=0 to save space
Dat.ru=0
Dat.rv=0
# t2=proc.time()[[3]]
# t2-t1=78.728 seconds (1.31 minutes)



###### Part 2. Load the geometry information for the Ababian Pennisula Region (ARP)
# t1=proc.time()[[3]]
load("Data/IPCC-WGI-reference-regions-v4_R.rda")
ARP=IPCC_WGI_reference_regions_v4@polygons[['ARP']]
SPPol=SpatialPolygons(list(ARP),proj4string = CRS("+proj=longlat +datum=WGS84"))
sp_point=SpatialPoints(Dat.loc, proj4string = CRS("+proj=longlat +datum=WGS84"))
is.inside=over(sp_point,SPPol)
id.ARP=which(!is.na(is.inside))           # length(id.ARP): 1215
# t2=proc.time()[[3]]
# t2-t1=0.113



###### Part 3. Obtain data over the ARP and their grid points
t1=proc.time()[[3]]
Windu.ARP=Windu.ARP[,id.ARP,]
Windv.ARP=Windv.ARP[,id.ARP,]
Dat.loc.arp=Dat.loc[id.ARP,]
t2=proc.time()[[3]]
# t2-t1=8.737





