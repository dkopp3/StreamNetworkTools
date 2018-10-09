# example workflow for StreamNetworkTools

# install devtools if not already
# install.packages(devtools)
library("devtools")

# install StreamNetworkTools from github repository
# install_git("https://github.com/dkopp3/StreamNetworkTools_git.git", subdir = "StreamNetworkTools")
library(StreamNetworkTools)

#check SNT package help for avaialble functions
help(package ="StreamNetworkTools")

# to begin download NHDPlusV2 data for Vector Processing Unit (VPU) of interest
# here we are using VPU 11 which includes Arkansas, Red and White River basins
# check function documentation for further details
# setwd to example folder - where NHDPlusV2 data will be downloaded

setwd("C:/Users/Darin/Dropbox/Dissertation/Chapter_2_StreamNetworkTools/StreamNetworkTools_git/Example_Data")

# check documentation
# net_nhdplus
# can download all available data for vpu but sppecified only files needed to run streamNetworkTools
# this can be time consuming, be patient it only has to be done once
# data for VPU is already present in example data folder

#net_nhdplus(nhdplus_path = getwd(), download = "http",
 #           vpu = 11, files = c("NHDPlusAttributes", "NHDSnapshot",
  #                              "NHDPlusCatchment", "VPUAttributeExtension",
   #                             "VogelExtension", "EROMExtension"),
    #        zip_7 = "C:/Program Files/7-Zip")

# can run with for loop to download all VPU in CONUS
# recommend running overnight

#vpus<- c("01","02","03N","03S","03W","04","05","06","07","08","09","10U",
 #        "10L","11","12","13","14","15","16","17","18")

# for (i in vpus){
  # net_nhdplus(nhdplus_path = getwd(), download = "http",
   #            vpu = i, files = c("NHDPlusAttributes", "NHDSnapshot",
    #                               "NHDPlusCatchment", "VPUAttributeExtension",
     #                              "VogelExtension", "EROMExtension"),
      #         zip_7 = "C:/Program Files/7-Zip")
#}

# net_sample randomly selects comid's of specified stream order from a vector processing unit
# below identifies 3, fifth order comid's from VPU 11

rmd_comid <- net_sample(nhdplus_path = getwd(), vpu = "11", ws_order = 5, n = 3)
print(rmd_comid)

# net_delin queries all COMID's upsteam comid's from root
# inpput group_comids must be character
rmd_netdelin <- net_delin(group_comid = as.character(rmd_comid),
                          nhdplus_path = getwd(),
                          vpu = "11")
# output is list of 3
str(rmd_netdelin)

# can write network as shapefile with
library(sf)
write_sf(rmd_netdelin$SF_Obj, paste(getwd(),"/rmd_netdelin.shp", sep = ""))

# value added attribute queries for NHDPlusV2
# landcover percentage are for sub-catchments
rmd_netlc <- net_lc(netdelin = rmd_netdelin, vpu = "11", nhdplus_path = getwd())
# field headings follow https://www.mrlc.gov/nlcd11_leg.php
head(rmd_netlc)

# climate variables follow world clim, bioclim variables and were calculated from 1971-2001 PRISM
# where necessary units are in tempp in deg C and ppt in mm
rmd_netclim <- net_clim (nhdplus_path = getwd(), vpu = "11", netdelin = rmd_netdelin)
head(rmd_netclim)

# flow variables were calculated using the Mean Annual and Mean Monthly EROM
rmd_netflow <- net_flow(nhdplus_path = getwd(), vpu = "11", netdelin = rmd_netdelin)
head(rmd_netflow)

# network scale topology
rmd_netclac <- net_calc(netdelin = rmd_netdelin, vpu = "11", nhdplus_path = getwd())
head(rmd_netclac)

# net_hort M values are not included here
rmd_nethort <- net_hort(netdelin = rmd_netdelin, vpu = "11", nhdplus_path = getwd())
str(rmd_nethort)

# net sinu gives values for each reach (comid) within a network
rmd_netsinu <- net_sinu (netdelin = rmd_netdelin, nhdplus_path = getwd(), vpu = "11")
head(rmd_netsinu)

# can be aggregated as a network mean
mean.sinu <- aggregate(rmd_netsinu[,"sinuosity"],
                       by = list(group.comid = rmd_netsinu[,"group.comid"]),
                       mean)
head(mean.sinu)

# net_conflu resullts are given by each confluence
# this takes a while - ended
#rmd_netconflu <- net_conflu(netdelin = rmd_netdelin,
 #                           nhdplus_path = getwd(),
  #                          vpu = "11")
