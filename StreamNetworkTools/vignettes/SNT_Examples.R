# example workflow for StreamNetworkTools

# install devtools if not already
# install.packages(devtools)
library("devtools")

# install StreamNetworkTools from github repository
# install_git("https://github.com/dkopp3/StreamNetworkTools_git.git", subdir = "StreamNetworkTools")
library(StreamNetworkTools)

#OR download the

#check SNT package help for avaialble functions
help(package = "StreamNetworkTools")

# to begin download NHDPlusV2 data for Vector Processing Unit (VPU) of interest
# here we are using VPU 11 which includes Arkansas, Red and White River basins
# check function documentation for further details
# setwd to example folder - where NHDPlusV2 data will be downloaded
setwd("C:/Users/Darin/Dropbox/Dissertation/Chapter_2_StreamNetworkTools/StreamNetworkTools_git/Example_Data")
install.packages("devtools")
library(devtools)

help(packages = "StreamNetworkTools")
ls(StreamNetworkTools)
getwd()
devtools::build()


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
# Network is data.frame. group.comid indexes a Network
# and net.comid are their asociated COMID's
# can write network as shapefile with
library(sf)
write_sf(rmd_netdelin$SF_Obj, paste(getwd(),"/rmd_netdelin.shp", sep = ""))
str(rmd_netdelin)

# can also "snap" user defined points to nearest NHDPlusV2 flowline
# this could be useful for working with national scale datasets (WAS, NARS, BioData)
# prep sample points
ExLoc <- read.csv("Sample_Locations.csv")
head(ExLoc)
# Locations must be renamed as follows, kind of annoying
ExLoc <- ExLoc[,c("SiteName","W","N")]
names(ExLoc) <- c("SITE_ID","X","Y")

# finding closest COMID
sam_pts <- net_comid(sample_points = ExLoc, CRS = 4269,
                     nhdplus_path = getwd(), vpu = 11, maxdist = 200)
head(sam_pts)

# value added attribute queries for NHDPlusV2
# should be able to interchange sam_pts and rmd_netdelin in all following functions
# landcover percentage are for sub-catchments
rmd_netlc <- net_lc(netdelin = rmd_netdelin, vpu = "11", nhdplus_path = getwd())
# field headings follow https://www.mrlc.gov/nlcd11_leg.php
head(rmd_netlc)

# climate variables follow world clim, bioclim variables and were calculated from 1971-2001 PRISM
# where necessary units are in temp in deg C and ppt in mm
rmd_netclim <- net_clim (nhdplus_path = getwd(), vpu = "11", netdelin = rmd_netdelin)
head(rmd_netclim)

# flow variables were calculated using the Mean Annual and Mean Monthly flow estimates from NHDPlusV2 EROM
rmd_netflow <- net_flow(nhdplus_path = getwd(), vpu = "11", netdelin = rmd_netdelin)
head(rmd_netflow)

# network scale topology: counts headwaters, nodes, trib juntions, edges
# catchment area and total length and drainage density
rmd_netclac <- net_calc(netdelin = rmd_netdelin, vpu = "11", nhdplus_path = getwd())
head(rmd_netclac)

# net_hort determines horton laws for networks
# produces a list: topology are means for each stream order
# horton est and the estimated hortonian ratios
rmd_nethort <- net_hort(netdelin = rmd_netdelin, vpu = "11", nhdplus_path = getwd())
#str(rmd_nethort)

# net sinu gives sinusity and slope values for each reach (comid) within a network
rmd_netsinu <- net_sinu (netdelin = rmd_netdelin, nhdplus_path = getwd(), vpu = "11")
head(rmd_netsinu)

# can be aggregated as a network-scale mean
mean.sinu <- aggregate(rmd_netsinu[,"sinuosity"],
                       by = list(net.id = rmd_netsinu[,"net.id"]),
                       mean)
head(mean.sinu)

# net_conflu results are given for each confluence
# this takes a while
rmd_netconflu <- net_conflu(netdelin = rmd_netdelin, nhdplus_path = getwd(), vpu = "11")
head(rmd_netconflu)

#net_cat
rmd_netcat <- net_cat(netdelin = rmd_netdelin, nhdplus_path = getwd(), vpu = "11", dissolve = "N")
str(rmd_netcat)

#basin shape metrics
rmd_catshp <- cat_shp(rmd_netcat)
head(rmd_catshp)
