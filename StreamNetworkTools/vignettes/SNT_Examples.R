# example workflow for StreamNetworkTools
remove.packages("StreamNetworkTools")

# install devtools if not already
#install.packages(devtools)
library("devtools")
#install StreamNetworkTools from github repository
#install_git("https://github.com/dkopp3/StreamNetworkTools_git.git", subdir = "StreamNetworkTools")
library(StreamNetworkTools)

#check package help
help(package="StreamNetworkTools")

# to begin download NHDPlusV2 data for Vector Processing Unit (VPU) of interest
# here we are using VPU 11 which includes Arkansas, Red and White River basins
# check function documentation for further details
# setwd to example folder - where NHDPlusV2 data will be downloaded
# this is mostly to keep myself organized
setwd("C:/Users/Darin/Dropbox/Dissertation/Chapter_2_StreamNetworkTools/StreamNetworkTools_git/Example_Data")

?net_nhdplus
net_nhdplus(nhdplus_path = getwd(), download = "http",
            vpu = 11, files = ALL, zip_7 = "C:/Program Files/7-Zip")



