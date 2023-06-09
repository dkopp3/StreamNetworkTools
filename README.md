# StreamNetworkTools --- Updated and Assimiliated to MMASN package. 
Rpackage: StreamNetworkTools 
Darin Kopp | darinkopp@gmail.com 

Abstract: 
	Understanding large scale processes in lotic ecosystems requires a requires a geospatial framework and set of tools designed characterize river networks. StreamNetworkTools uses the NHDPlusV2 dataset to delineate stream networks and characterize their physical and geomorphical attributes. 

Purpose: an R package to derive covariates from NHDPlusV2 dataset and facilitate continental scale analyses of river networks.  
Status: V1.0

Data Requirements: 
StreamNetworkTools works with NHDPlusV2.  Users should become familiar with NHDPlusV2 documentation (http://www.horizon-systems.com/NHDPlus/NHDPlusV2_documentation.php)

Installing StreamNetworkTools*
1)	install.packages(devtools)
2)	library("devtools")
3)	install_git("https://github.com/dkopp3/StreamNetworkTools_git.git", subdir = "StreamNetworkTools")
4)	library(StreamNetworkTools)
5)	help(package="StreamNetworkTools")
OR*
1)	download StreamNetworkTools_1.0.0.000.tar.gz from github
2)	install.packages (/StreamNetworkTools_1.0.0.000.tar.gz", repos = NULL, type="source")
*if reinstalling after update, remember to: 
1)	remove.packages("StreamNetworkTools")
2)	restart R
3)	repeat above

 
Figure 1:  Workflow of StreamNetworkTools. Lines connecting boxes show relationships between functions. See StreamNetworkTools for descriptions. 
Variable descriptions
Category	Variable	Definition	StreamNetworkTools
 Function
Basin Shape	basin_len	longest distance between two catchment verticies	net_cat
Basin Shape	basin_area	area of catchments	net_cat
Basin Shape	basin_width	efffective basin width 
(basin_area/basin_len)	net_cat
Topology	WS.order	strahler order for root node	net_calc
Topology	head.h2o	number of headwater reaches	net_calc
Topology	trib.jun	number of tributary junctions	net_calc
Topology	reach.cnt	number of reaches in network	net_calc
Topology	diver.cnt	count of divergent flow paths	net_calc
Topology	AREASQKM	drainage area (km^2)	net_calc
Topology	LENGTHKM	total lenght of network flow-lines (km)	net_calc
Topology	drain.den	drainage density (LENGTHKM / AREASQKM)	net_calc
Climate	TEMPVC	mean annual temperature
 (deg C)	net_clim
Climate	seasonality_t	Coefficient of variation of mean monthly temperatures	net_clim
Climate	warm_mo	2-digit warmest month	net_clim
Climate	warm_mo_t	mean temperature of warmest month	net_clim
Climate	cold_mo	2-digit coldest month	net_clim
Climate	cold_mo_t	mean temperature of coldest month	net_clim
Climate	diff_t	difference between warm and cold monthly temperatures	net_clim
Climate	warm_q_t	mean temperature of warmest quarter	net_clim
Climate	warm_q	2-digit warmest quarter	net_clim
Climate	cold_q_t	mean temperature of coldest quarter	net_clim
Climate	cold_q	2-digit coldest quarter	net_clim
Climate	PRECIPVC	cumulative mean annual precipiration (mm)	net_clim
Climate	wet_mo	2-digit wettest month	net_clim
Climate	wet_mo_p	cumulative mean precipitation of wettest month	net_clim
Climate	dry_mo	2-digit driest month	net_clim
Climate	dry_mo_p	cumulative mean precipitation of driest month	net_clim
Climate	seasonality_p	coefficient of vatiation of mean monthly precipitation	net_clim
Climate	wet_q_p	cumulaltive mean precipita-tion of wettest quarter	net_clim
Climate	wet_q	2-digit wettest quarter	net_clim
Climate	dry_q_p	cumulative mean precipitation of driest quarter	net_clim
Climate	dry_q	2-digit driest quarter	net_clim
Climate	dry_q_t	mean temperature of driest quarter	net_clim
Climate	wet_q_t	mean temperature of wettest quarter	net_clim
Climate	warm_q_p	cumulaltive mean precipita-tion of warmest quarter	net_clim
Climate	cold_q_p	cumulaltive mean precipita-tion of coldest quarter	net_clim
Topology	trib_order	order of COMID downstream of confluence	net_conflu
Topology	area_ratio	darinage areas ratios 
(i.e. Triburaty Drainage Area / Mainstem Drainage Area)	net_conflu
Topology	trib_area	drainage area upstream of 
confluence	net_conflu
Topology	junction_num	Concatenation of stream or-ders of confluence reaches	net_conflu
Topology	alpha	angle (degrees) of  tributary junction	net_conflu
Topology	complex	indicates complex tributary junction	net_conflu
Flow	RUNOFFVC	cumulative mean annual run-off (mm)	net_flow
Flow	MAQ0001E	Mean Annual EROM discharge	net_flow
Flow	minMMQ0001E	minimum mean monthly dis-charge	net_flow
Flow	max-MMQ0001E	maximum mean monthly
discharge (cf)	net_flow
Flow	covMMQ0001E	coefficient of variation of mean monthly discharge	net_flow
Flow	V0001E	mean annual velocity (cfs)	net_flow
Flow	minMMV0001E	minimum mean monthly ve-locity (cfs)	net_flow
Flow	max-MMV0001E	maximum mean monthly ve-locity	net_flow
Flow	covMMV0001E	coefficient of variation in mean monthly velocity	net_flow
Topology	str_ord	stream order	net_hort
Topology	str_num	count of stream reaches of specified order	net_hort
Topology	str_len	mean length of stream reach-es of specified order	net_hort
Topology	str_area	mean drainage area of stream reaches of specified order	net_hort
Topology	ohm	order of the network - 1	net_hort
Topology	Rb	bifurcation Ratio	net_hort
Topology	Rl	length Ratio	net_hort
Topology	Ra	area Ratio	net_hort
Landcover		NLCD2011 landcover percent-ages	net_lc
Planform	tot.len	length of reach	net_sinu
Planform	str.len	straight line length of reach	net_sinu
Planform	sinuosity	total length / straight line length	net_sinu
Planform	MaxElevSM	maximum elevation of reach	net_sinu
Planform	MinElevSM	minimum elevtion of reach	net_sinu
Planform	SlopeNHDPlus	slope of reach	net_sinu

Error Log: 
Upon loading StreamNetworkTools: “package or namespace load failed for 'dplyr' in loadNamespace…” because of missing package “bindr” resolved with restarting R and install.packages(“bindr”) before loading StreamNetworkTools

NHDPlus stream Order Error: Group id = 17153302, vpu = 18 is listed as 4th order but contains 5th order network upstream. Error at Tributaty junction of COMID’s 17153404 (STREAMORDE = 3) and 948030237 (STREAMORDE = 5) yields 3 order stream, should be 5th order. Needs to be reported

