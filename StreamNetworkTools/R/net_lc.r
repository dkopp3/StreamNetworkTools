#' NLCD2011 Landcover Percentages
#'
#' Percent of watershed covered by NLCD2011 landcover class
#'
#' Requires VPUAttributeExtension directory see(\code{\link{net_nhdplus}})
#'
#' @param netdelin output from \code{\link{net_delin}}
#' @param vpu NHDPlusV2 Vector Processing Unit
#' @param nhdplus_path Directory for NHDPlusV2 files (\code{\link{net_nhdplus}})
#'
#' @return \code{data.frame} of NLCD2011 classification codes
#'   (\url{https://www.mrlc.gov/nlcd11_leg.php}). Each record is percentage of
#'   catchment area
#'
#' @examples
#' #identify sample comid
#' a <- net_sample(nhdplus_path = getwd(), vpu = "01", ws_order = 6, n = 5)
#' #delineate stream network
#' b <- net_delin(group_comid = as.character(a[,"COMID"]), nhdplus_path = getwd(), vpu = "01")
#' #calculate NLCD2011 Landcover summaries
#' net_lc(netdelin = b, vpu = "01", nhdplus_path = getwd())
#' @export

net_lc <- function(netdelin, vpu, nhdplus_path){
  #set NLCD directory to read in cumalitive climate data (CumTotNLCD2011)
  directory <- grep(paste(vpu, "/VPUAttributeExtension", sep = ""),
                    list.dirs(nhdplus_path, full.names = T),
                    value = T)
  #read in NLCD files
  NLCD.files <- grep("CumTotNLCD", list.files(directory[1], full.names = T), value = T)

  NLCD.2011 <- read.table(NLCD.files[1], header = T, sep = ",")
  names(NLCD.2011) <- toupper(names(NLCD.2011))

  #not including incr
  #IncrNLCD.2011 <- grep("IncrNLCD", list.files(directory[1], full.names = T), value = T)
  #IncrNLCD.2011 <- read.table(IncrNLCD.2011, header = T, sep = ",")
  #change featureID to COMID
  #names(IncrNLCD.2011)[1] <- "COMID"
  #names(IncrNLCD.2011) <- toupper(names(IncrNLCD.2011))

  full.net <- netdelin$Network[, c("net.id", "group.comid", "net.comid", "vpu")]

  #set data.out file
  data.out <- data.frame(unique(full.net[, c("net.id", "group.comid")]))
  NLCD.2011 <- merge(full.net, NLCD.2011, by.x = "net.comid", by.y = "COMID")
  #IncrNLCD.2011 <- merge(full.net, IncrNLCD.2011, by.x = "net.comid", by.y = "COMID")

  #aggregates accumilatied as sum of incremental area divided by total area
  #dont think its correct to agregate like this - I bet scaling all values would produce same percentages
  #tes_incr <- IncrNLCD.2011[IncrNLCD.2011[,"net.id"] == 1, ]
  #tota <- tes_incr[,grep("A", names(tes_incr))]
  #Tarea <- apply(tota[, grep("AT",  names(tota), invert = T)], 1, sum)
  #sum(tes_incr[,"NLCD21A"]) / sum(Tarea) * 100
  #check against cumtot
  #NLCD.2011[NLCD.2011[,"net.id"] == 1, c("NLCD21ACT", "NLCD21AC", "NLCD21PCT", "NLCD21PC")]

  #NLCD values are cum_values
  NLCD.2011 <- NLCD.2011[as.character(NLCD.2011[, "net.comid"]) ==
                           as.character(NLCD.2011[, "group.comid"]), -c(1, 85)]
  NLCD.2011 <- NLCD.2011[, names(NLCD.2011)[grep('CT', names(NLCD.2011), invert = T)], ]
  NLCD.2011 <- NLCD.2011[, names(NLCD.2011)[grep('AC', names(NLCD.2011), invert = T)], ]
  data.out <- merge(data.out, NLCD.2011[,-c(23,24)], by = "net.id", all.x = T)
  # above joins two "group.comid" fields. need to remove and rename
  data.out <- data.out[, -c(3) ]
  names(data.out)[2] <- "group.comid"
  return(data.out)
}
