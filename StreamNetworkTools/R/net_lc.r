#' NLCD2011 Landcover Percentages
#'
#' Network scale landcover percentages
#'
#' Requires VPUAttributeExtension directory see(\code{\link{net_nhdplus}})
#'
#' @param netdelin output from \code{\link{net_delin}}
#' @param vpu vector processing unit
#' @param nhdplus_path directory containing NHDPlus
#'   see(\code{\link{net_nhdplus}})
#'
#' @return \code{data.frame} with NLCD2011 classification codes. Dee
#'   \url{https://www.mrlc.gov/nlcd11_leg.php} for definitions
#'
#' @examples
#' net_lc(netdelin = c, vpu = "01", nhdplus_path = getwd())
#' @export

net_lc<-function(netdelin, vpu, nhdplus_path){
  #set NLCD directory to read in cumalitive climate data (CumTotNLCD2011)
  directory <- grep(paste(vpu, "/VPUAttributeExtension", sep = ""),
                    list.dirs(nhdplus_path, full.names = T),
                    value = T)
  #read in NLCD files
  NLCD.files <- grep("CumTotNLCD",
                     list.files(directory[1], full.names = T),
                     value = T)
  NLCD.2011 <- read.table(NLCD.files[1], header = T, sep = ",")
  names(NLCD.2011) <- toupper(names(NLCD.2011))
  full.net <- netdelin$Network[,c("group.comid", "net.comid", "vpu")]
  #set data.out file
  data.out <- data.frame("COMID" = unique(full.net[,"group.comid"]))
  NLCD.2011 <- merge(full.net, NLCD.2011,
                     by.x = "net.comid",
                     by.y = "COMID")
  #NLCD values are cum_values
  NLCD.2011 <- NLCD.2011[as.character(NLCD.2011[, "net.comid"]) ==
                           as.character(NLCD.2011[, "group.comid"]), -c(1,85)]
  NLCD.2011 <- NLCD.2011[, names(NLCD.2011)[grep('CT', names(NLCD.2011), invert = T)],]
  NLCD.2011 <- NLCD.2011[, names(NLCD.2011)[grep('AC', names(NLCD.2011), invert = T)],]
  data.out <- merge(data.out, NLCD.2011[,-c(23,24)],
                    by.x = "COMID",
                    by.y = "group.comid",
                    all.x = T)
  return(data.out)
}
