#' Bankfull Width Estimate
#'
#' Estimates bankfull channel width at comid outlet (unless M-value is
#' specified) from drainage area using regression coefficients in Faustini et al
#' (2009) Geomorphology
#'
#' Requires /NHDPlusAttributes directory (see \code{\link{net_nhdplus}})
#'
#' Drainage area measures are scaled by M values
#'
#' @param netdelin output from \code{net_delin}
#' @param vpu NHDPlusV2 Vector Processing Unit
#' @param nhdplus_path Directory for NHDPlusV2 files (\code{\link{net_nhdplus}})
#'
#' @return \code{data.frame}: \code{$net.id} unique identifier for stream
#'   network; \code{$group.comid} CoMID of stream network root; \code{M}
#'   Position of sampling point on COMID, as proportion of COMID from upstream
#'   end; \code{$AREASQKM} drainage area (km^2) scaled by M value;
#'   \code{est_bfw} estimated bankfull width; \code{bfw_fit} model fit for
#'   bankfull width - drainage area relationship
#'
#' @examples
#' # identify NHDPlusV2 COMID
#' a <- net_sample(nhdplus_path = getwd(), vpu = "01", ws_order = 6, n = 5)
#' # delineate stream network
#' b <- net_delin(group_comid = as.character(a[,"COMID"]), nhdplus_path = getwd(), vpu = "01")
#' calculate topology summary
#' c <- net_bfw(netdelin = b, vpu = "01", nhdplus_path = getwd())
#' @export

net_bfw <- function(netdelin, vpu, nhdplus_path){
    directory <- grep(paste(vpu, "/NHDPlusAttributes", sep = ""),
                      list.dirs(nhdplus_path, full.names = T),
                      value = T)
    Vaa <- grep("PlusFlowlineVAA.dbf",
                list.files(directory[1], full.names = T),
                value = T)
    vaa <- foreign::read.dbf(Vaa)
    names(vaa) <- toupper(names(vaa))

    full.net <- unique(netdelin$Network)

    reach.data <- merge(full.net, vaa, by.x = "net.comid", by.y = "COMID", all.x = T)

    # from Faustini et al 2009; w=aA^b -> logw = log α + β*log(A), where w is
    # bankfull channel width in m and A is drainage area in km2;
    # coefficients provided in Faustini
    width_parms <- StreamNetworkTools::bf_widthcoeff
    width_parms <- width_parms[width_parms[,"vpu"]==vpu,]

    w <- log10(width_parms[,"a"]) + (width_parms[,"b"] * log10(reach.data[,"TOTDASQKM"]))
    w <- 10^w

    data.out <- data.frame(reach.data[, c("vpu", "net.id", "group.comid", "net.comid", "M")],
                           AreaSQKM = reach.data[,"TOTDASQKM"],
                           est_bfw = w, bfw_fit = width_parms[,"Fit"])
    #catchemnts catchment area
    #group by, substract, multiply
    #value at end of flowline
    cat.area <- aggregate(reach.data[, c("AREASQKM", "LENGTHKM")],
                          by = list(net.id = reach.data[,  "net.id"],
                                    group.comid = reach.data[,"group.comid"]),
                          sum)
    incr <- reach.data[as.character(reach.data[,"group.comid"]) ==
                         as.character(reach.data[,"net.comid"]),
                       c("net.id", "net.comid", "AREASQKM","LENGTHKM", "M")]

    incr <- merge(incr, cat.area, by = "net.id")
    area <- (incr[,"AREASQKM.y"] - incr[,"AREASQKM.x"]) + incr[,"AREASQKM.x"]*incr[,"M"]

    #scaled length and catchment vlaues
    cat.area <- data.frame(net.id = incr[,"net.id"], group.comid = incr[,"group.comid"],
                           net.comid = incr[,"net.comid"], M = incr[,"M"], AreaSQKM = area)
    w <- log10(width_parms[,"a"]) + (width_parms[,"b"] * log10(cat.area[,"AreaSQKM"]))
    #back transform
    w <- 10^w
    scaled<-data.frame(vpu = vpu, cat.area, est_bfw = w, bfw_fit = width_parms[,"Fit"])

    #remove and replace root nodes
    data.out <- data.out[as.character(data.out[,"group.comid"]) != as.character(data.out[,"net.comid"]),]
    data.out <- rbind(data.out, scaled)
    data.out <- data.out[order(data.out[,"net.id"]), ]
    data.out[as.character(data.out[,"net.comid"])==as.character(data.out[,"group.comid"]),]
    return(data.out)
  }
