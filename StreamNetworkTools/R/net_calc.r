#' Network Topology Metrics
#'
#' Calculates stream network topology metrics
#'
#' Requires /NHDPlusAttributes directory (see \code{\link{net_nhdplus}})
#'
#' Length and area measures are scaled by M values
#'
#' @param netdelin output from \code{net_delin}
#' @param vpu NHDPlusV2 Vector Processing Unit
#' @param nhdplus_path Directory for NHDPlusV2 files (\code{\link{net_nhdplus}})
#'
#' @return \code{data.frame}: \code{$group.comid} stream network root COMID;
#'   \code{$vpu} NHDPlusV2 vector processing unit;\code{M} Position of sampling
#'   point on COMID, as proportion of COMID from upstream end; \code{WS.ord}
#'   strahler order for root node;\code{$head.h2o} number of headwater reaches;
#'   \code{$trib.jun} number of tributary junctions; \code{reach.cnt} number of
#'   reaches in network; \code{diver.cnt} count of divergent flow paths;
#'   \code{$AREASQKM} drainage area (km^2); \code{$LENGTHKM} total lenght of
#'   network flowlines (km); \code{drain.den} drainage density (\code{LENGTHKM}
#'   / \code{AREASQKM})
#'
#' @examples
#' # identify NHDPlusV2 COMID
#' a <- net_sample(nhdplus_path = getwd(), vpu = "01", ws_order = 6, n = 5)
#' # delineate stream network
#' b <- net_delin(group_comid = as.character(a[,"COMID"]), nhdplus_path = getwd(), vpu = "01")
#' calculate topology summary
#' c <- net_calc(netdelin = b, vpu = "01", nhdplus_path = getwd())
#' @export

net_calc <- function(netdelin, vpu, nhdplus_path){
  directory <- grep(paste(vpu, "/NHDPlusAttributes", sep = ""),
                    list.dirs(nhdplus_path, full.names = T),
                    value = T)
  Vaa <- grep("PlusFlowlineVAA.dbf",
              list.files(directory[1], full.names = T),
              value = T)
  slope <- grep("elevslope.dbf",
                list.files(directory, full.names = T),
                value = T)
  flow.files <- grep("PlusFlow.dbf",
                     list.files(directory[1], full.names = T),
                     value = T)

  flow <- foreign::read.dbf(flow.files)
  vaa <- foreign::read.dbf(Vaa)
  slope <- foreign::read.dbf(slope)
  names(slope) <- toupper(names(slope))
  names(vaa) <- toupper(names(vaa))

  full.net <- unique(netdelin$Network)

  reach.data <- Reduce(function(x, y)
    merge(x, y, by.x = "net.comid", by.y = "COMID", all.x = T),
    list(full.net, vaa, slope))

  #calculate network order
  WS.ord <- reach.data[as.character(reach.data[,"group.comid"]) ==
                         as.character(reach.data[,"net.comid"]),
                       c("net.id","M", "STREAMORDE")]

  names(WS.ord) <- c("net.id", "M", "WS.ord")

  #catchemnts catchment area
  #group by, substract, multiply
  #value at end of flowline
  cat.area <- aggregate(reach.data[, c("AREASQKM", "LENGTHKM")],
                        by = list(net.id = reach.data[,  "net.id"],
                                  group.comid = reach.data[,"group.comid"]),
                        sum)
  incr <- reach.data[as.character(reach.data[,"group.comid"]) ==
                       as.character(reach.data[,"net.comid"]),
                     c("net.id", "AREASQKM","LENGTHKM", "M")]

  incr <- merge(incr, cat.area, by = "net.id")
  area <- (incr[,"AREASQKM.y"] - incr[,"AREASQKM.x"]) + incr[,"AREASQKM.x"]*incr[,"M"]
  len <- (incr[,"LENGTHKM.y"] - incr[,"LENGTHKM.x"]) + incr[,"LENGTHKM.x"]*incr[,"M"]

  #scaled length and catchment vlaues
  cat.area <- data.frame(net.id = incr[,"net.id"],
                         AreaSQKM = area, LengthKM = len)

  drain.den <- cat.area[ ,"LengthKM"] / cat.area[ ,"AreaSQKM"]
  cat.area <- data.frame(cat.area, drain.den)

  #diversion feature count
  #counts minor flow paths of divergences
  if (any(reach.data[,c("STREAMORDE")] !=
          reach.data[,"STREAMCALC"] &
          reach.data[,"DIVERGENCE"]==2)){

    div.rm <- reach.data[reach.data[,c("STREAMORDE")] !=
                           reach.data[,"STREAMCALC"] &
                           reach.data[, "DIVERGENCE"] == 2,
                         c("net.id", "net.comid", "group.comid")]

    diver.cnt <- aggregate(div.rm[, "group.comid"],
                           by = list(div.rm[,"net.id"]),
                           length)

    names(diver.cnt) <- c("net.id", "diver.cnt")

  } else {
    diver.cnt <- data.frame(net.id = 99999, diver.cnt = 999999)
  }

  #headwaters & Tribs
  head.h2o <- aggregate(reach.data[
    reach.data[,"STARTFLAG"] == 1, "STREAMORDE"],
    by = list(reach.data[reach.data[,"STARTFLAG"] == 1, "net.id"]),
    length)

  names(head.h2o) <- c("net.id", "head.h2o")

  trib.jun <- as.numeric(as.character(head.h2o[, "head.h2o"])) - 1
  head.h2o <- data.frame(head.h2o, trib.jun)

  #edge count
  edges <- head.h2o[,"head.h2o"] + head.h2o[,"trib.jun"]
  reach.cnt <- data.frame(net.id = head.h2o[,"net.id"], reach.cnt = edges)

  #relief - at outlet; I want to move this to basin metric
  #maxelev <- aggregate(reach.data[,"MAXELEVSMO"],
  #                    by = list(reach.data[,"group.comid"]),
  #                   max)
  #minelev <- aggregate(reach.data[, "MINELEVSMO"],
  #                    by = list(reach.data[, "group.comid"]),
  #                   min)
  #relief <- maxelev[,"x"]-minelev[,"x"]
  #relief <- data.frame(COMID = maxelev[,"Group.1"],
  #                    maxelev = maxelev[,"x"],
  #                   minelev = minelev[,"x"],
  #                  releif = relief)

  #aggregate table for summaries of group comid
  data.out <- unique(full.net[, c("net.id","group.comid", "vpu")])

  names(data.out)[2] <- "COMID"

  data.out <- Reduce(function(x, y)
    merge(x, y, by = "net.id", all.x = T),
    list(data.out, WS.ord,head.h2o, reach.cnt, diver.cnt, cat.area))#, relief))

  names(data.out)[2] <- "group.comid"

  return(data.out)
}
