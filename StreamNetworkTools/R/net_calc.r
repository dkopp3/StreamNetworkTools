#' Network Topology
#'
#' Calculates metrics related to stream network topology see \code{Value}
#'
#' Requires /NHDPlusAttributes directory (see \code{\link{net_nhdplus}})
#'
#' Warning: "Check drain.dia. Incomplete headwater path 5848788 at COMID 5848656
#' in network 5848066 perhaps due to waterbody" may indicate that
#' drain.dia is flawed by the presence of a waterbody in the network
#'
#' @param netdelin output from \code{net_delin}
#' @param vpu vector processing unit
#' @param nhdplus_path directory containing NHDPlusV2 \code{\link{net_nhdplus}}
#'
#' @return a \code{data.frame}: \code{COMID} of network root node; \code{vpu}
#'    vector processing unit; \code{maxelev} max elevation of network flowline (cm);
#'   \code{minelev} min elevation of network flowline (cm); \code{releif}
#'   (\code{maxelev} - \code{minelev}); \code{drain.dia} drainage diameter
#'   (longest flowpath in network); \code{head.h2o} number of terminal nodes
#'   (i.e. headwaters); \code{trib.jun} number of tributary junctions
#'   (\code{head.h2o} - 1); \code{AREASQKM} drainage area (km^2);
#'   \code{LENGTHKM} total lenght of network flowlines; \code{drain.den}
#'   drainage density (\code{LENGTHKM} / \code{AREASQKM}); \code{WS.ord}
#'   strahler order for root node; \code{reach.cnt} number of valley segments
#'   (edges); \code{diver.cnt} count of divergent flow paths
#'
#' @examples
#' g <- net_calc(netdelin = c, vpu = "01", nhdplus_path = getwd())
#'
#' @export

net_calc <- function(netdelin, vpu, nhdplus_path ){
  directory <- grep(paste(vpu, "/NHDPlusAttributes", sep = ""),
                    list.dirs(nhdplus_path, full.names = T),
                    value = T)
  Vaa <- grep("PlusFlowlineVAA.dbf",
              list.files(directory[1],full.names=T),
              value = T)
  slope <- grep("elevslope.dbf",
                list.files(directory, full.names=T),
                value = T)
  flow.files <- grep("PlusFlow.dbf", list.files(directory[1],
                                                full.names = T), value = T)
  flow <- foreign::read.dbf(flow.files)
  vaa <- foreign::read.dbf(Vaa)
  slope <- foreign::read.dbf(slope)
  names(slope) <- toupper(names(slope))
  names(vaa) <- toupper(names(vaa))
  full.net <- unique(netdelin$Network)
  reach.data <- Reduce(function(x, y) merge(x, y,
                                            by.x = "net.comid",
                                            by.y = "COMID",
                                            all.x = T),
                       list(full.net, vaa, slope))
  #calculate network order
  WS.ord <- aggregate(reach.data[, "STREAMORDE"],
                      by = list(group.comid = reach.data[, "group.comid"]),
                      max)
  names(WS.ord) <- c("COMID", "WS.ord")
  #catchemnts catchment area
  cat.area <- aggregate(reach.data[, c("AREASQKM", "LENGTHKM")],
                        by = list(COMID = reach.data[, "group.comid"]),
                        sum)
  drain.den <- cat.area[ ,"LENGTHKM"] / cat.area[ ,"AREASQKM"]
  cat.area <- data.frame(cat.area, drain.den)
  #diversion feature count
  #counts minor flow paths of divergences
  div.rm <- reach.data[reach.data[,c("STREAMORDE")] !=
                         reach.data[,"STREAMCALC"]&reach.data[,"DIVERGENCE"]==2,
                       c("net.comid","group.comid")]
  diver.cnt <- aggregate(div.rm[,"group.comid"],
                 by = list(div.rm[,"group.comid"]),
                 length)
  names(diver.cnt) <- c("COMID", "diver.cnt")

  #headwaters & Tribs
  head.h2o <- aggregate(reach.data[reach.data[,"STARTFLAG"] == 1,
                                   "STREAMORDE"],
                        by = list(group.comid =
                                    reach.data[reach.data[,"STARTFLAG"] ==
                                                 1, "group.comid"]),
                        length)
  names(head.h2o) <- c("COMID","head.h2o")
  trib.jun <- as.numeric(as.character(head.h2o[,"head.h2o"])) - 1
  head.h2o <- data.frame(head.h2o, trib.jun)

  #edge count
  edges <- head.h2o[,"head.h2o"]+head.h2o[,"trib.jun"]
  reach.cnt <- data.frame(COMID = head.h2o[,"COMID"], reach.cnt = edges)
  names(reach.cnt) <- c("COMID", "reach.cnt")

  #relief
  maxelev <- aggregate(reach.data[,"MAXELEVSMO"],
                       by = list(reach.data[,"group.comid"]),
                       max)
  minelev <- aggregate(reach.data[, "MINELEVSMO"],
                       by = list(reach.data[, "group.comid"]),
                       min)
  relief <- maxelev[,"x"]-minelev[,"x"]
  relief <- data.frame(COMID = maxelev[,"Group.1"],
                       maxelev = maxelev[,"x"],
                       minelev = minelev[,"x"],
                       releif = relief)

  #aggregate table for summaries of group comid
  data.out <- as.data.frame(unique(full.net[, c("group.comid", "vpu")]))
  names(data.out)[1] <- "COMID"
  data.out<-Reduce(function(x, y) merge(x, y,
                              by = "COMID",
                              all.x = T),
                   list(data.out, relief,head.h2o, cat.area, WS.ord, reach.cnt, diver.cnt))
  return(data.out)
}
