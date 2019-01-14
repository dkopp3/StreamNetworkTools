#' Nearest confluence to sampling point
#'
#' Calculates distance to nearest confluence and appends confluence attributes
#'
#' @param netdelin output from \code{net_delin}
#' @param netconflu output form \code{net_conflu}
#' @param vpu NHDPlusV2 Vector Processing Unit
#' @param nhdplus_path directory containing NHDPlusV2 \code{net_nhdplus}
#'
#' @return \code{data.frame}: \code{net.id} unique network identifier;
#'   \code{group.comid} root COMID for network; \code{vpu} Vector Processing
#'   Unit; \code{X_trib} and \code{Y_trib} confluence coordinates;
#'   \code{trib_order} order of COMID downstream of confluence;
#'   \code{area_ratio} darinage areas ratios (i.e. Triburaty Drainage Area /
#'   Mainstem Drainage Area); \code{trib_area} drainage area upstream of
#'   confluence; \code{junction_num} is a concatenation of of strahler stream
#'   orders of confluence reaches (e.g 11 is concluence of 2 frist orders, and
#'   23 is a second order joining a thrid order); alpha is the angle of
#'   (degrees) tributary junction; \code{complex} indicates complex triburaty
#'   junction, \code{trib_comid} is the comid down stream of confluence;
#'   \code{dist_km} is distance from sampling point to confluence
#'
#' @examples
#' # identify NHDPlusV2 COMID
#' a <- net_sample(nhdplus_path = getwd(), vpu = "01", ws_order = 6, n = 5)
#' # delineate stream network
#' b <- net_delin(group_comid = as.character(a[,"COMID"]), nhdplus_path = getwd(), vpu = "01")
#' # calculate confluence metrics
#' h <- net_conflu(netdelin = c, vpu = "01", nhdplus_path = getwd())
#' j <- conflu_dist(netdelin = b, netconflu = h, vpu = "01", nhdplus_path = getwd())
#'
#' @export

conflu_dist <- function (netdelin, netconflu, nhdplus_path, vpu){

  if(!is.character(vpu)){
    stop("vpu must be character")
  }

  directory <- grep(paste(vpu, "/NHDPlusAttributes", sep = ""),
                    list.dirs(nhdplus_path, full.names = T), value = T)
  #to/from comids
  flow.files <- grep("PlusFlow.dbf", list.files(directory[1],
                                                full.names = T), value = T)
  flow <- foreign::read.dbf(flow.files)
  Vaa <- grep("PlusFlowlineVAA.dbf",
              list.files(directory[1], full.names = T),
              value = T)
  vaa <- foreign::read.dbf(Vaa)
  names(vaa) <- toupper(names(vaa))

  flow_all <- flow#flow[flow[,"TOCOMID"] %in% netdelin$Network[,"net.comid"], ]

  #remove all comids with confluences on same comid
  conflu_dis <- merge(netdelin$Network[netdelin$Network[,"group.comid"] == netdelin$Network[, "net.comid"],
                              c("group.comid", "M", "net.id")], netconflu[,-2], by.x = c("group.comid","net.id"),
                      by.y = c("trib_comid", "net.id"), all.x = T)

    samplepts <- conflu_dis[is.na(conflu_dis[,"vpu"]), c("group.comid","net.id")]
  samplepts <- samplepts[complete.cases(samplepts),]
  conflu_dis <- conflu_dis[!is.na(conflu_dis[,"complex"]),]

  conflu_dis$trib_comid <- conflu_dis[, "group.comid"]
  dist <- merge(conflu_dis[,c("group.comid", "M","net.id")], vaa[,c("COMID","LENGTHKM")], by.x = "group.comid", by.y = "COMID")
  dist$dist_km <- dist[,"LENGTHKM"] * dist[,"M"]
  conflu_dis <- merge(conflu_dis,dist[,c("net.id", "dist_km")], by.x = "net.id")

  if(dim(samplepts)[1]>1){
  #dist(mvalueXlenght)
  for (i in 1:nrow(samplepts)){
    #return point information and confluence info (including distance)
    comids <- samplepts [i,]
    root_Node <- comids[,"group.comid"]
    net.id <- comids[, "net.id"]
    conflu <- netconflu[netconflu[,"net.id"] == net.id,]
    row.names(conflu) <- NULL
    networkflow <- netdelin$Network[netdelin$Network$net.id == net.id,]

    flow <- flow_all[flow_all[,"TOCOMID"] %in% networkflow[,"net.comid"], ]

    out <- data.frame(group.comid = character(),  M = numeric(), net.id = character(),
                      trib_comid= character(), vpu= character(), X= numeric(), Y = numeric(),
                      trib_order = numeric(), area_ratio = numeric(), trib_area= numeric(),
                      junction_num = numeric(), alpha = numeric(), complex = numeric(),
                      dist_km = numeric())

    for (j in unique(conflu[,"trib_comid"])){

      #start at confluence
      strt <- j
      strt_len <- vaa[vaa[,"COMID"] %in% strt, "LENGTHKM"]
      if (j == root_Node){
        #if the confluence is directly above the root
        #stop and modify length by M value (confluence is top, so M x length)
        mvalue <- netdelin$Network[netdelin$Network$group.comid == root_Node &
                                   netdelin$Network$net.comid == root_Node, "M"]
        dist <- strt_len * mvalue

        temp <- data.frame(group.comid = root_Node,  M = mvalue,
                           conflu[conflu[,"trib_comid"] == j, -2], dist_km = dist)
        out <- rbind(temp, out)

      } else {
      #downstream navigation (F->T)
      #what comid is downstream form J
      fcomid <- flow[flow[, "FROMCOMID"] %in% strt, c("TOCOMID", "FROMCOMID")]
      net <- flow[flow[, "FROMCOMID"] %in% strt, c("TOCOMID", "FROMCOMID")]
      len <- vaa[vaa[,"COMID"] %in% fcomid[,"TOCOMID"], c("COMID", "LENGTHKM", "DIVERGENCE")]

      if(dim(fcomid)[1] > 1){
        len <- len[len[,"DIVERGENCE"] == 1, c("COMID", "LENGTHKM")]
        fcomid <- data.frame(TOCOMID = len[, "COMID"], FROMCOMID = unique(fcomid[,"FROMCOMID"]))
        net <- fcomid
        len <- len[, "LENGTHKM"]
      } else {
        len <- len[ ,"LENGTHKM"]
      }

      #sum length (value is the distance form upstream location to outlet of downstream comid)
      len <- sum(len, strt_len)
      net <- data.frame(net, length_sqkm_incr = len)

      while(all(fcomid[,"TOCOMID"] != conflu[,"trib_comid"]) & fcomid[,"TOCOMID"] != root_Node){
        fcomid <- flow[flow[, "FROMCOMID"] %in% fcomid[, "TOCOMID"], c("TOCOMID", "FROMCOMID")]
        len <- vaa[vaa[,"COMID"] %in% fcomid[,"TOCOMID"], c("COMID", "LENGTHKM","DIVERGENCE")]
        if(dim(fcomid)[1] > 1){
          len <- len[len[,"DIVERGENCE"] == 1, c("COMID", "LENGTHKM")]
          fcomid <- data.frame(TOCOMID = len[, "COMID"], FROMCOMID = unique(fcomid[,"FROMCOMID"]))
          len <- len[, "LENGTHKM"]
        } else {
          len <- len[ ,"LENGTHKM"]
        }

        #sum length (value is the distance form upstream location to outlet of downstream comid)
        length_sqkm_incr <- sum(len, max(net[,"length_sqkm_incr"]))
        net <- rbind(net, data.frame(fcomid, length_sqkm_incr))
      }
      #does the last value in net == the group comid or a confluence
      if(net[dim(net)[1],"TOCOMID"] == root_Node){
        root_len <- vaa[vaa[,"COMID"] == root_Node,"LENGTHKM"]
        mvalue <- netdelin$Network[netdelin$Network$group.comid == root_Node&
                                     netdelin$Network$net.comid == root_Node, "M"]
        dist <- (net[dim(net)[1],"length_sqkm_incr"] - root_len) + (root_len*mvalue)
        temp <- data.frame(group.comid = root_Node,  M = mvalue,
                           conflu[conflu[,"trib_comid"] == j, -2], dist_km = dist)
        out <- rbind(temp, out)
      }
      }
    }
    out <- out[which.min(out[,"dist_km"]), ]
    conflu_dis <- rbind(conflu_dis, out)
    #endtime <- Sys.time()
    #print(endtime - srt.time)
  }
  }
    #add values w/out confluences

  Noconflu <- unique(netdelin$Network[netdelin$Network[,c("net.id")] %in% conflu_dis[,"net.id"]==F,
                                      c("group.comid", "net.id","M","vpu")])
  if(dim(Noconflu)[1] > 0){
  conflu_dis <- rbind(conflu_dis, data.frame (Noconflu, X = NA, Y = NA, trib_order = NA,
                                              area_ratio = NA, trib_area = NA,
                                              junction_num = NA, alpha = NA, complex = NA,
                                              trib_comid = NA, dist_km = NA  ))
  }
  names(conflu_dis)[grep(paste0("X","|","Y"), names(conflu_dis))]<-c("X_trib", "Y_trib")

return(conflu_dis[order(conflu_dis[,"net.id"]),])
}
