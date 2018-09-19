#' Network Delineation
#'
#' Delineates stream network upstream of root NHDPlus COMID (see
#' \code{\link{net_sample}} or \code{\link{net_comid}} or
#' \url{http://www.horizon-systems.com/NHDPlus/NHDPlusV2_data.php}) via upstream
#' navigation
#'
#' Requires /NHDPlusAttributes and NHDSnapshot directories (see
#' \code{\link{net_nhdplus}})
#'
#' All input group comids are delineated. If nested comids are unexpected or
#' unwanted, remove from \code{$SF_Obj} and \code{$Network} before proceeding
#'
#' @param group_comid a vector of NHDPlus COMIDs
#' @param nhdplus_path nhdplus_path directory for downloaded NHDPlus files
#'   (\code{\link{net_nhdplus}})
#' @param vpu vector processing unit
#' @param M Indcates position of samppling point on flowline for geach group
#'   COMID generated from \code{\link{net_comid}}.
#'
#' @return netdelin, a named list (\code{$Network, $Nested_COMIDs, $SF_Obj})
#'   which serves as an argeument for subsequent functions in StreamNetworkTools.
#'   \code{$Network} stores network comids, \code{Nested_COMIDs} stores comids
#'   which are in the same network, and SF_Obj is a simple feature object of the
#'   networks.
#'
#' @examples
#' a <- net_comid(sample_points = z, CRS = 4269, nhdplus_path = getwd(), vpu="01", maxdist = 100)
#' b <- net_delin(group_comid = as.character(a[,"COMID"]), nhdplus_path = getwd(), vpu = "01")
#' write_sf(b$SF_Obj...)
#' @export

net_delin <- function (group_comid, nhdplus_path, vpu, M = NULL) {

  if(is.character(group_comid) == F){
      stop("group_comid must be character vector")
  }

  Ms <- M
  if (length(Ms) == 0) {
    Ms<- rep(1,length(group_comid))
    } else if (length(Ms)!= length(group_comid)) {
      stop("length(M)!=length(group_comid)")
    }

  if (any(duplicated(group_comid))) {
    warning(paste("duplicated comid:",
                  group_comid[duplicated(group_comid)],
                  "network delineated once", sep = " "))
  }
  group_comid <- unique(group_comid)
  directory <- grep(paste(vpu, "/NHDPlusAttributes", sep = ""),
                    list.dirs(nhdplus_path, full.names = T), value = T)
  #to/from comids
  flow.files <- grep("PlusFlow.dbf", list.files(directory[1],
                                                full.names = T), value = T)
  flow <- foreign::read.dbf(flow.files)
  Vaa <- grep("PlusFlowlineVAA.dbf",
              list.files(directory[1], full.names=T),
              value = T)
  vaa <- foreign::read.dbf(Vaa)
  names(vaa) <- toupper(names(vaa))
  dir.spatial <- grep(paste(vpu, "/NHDSnapshot/Hydrography", sep = ""),
                      list.dirs(nhdplus_path, full.names = T),
                      value = T )
  NHDFlowline <- sf::st_read(dir.spatial, layer = "NHDFlowline")
  #some names are case sensitive
  names(NHDFlowline)[1] <- toupper(names(NHDFlowline)[1])
  network <- data.frame(group.comid = character(),
                        net.comid = character(),
                        vpu = character(),
                        M = numeric ())
  #nested<-NULL
  #delineate network for each group COMID
  for (i in 1:length(group_comid)) {
    net <- group_comid[i]
    fcomid <- group_comid[i]
    while (length(flow[flow[, "TOCOMID"] %in% fcomid, "FROMCOMID"]) >= 1) {
      fcomid <- flow[flow[, "TOCOMID"] %in% fcomid, "FROMCOMID"]
      fcomid <- fcomid[fcomid != 0]
      net <- append(fcomid, net, length(net))
    }
    group.comid <- group_comid[i]
    net.comid <- unique(net[order(net)])
    M <- ifelse(group.comid==net.comid, Ms[i] , 1)
    network <- rbind(network, data.frame(group.comid = group.comid,
                                         net.comid = net.comid,
                                         vpu = vpu,
                                         M = M))
    }
    #check for nested COMID's and display warning()
    q <- aggregate(network[,"net.comid"], by =list(network[,"net.comid"]), length)
    head(q)
    w <- unique(network[network[,"net.comid"] %in% q[q[, "x"] > 2, "Group.1"], ])
    alarm <- list()
    if(length(w[,1])>0){
      for (h in unique(w[,"net.comid"])){
      o <- list(as.character(w[w[, "net.comid"] == h, "group.comid"]))
      alarm[[h]] <- o
    }
    alarm<-unique(alarm)
    warning("check nested comid's listed in $Nested_COMIDs" )
    }
    count <- 1
    for (p in levels(network[,"group.comid"])){
        if (count == 1) {
          save.shp <- NHDFlowline[NHDFlowline$COMID %in% network[network[,"group.comid"]==p, "net.comid"], "COMID"]
          save.shp$group.comid <- p
          save.shp$VPUID <- vpu
        } else {
          temp <- NHDFlowline[NHDFlowline$COMID %in% network[network[,"group.comid"]==p, "net.comid"], "COMID"]
          temp$group.comid <- p
          temp$VPUID <- vpu
          save.shp <- rbind(save.shp, temp)
        }
      count<-count+1
      }
    out <- list(Network = network,
                Nested_COMIDs = alarm,
                SF_Obj = save.shp)
    return(out)
}
#-----------------------------------------------------
#' Identify Network Segments
#'
#' modifies \code{\link{net_delin}} by creating an index (\code{seg.id}) for
#' multiple comids occuring between confluences
#'
#' \code{seg.id} are arbitrarily assigned to network segments (i.e. there is no
#' up/down stream ordering to seg.id with a network)
#'
#' requires /NHDPlusAttributes directory (see \code{\link{net_nhdplus}})
#'
#' optional with respect to other functions within StreamNetworkTools
#'
#' @param netdelin output from \code{\link{net_delin}}
#' @param nhdplus_path nhdplus_path directory for downloaded NHDPlus files
#'   (\code{\link{net_nhdplus}})
#' @param vpu vector processing unit
#'
#' @return
#' modifies \code{link{net_delin}} with additional field \code{seg.id}.
#'
#' @examples
#' net_segid(netdelin = b, nhdplus_path = getwd(), vpu = "01")

#assign uniquid to edges
net_segid <- function(netdelin, nhdplus_path, vpu){
  directory <- grep(paste(vpu, "/NHDPlusAttributes", sep = ""),
                    list.dirs(nhdplus_path, full.names = T),
                    value = T)
  #to/from comids
  flow.files <- grep("PlusFlow.dbf",
                     list.files(directory[1],
                                full.names = T),
                     value = T)
  flow <- foreign::read.dbf(flow.files)
  Vaa <- grep("PlusFlowlineVAA.dbf",
              list.files(directory[1],
                         full.names = T),
              value = T)
  vaa <- foreign::read.dbf(Vaa)
  names(vaa) <- toupper(names(vaa))
  segid.out <- data.frame(group.comid = character(),
                        net.comid = character(),
                        seg.id = character())
  full.net <- netdelin$Network
  full.net <- merge(full.net,
                    vaa[,c("COMID", "STREAMORDE", "STREAMCALC")],
                    by.x = "net.comid",
                    by.y = "COMID")

  for (d in levels(netdelin$Network[,"group.comid"])){
    #removes divergences
    net <- full.net[full.net[, "group.comid"] == d &
                      full.net[, "STREAMORDE"] ==
                      full.net[, "STREAMCALC"], ]
    net.flow <- flow[flow[, "TOCOMID"] %in%
                       net[, "net.comid"]&
                       flow[, "FROMCOMID"] %in%
                       net[, "net.comid"],
                      c("TOCOMID", "FROMCOMID")]
    heads <- flow[flow[, "TOCOMID"] %in%
                    net[, "net.comid"] &
                    flow[, "FROMCOMID"] == 0,
                  c("TOCOMID", "FROMCOMID")]
    net.flow <- rbind(net.flow,heads)
    if(length(net.flow[,1]) > 0){
      #when TOCOMID's are duplicated = confluence
      conv <- aggregate(net.flow[,"TOCOMID"],
                        by = list(TOCOMID = net.flow[, "TOCOMID"]),
                        length)
      #these COMID's diverge into multiple TOCOMID's
      conv <- conv[conv[, "x"] > 1 & conv[, "TOCOMID"] != 0, "TOCOMID"]
    } else {
      conv <- 0
    }
    #if there are no confluences, no need to do anything else
    if(length(conv) > 0){
      #add zaro as headwater indicator
      seg.id <- 0
      out <- data.frame(group.comid = character(),
                        net.comid = character(),
                        seg.id = character(),
                        conv = character())
      for (j in conv){
        #how many lines are flowing into convluence
        segs <- net.flow[net.flow[, "TOCOMID"] %in% j, "FROMCOMID"]
        #each confluence has a path that ends at an upstream confluence
        for (p in segs){
          net <- p
          fcomid <- p
          #all comid's floing to a focal comid are headwaters
          if (all(net.flow[net.flow[, "TOCOMID"] %in%
                           fcomid, "FROMCOMID"]) == 0){
            #consider adding in seg type (e.g. headwater)
          } else if (fcomid %in% conv){
            #kicks ouif id the path directly joins confluence
            #all comid's floing to a focal comid are from confluences
            } else if (any(net.flow[net.flow[, "TOCOMID"] %in%
                                  fcomid, "FROMCOMID"] %in%
                         conv)) {
            #consider adding in seg type (e.g. edge)
            #need to append from comid flowing from the confluence (i.e.)
              net <- append(net.flow[net.flow[, "TOCOMID"] %in%
                                       fcomid, "FROMCOMID"],
                            net, length(net))

              } else {
              #did the upstream comid originate form confluence
                while(any(net.flow[net.flow[, "TOCOMID"] %in%
                                   fcomid, "FROMCOMID"] %in%
                          conv) == F & all(fcomid != 0)){
                  fcomid <- net.flow[net.flow[, "TOCOMID"] %in% fcomid, "FROMCOMID"]
                  if (any(fcomid != 0)){
                    fcomid <- fcomid[fcomid != 0]
                    net <- append(fcomid, net, length(net))
                    }
                  }
            #if the path ends at a conv, need to apppend net congluence...
                if (any(net.flow[net.flow[, "TOCOMID"] %in%
                                 fcomid, "FROMCOMID"] %in%
                        conv)){
                  net <- append(net.flow[net.flow[, "TOCOMID"] %in%
                                       fcomid, "FROMCOMID"],
                            net, length(net))
            }
          }
          #net.flow[net.flow[,"FROMCOMID"]==7733411,]
          out <- rbind(out, data.frame(group.comid = d,
                                       net.comid = net,
                                       seg.id = seg.id,
                                       conv = j))
          seg.id <- seg.id + 1
          #aggregate(out[,"net.comid"],by=list(out[,"net.comid"]),length)
        }
      }
      #add in root node
      net <- d
      fcomid <- d
      if(fcomid %in% conv){
        #if d is in the name of the confluence therese only one COMID
      } else {
        while (any(net.flow[net.flow[, "TOCOMID"] %in%
                            fcomid, "FROMCOMID"] %in%
                   conv) == F) {
          fcomid <- net.flow[net.flow[, "TOCOMID"] %in%
                               fcomid, "FROMCOMID"]
          fcomid <- fcomid[fcomid != 0]
          net <- append(fcomid, net, length(net))
        }
        #need to append the confluence here
        net <- append(net.flow[net.flow[, "TOCOMID"] %in%
                                 fcomid, "FROMCOMID"],
                      net, length(net))
      }
      out<-rbind(out,data.frame(group.comid = d,
                                net.comid = net,
                                seg.id = seg.id,
                                conv = "ROOT"))
    } else {
      out<-data.frame(group.comid = d,
                      net.comid = net[, "net.comid"],
                      seg.id = seg.id,
                      conv = "ROOT")
    }
    segid.out <- rbind(out, segid.out)

  }
  netdelin$Network <- merge(netdelin$Network,
                            segid.out,
                            by = c("group.comid","net.comid"),
                            all.x = T)
  netdelin$Network[is.na(netdelin$Network[,"conv"]),
                   c("seg.id","conv")] <- "9999"
  netdelin$SF_Obj <- merge(netdelin$SF_Obj, segid.out,
                           by.x = c("group.comid","COMID"),
                           by.y = c("group.comid","net.comid"),
                           all.x = T)
  netdelin$SF_Obj$seg.id[is.na(netdelin$SF_Obj$seg.id)] <- "9999"
  netdelin$SF_Obj$conv[is.na(netdelin$SF_Obj$conv)] <- "9999"
  return(netdelin)
}
