#' Network Poisition
#'
#' Calculates distances matrix for each nested (flow connected) network
#'
#' NHDSnapshot and NHDPlusAttributes are required NHDlusV2 files (see
#' \code{\link{net_nhdplus}})
#'
#' Adjusts distances to account for M values
#'
#' @param netdelin output from \code{net_delin}
#' @param nhdplus_path directory for NHDPlusV2 files
#' @param vpu NHDPlusV2 vector Processing Unit
#'
#' @return named \code{list()} of distance matrix (sqKM).  Names are To/From comids with net.id appended
#'
#' @examples
#' #read example locations from VPU 11
#' ExLoc <- read.csv("Sample_Locations.csv")
#' # reorder and rename location data.frame
#' ExLoc <- ExLoc[,c("SiteName","W","N")]
#' names(ExLoc) <- c("SITE_ID","X","Y")
#' #find nearest NHDPlusV2 COMID
#' sam_pts <- net_comid(sample_points = ExLoc, CRS = 4269,
#'                      nhdplus_path = getwd(), vpu = 11, maxdist = 1)
#' b <- net_delin(group_comid = as.character(sam_pts[,"COMID"]), nhdplus_path = getwd(), vpu = "11")
#' c <- net_posit(netdelin = b, nhdplus_path = getwd(), vpu = "11")
#' @export


net_posit <- function (netdelin, nhdplus_path, vpu){

  if(!is.character(vpu)){
    stop("vpu must be character")
  }

  if(dim(netdelin$Nested_COMIDs)[1]<1){
    stop("No flow connected points")
  }

  directory <- grep(paste(vpu, "/NHDPlusAttributes", sep = ""),
                  list.dirs(nhdplus_path, full.names = T), value = T)
  #to/from comids
  flow.files <- grep("PlusFlow.dbf", list.files(directory[1],
                                              full.names = T), value = T)
  flow <- foreign::read.dbf(flow.files)
  #reduce the number of flow to prevent errors in navigation
  flow <- flow[flow[,"TOCOMID"] %in% netdelin$Network[,"net.comid"],]
  Vaa <- grep("PlusFlowlineVAA.dbf",
              list.files(directory[1], full.names=T),
              value = T)
  vaa <- foreign::read.dbf(Vaa)
  names(vaa) <- toupper(names(vaa))

  roots <- unique(netdelin$Nested_COMIDs$root_group.comid)
  #check for multiple m's at root
  #netdelin$Network[netdelin$Network[,c("group.comid")] %in% roots & netdelin$Network[,c("net.comid")] %in% roots, ]
  count <- 1
  out <- list()

  #sf::write_sf(netdelin$SF_Obj, "C:/Users/Darin/Dropbox/Dissertation/StreamResiliencyRCN/Community_Group/path16test.shp")
for (i in roots){
  #i <- 10390290
  comids <- netdelin$Nested_COMIDs[netdelin$Nested_COMIDs$root_group.comid == i, ]
  #get M values here, the iterate through
  path_mvalues <- netdelin$Network[netdelin$Network[,"group.comid"] == netdelin$Network[,"net.comid"], ]
  path_mvalues <- path_mvalues[path_mvalues[,"group.comid"] %in% comids[,"upstream_net.comid"], ]

  path_mvalues$rowid <- apply(path_mvalues[,c("net.comid","net.id")], 1, paste, collapse = "_")

  #calcuate path to root for every comid
  PathsToRoot <- data.frame(TOCOMID = character(), FROMCOMID = character(),
                            length_sqkm_incr = numeric(), pathid = character())
  #getwd()
  #write.csv(PathsToRoot,"C:/Users/Darin/Dropbox/Dissertation/StreamResiliencyRCN/Community_Group/pathstest_ 22751957.csv",row.names=F)
  #clac path and distance to root
  for (row in path_mvalues$rowid){
    #row <- "10274376_40"
    strt <- path_mvalues[path_mvalues$rowid == row, "net.comid"]
    end <- i #column#comids[i, "FROMCOMID"]
    #if start == end youre already at the root = distance is zero (also theres no downstream ToCOMID)
    if (strt != end){
        mvalue <- path_mvalues[path_mvalues$rowid == row, "M"]
        #because moding down stream. M is the proportion from uptream end. distance to outlet is what's remaining
        strt_len <- vaa[vaa[,"COMID"] == strt, "LENGTHKM"] * (1 - mvalue)

        #select next down stream (TOCOMID) from strt
        fcomid <- flow[flow[, "FROMCOMID"] %in% strt, c("TOCOMID", "FROMCOMID")]
        net <- flow[flow[, "FROMCOMID"] %in% strt, c("TOCOMID", "FROMCOMID")]
        len <- vaa[vaa[,"COMID"] %in% fcomid[,"TOCOMID"], c("COMID", "LENGTHKM", "DIVERGENCE")]
        #choose main divergence path if present, otherwise drop divergence
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

        #delineate path to root node of network
        while(fcomid[,"TOCOMID"] != end){
          fcomid <- flow[flow[, "FROMCOMID"] %in% fcomid[, "TOCOMID"], c("TOCOMID", "FROMCOMID")]
          len <- vaa[vaa[,"COMID"] %in% fcomid[,"TOCOMID"], c("COMID", "LENGTHKM","DIVERGENCE")]
          #choose main divergence path if present, otherwise drop divergence
          if(dim(fcomid)[1] > 1){
            len <- len[len[,"DIVERGENCE"] == 1, c("COMID", "LENGTHKM")]
            fcomid <- data.frame(FROMCOMID = unique(fcomid[, "FROMCOMID"]),
                                 TOCOMID = len[, "COMID"])
            len <- len[, "LENGTHKM"]
          } else {
            len <- len[ ,"LENGTHKM"]
          }
          #sum length (value is the distance form upstream location to outlet of downstream comid)
          length_sqkm_incr <- sum(len, max(net[,"length_sqkm_incr"]))
          net <- rbind(net, data.frame(fcomid, length_sqkm_incr))
        }
        #adjust final length to match mvlaue of end
        m_adj_end <- netdelin$Network[netdelin$Network[,"group.comid"] == end &
                                        netdelin$Network[,"net.comid"] == end, "M"]
        rootlen <- vaa[vaa[,"COMID"] == end,"LENGTHKM"]
        net[dim(net)[1],"length_sqkm_incr"] <- (net[dim(net)[1],"length_sqkm_incr"] - rootlen) + (m_adj_end * rootlen)
        #add pathid to facilitate pairwise comparisions
        net <- data.frame(net, pathid = row)
        PathsToRoot <- rbind(PathsToRoot, net)
      } else {
        #"you'll need to add an else statement to account for strt == end"
        #if you addin something here you'll need to rbind again, or move above to abover outside of bracket
        #PathsToRoot <- rbind(PathsToRoot, net)
      }
  }

  print("completed path Distance")
  #make dist matrix here
  #add in M values for paths
  #check paths for multiple ID's
  path_ids <- unique(PathsToRoot[ ,"pathid"])

  distmat <- matrix(NA, length(path_ids) + 1, length(path_ids) + 1)
  colnames(distmat) <- c(i, as.character(path_ids))
  rownames(distmat) <- c(i, as.character(path_ids))
  diag(distmat) <- 0

  #distance to root, 1st row of matrix
  rootdist <- aggregate(PathsToRoot[,"length_sqkm_incr"], by = list(PathsToRoot[,"pathid"]), max)
  distmat[as.character(i), as.character(rootdist[,"Group.1"])] <- rootdist[,"x"]
  distmat[as.character(rootdist[,"Group.1"]), as.character(i)] <- rootdist[,"x"]

  #iterate through the upstream comids pairwise
  for (p in rownames(distmat)[-1]){
    #p <- "10390290"
    for (q in colnames(distmat)[-1]){
      #q <- "10348934_45"

      a <- PathsToRoot[PathsToRoot[, "pathid"] == p, ]
      b <- PathsToRoot[PathsToRoot[, "pathid"] == q, ]

      #decisions to populate matrix distmat distmat[p,q] <- 1
      Da <- any(a[,"FROMCOMID"]%in%b[,"FROMCOMID"] == F)
      Db <- any(b[,"FROMCOMID"]%in%a[,"FROMCOMID"] == F)

      if(Da == T & Db == T){
        #both have extra comids not included
        #find the common downstream comid
        a_not_b <- a[a[, "FROMCOMID"] %in% b[, "FROMCOMID"] == F, ]
        b_not_a <- b[b[, "FROMCOMID"] %in% a[, "FROMCOMID"] == F, ]

        if(dim(a_not_b)[1] == 1){
          #assign Start len value if the is only one record
          mvalue <- path_mvalues[path_mvalues[,"rowid"] == p, "M"]
          #mvalue <- netdelin$Network[netdelin$Network[,"group.comid"] ==  a_not_b[,"FROMCOMID"] &
           #                             netdelin$Network[,"net.comid"] == a_not_b[,"FROMCOMID"], "M"]
            #because moding down stream. M is the proportion from uptream end. distance to outlet is what's remaining
          dist_a <- vaa[vaa[,"COMID"] ==  a_not_b[,"FROMCOMID"], "LENGTHKM"] * (1-mvalue)
        } else {
          #you'll want to pull use one less bc length is at outlet not confluence
          dist_a <- a_not_b[dim(a_not_b)[1] - 1, "length_sqkm_incr"]
        }

        if(dim(b_not_a)[1] == 1){
          #assign Start len value if the is only one record
          mvalue <- path_mvalues[path_mvalues[,"rowid"] == q, "M"]
          #mvalue <- netdelin$Network[netdelin$Network[,"group.comid"] ==  b_not_a[,"FROMCOMID"] &
           #                            netdelin$Network[,"net.comid"] == b_not_a[,"FROMCOMID"], "M"]
          #because moding down stream. M is the proportion from uptream end. distance to outlet is what's remaining
          dist_b <- vaa[vaa[,"COMID"] ==  b_not_a[,"FROMCOMID"], "LENGTHKM"] * (1-mvalue)
        } else {
          #you'll want to pull use one less bc length is at outlet not confluence
          dist_b <- b_not_a[dim(b_not_a)[1] - 1, "length_sqkm_incr"]
        }

        #sum together - distance between p and q
        dist <- sum(dist_a,dist_b)
        distmat[p,q] <- dist
      }
      if(Da == T & Db == F){
        #a had extra comids, b doesnot -> a is upstream of b
        #distance between them is length at the 1st comid of b - need to adjust for M value
        #value at b minus value of a
        #adjust last record in unmatched
        dist <- a[a[,"FROMCOMID"] %in% b[,"FROMCOMID"] == F, ]
        len <- vaa[vaa[,"COMID"] == dist[dim(dist)[1], "TOCOMID"], c("LENGTHKM")]

        mvalue <- path_mvalues[path_mvalues[,"rowid"] == q, "M"]
        #the root is not in the path_mvalues table
        if (length(mvalue)!=1){
          mvalue <- netdelin$Network[netdelin$Network$group.comid == dist[dim(dist)[1], "TOCOMID"] &
                                     netdelin$Network$net.comid == dist[dim(dist)[1], "TOCOMID"], "M"]
        }
        dist <- dist[dim(dist)[1], "length_sqkm_incr"] - len + (mvalue * len)
        distmat[p, q] <- dist
      }
      if(Da == F & Db == T){
        #b had extra comids, a doesnot -> b is upstream of a
        #distance between them is length at the 1st comid of a - need to adjust for M value
        #value at a minus value of b
        dist <- b[b[,"FROMCOMID"] %in% a[,"FROMCOMID"] == F, ]
        len <- vaa[vaa[,"COMID"] == dist[dim(dist)[1], "TOCOMID"], c("LENGTHKM")]

        mvalue <- path_mvalues[path_mvalues[,"rowid"] == p, "M"]

        if (length(mvalue)!=1){
        mvalue <- netdelin$Network[netdelin$Network$group.comid == dist[dim(dist)[1], "TOCOMID"] &
                                   netdelin$Network$net.comid == dist[dim(dist)[1], "TOCOMID"], "M"]
        }
          dist <- dist[dim(dist)[1], "length_sqkm_incr"] - len + (mvalue * len)
        distmat[p, q] <- dist
        }
      if(Da == F & Db == F){
        #neither a or b have extra comid
        if(p!=q){
        #they are on the same comid but have different M_values
        len <- vaa[vaa[,"COMID"] == a[1, "FROMCOMID"], c("LENGTHKM")]
        mvalue <- path_mvalues[path_mvalues[,"rowid"] == p, "M"]
        dista<-len*mvalue
        mvalue <- path_mvalues[path_mvalues[,"rowid"] == q, "M"]
        distb<-len*mvalue
        distmat[p,q] <- max(dista, distb) - min(dista, distb)}
        #distance between them should be zero
      }
    }
  }

  print("comleted dist matrix")
  out[[count]]<-list(distmat)
  count <- count+1
}
names(out) <- as.character(roots)
return(out)
}
