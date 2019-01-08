#' Identify NHDPlusV2 COMID
#'
#' Identifies NHDPlusV2 COMID closest to sampling point
#'
#' NHDSnapshot and NHDPlusAttributes are required NHDlusV2 files (see
#' \code{\link{net_nhdplus}})
#'
#' CRS examples: \code{CRS = 4269} = NAD83 (see \url{https://epsg.io/4269});
#' \code{CRS = 5070} = NAD83/CONUS Albers (see \url{https://epsg.io/5070})
#'
#' @param sample_points location \code{data.frame}. fields must be ordered and
#'   named as "SITE_ID", "X" and "Y"
#' @param CRS coordinate reference system of locations (see Details)
#' @param nhdplus_path directory for NHDPlusV2 files
#' @param vpu NHDPlusV2 vector Processing Unit
#' @param maxdist search radius around points (m)
#'
#' @return \code{data.frame} with site information (i.e \code{$SITE_ID},
#'   \code{$X}, and \code{$Y}) and \code{$snap_dist} distance (m) to nearest
#'   COMID; \code{$snap_x, $snap_y}, coordinates of nearest COMID vertex;
#'   \code{$M} Position of sampling point on COMID, as proportion of COMID from
#'   upstream end; \code{COMID} common identifer of NHDPlusV2 flowline;
#'   \code{GNIS_NAME} Geographic Names Information System name of COMID;
#'   \code{ApproxTOTDASQKM} drainage area at COMID outlet, may overestimate
#'   drainage area if \code{$M} < 1; and \code{STREAMORDE} Stream order form
#'   NHDPlusV2 Value Added Attributes.
#'
#' @examples
#' #read example locations from VPU 11
#' ExLoc <- read.csv("Sample_Locations.csv")
#' # reorder and rename location data.frame
#' ExLoc <- ExLoc[,c("SiteName","W","N")]
#' names(ExLoc) <- c("SITE_ID","X","Y")
#' #find nearest NHDPlusV2 COMID
#' sam_pts <- net_comid(sample_points = ExLoc, CRS = 4269,
#'                      nhdplus_path = getwd(), vpu = 11, maxdist = 200)
#' @export
#'
net_comid <- function(sample_points, CRS, nhdplus_path, vpu, maxdist){

  if(!is.character(vpu)){
    stop("vpu must be character")
  }

  dir.spatial <- grep(paste(vpu, "/NHDSnapshot/Hydrography", sep = ""),
                      list.dirs(nhdplus_path, full.names = T),
                      value = T)

  if(length(dir.spatial)==0){
    stop(paste("check NHDSnapshot subdirectory"))
  }

  if(names(sample_points)[1] != "SITE_ID" | names(sample_points)[2] != "X" | names(sample_points)[3] != "Y"){
    stop(paste("Name sample points SITE_ID, X, Y"))
  }

  NHDFlowline <- sf::st_read(dir.spatial, layer = "NHDFlowline", quiet = T)
  NHDFlowline <- sf::st_transform(NHDFlowline, crs = (5070))
  NHDFlowline <- sf::st_zm(NHDFlowline, drop = T, what = "ZM")
  geom <- sf::st_geometry(NHDFlowline)
  sf::st_geometry(NHDFlowline)<-NULL
  names(NHDFlowline) <- toupper(names(NHDFlowline))
  NHDFlowline <- sf::st_sf(NHDFlowline,geom=geom)


  directory <- grep(paste(vpu, "/NHDPlusAttributes", sep = ""),
                    list.dirs(nhdplus_path, full.names = T),
                    value = T)
  Vaa <- grep("PlusFlowlineVAA.dbf",
              list.files(directory[1], full.names=T),
              value = T)
  vaa <- foreign::read.dbf(Vaa)
  names(vaa) <- toupper(names(vaa))

  sample_points <- sf::st_as_sf(sample_points, coords = c(2,3), crs = CRS)
  sample_points <- sf::st_transform(sample_points, crs = 5070)

  #search Radius around each point
  #Consider adding while statement here
  #can save 40sec by not reading in file every time
  #that will be version 1.1 :)
  rad <- sf::st_buffer(sample_points, dist = maxdist)
  #supresses warning message from intersection.
  #assumes attributes are constant throughout the geometries
  sf::st_agr(rad) = "constant"
  sf::st_agr(NHDFlowline) = "constant"
  int <- sf::st_intersection(rad, NHDFlowline)

  sites <- as.character(unique(int$SITE_ID))
  out <- data.frame(SITE_ID = character(), X = numeric(), Y = numeric(),
                    M = numeric(), snap_dist = numeric(),
                    snap_x = numeric(), snap_y = numeric(),
                    COMID = character(), GNIS_NAME = character(),
                    vpu = character(), TOTDASQKM = numeric(),
                    STREAMORDE = numeric(), COMMENTS = character())
  #each site
  for (i in sites){
    #i<-sites[2]
    #identify point for site i, and create sf object
    if(dim(sf::st_coordinates(sample_points[sample_points$SITE_ID == i, ]))[1]>1){
      stop(paste("duplicated SITE_ID", i))
    }
    p <- sf::st_sfc(sf::st_point(sf::st_coordinates(sample_points[sample_points$SITE_ID == i, ])),
                    crs = 5070)
    geom <- sf::st_geometry(p)
    p <- sf::st_sf(geom, data.frame(id = as.character(i)))

    #identify which comid's are in search radius
    comids <- int[int$SITE_ID == i, c("COMID", "GNIS_NAME")]
    sf::st_geometry(comids) <- NULL
    if (length(comids[,1]) > 1){ #iterate through possible comids to find closest
      count <- 1
      for (q in comids[, "COMID"]){
        if(count == 1){
          l <- sf::st_coordinates(int[int$SITE_ID == i & int$COMID == q, ])
          COMID <- q
          GNIS_NAME <- as.character(comids[comids[, "COMID"] == q, "GNIS_NAME"])
          l <- data.frame(X = as.numeric(l[, c("X")]),
                          Y = as.numeric(l[, c("Y")]),
                          COMID = as.character(COMID),
                          GNIS_NAME = as.character(GNIS_NAME))
        } else {
          temp <- sf::st_coordinates(int[int$SITE_ID == i & int$COMID == q, ])
          COMID <- q
          GNIS_NAME <- as.character(comids[comids[, "COMID"] == q, "GNIS_NAME"])
          temp <- data.frame(X = as.numeric(temp[, c("X")]),
                             Y = as.numeric(temp[, c("Y")]),
                             COMID = as.character(COMID),
                             GNIS_NAME = as.character(GNIS_NAME))

          l <- rbind(l, temp)
        }
        count <- count + 1
      }
    } else {
      l <- sf::st_coordinates(int[int$SITE_ID == i, ])
      COMID <- comids[, "COMID"]
      GNIS_NAME <- as.character(comids[, "GNIS_NAME"])
      l <- data.frame(X = as.numeric(l[, c("X")]),
                      Y = as.numeric(l[, c("Y")]),
                      COMID = as.character(COMID),
                      GNIS_NAME = as.character(GNIS_NAME))
    }
    l <- as.data.frame(l) #coordinates for all flowline verteies withing search buffer
    #need to find which vertex is closes to original point
    for (j in 1:length(l[,1])){
      if (j == 1){
        lp <- sf::st_sfc(sf::st_point(apply(l[j, c(1, 2)], 2, as.numeric)), crs = 5070)
        geom <- sf::st_geometry(lp)
        lp <- sf::st_sf(geom, data.frame(id = as.character(l[j, "COMID"])))

      } else {
        temp <- sf::st_sfc(sf::st_point(apply(l[j, c(1, 2)], 2, as.numeric)), crs = 5070)
        geom <- sf::st_geometry(temp)
        temp <- sf::st_sf(geom, data.frame(id = as.character(l[j, "COMID"])))
        lp <- rbind(temp, lp)
      }
    }

    dist <- sf::st_distance(p, lp)
    colnames(dist) <- paste(lp$id, c(1:length(lp$id)), sep = "_")
    rownames(dist) <- p$id
    snap_dist <- apply(dist, 1, min)

    snap_vert <- sf::st_coordinates(lp[which(as.numeric(dist[1, ]) == as.numeric(snap_dist)), ])
    colnames(snap_vert) <- c("snap_X", "snap_Y")

    snap_vert <- snap_vert[1, ]
    COMID_GNIS <- l[l[, "X"] == snap_vert["snap_X"] & l[, "Y"] == snap_vert["snap_Y"],
                    c("COMID", "GNIS_NAME")]

    COMID_GNIS <- COMID_GNIS[1, ]
    COMMENTS <- NA

    #match with VAA HERE
    #drainage density no matching...
    DA <- vaa[vaa[,"COMID"] == COMID_GNIS[, "COMID"], c("TOTDASQKM", "STREAMORDE")]

    #consider adding in comments section.
    if(length(DA[,1]) == 0){
      DA<-data.frame(TOTDASQKM = NA,  STREAMORDE = NA)
      COMMENTS <- "Point on Isolated Network (No VAA)"
    }

    snap_vert <- sf::st_transform(lp[which(as.numeric(dist[1, ]) == as.numeric(snap_dist)), ], crs = CRS)
    #sf::write_sf(snap_vert, "C:/Users/Darin/Dropbox/Dissertation/Chapter_3_Distance_Deposition/RobertsData/test_5.shp")

    snap_vert <- sf::st_coordinates(snap_vert)
    colnames(snap_vert) <- c("snap_X", "snap_Y")
    snap_vert <- data.frame(snap_x = snap_vert[1,1], snap_y = snap_vert[1,2])

    SITE_ID <- as.character(p$id)
    orig_xy <- sf::st_transform(p, CRS)
    orig_xy <- sf::st_coordinates(orig_xy)
    vpu <- vpu

    #M #find comid. Make new line from
    totline <- sf::st_coordinates(NHDFlowline[NHDFlowline$COMID == COMID_GNIS[, "COMID"], c("COMID", "GNIS_NAME")])

    #same as vertex above w/ no transformation snap_vert
    sv <- sf::st_coordinates(lp[which(as.numeric(dist[1, ]) == as.numeric(snap_dist)), ])
    #thich vertex matches snaped points

    ppts <- which(totline[,"X"] == sv[1, 1] & totline[,"Y"] == sv[1, 2])

    #vertices - if the buffer (search radius intessects line and does not include an existing vertex)
    #will produce a new coordinate that needs to be added to added to totline
    if(length(ppts) == 0){ #insert vertex... after closest vertex
      #calc distance between snappped vertex and totline vertecies
      asew <- sf::st_distance(sf::st_point(sv), sf::st_as_sf(data.frame(totline), coords = c("X","Y")))
      #vertex closes to new point index
      minx <- which.min(asew)
      #if the closest point is the most down stream vertex of flowline
      #put new point before end of the line
      if(minx == dim(totline)[1]){
        totline <- rbind(totline[c(1:minx-1), c(1,2)], sv)
        rownames(totline) <- NULL
        ppts <- which(totline[,"X"] == sv[1, 1] & totline[,"Y"] == sv[1, 2])
        } else if (minx == 1){# if closest point is most upstream, append to end of line
          totline <- rbind(totline[c(1:minx), c(1,2)], sv)
          rownames(totline) <- NULL
          ppts <- dim(totline)[1]

        } else {#compare the slope upstream and down stream vertices

          focal <- totline[minx, c(1,2)]
          upstr <- totline[minx-1, c(1,2)]
          dwnstr <- totline[minx+1, c(1,2)]
          slope_us <- (focal[2] - upstr[2]) / (focal[1] - upstr[1])

          slope_ds <- (focal[2] - dwnstr[2]) / (focal[1] - dwnstr[1])
          slope_new <- (focal[2] - sv[2]) / (focal[1] - sv[1])
          # these values are in meters... you are discussing sub meter measuresments
          # change this to format numbers
          #below ws format numbers as == use if throws another
          #slope_ds <- as.numeric(format((focal[2] - dwnstr[2]), digits = 10))/
           # as.numeric(format((focal[1] - dwnstr[1]), digits = 10))
          #slope_new <- as.numeric(format((focal[2] - sv[2]), digits = 10))/
           # as.numeric(format((focal[1] - sv[1]), digits = 1))

          if(isTRUE(all.equal(slope_us, slope_new))){
            #put point upstream of minx
            totline <- rbind(totline[c(1:minx-1), c(1,2)], sv)
            rownames(totline) <- NULL
            ppts <- dim(totline)[1]

          } else if (isTRUE(all.equal(slope_ds, slope_new))){
            #put point down stream
            totline <- rbind(totline[c(1:minx), c(1,2)], sv)
            rownames(totline) <- NULL
            ppts <- which(totline[,"X"] == sv[1, 1] & totline[,"Y"] == sv[1, 2])

          } else {
            #when all else fails, put the point at min difference
            usds <- which.min(data.frame(abs(abs(slope_ds) - abs(slope_new)),
                                         abs(abs(slope_us) - abs(slope_new))))
            if(usds == 1){
              #put point down stream
              totline <- rbind(totline[c(1:minx), c(1,2)], sv)
              rownames(totline) <- NULL
              ppts <- which(totline[,"X"] == sv[1, 1] & totline[,"Y"] == sv[1, 2])
            } else {
              #put point upstream of minx
              totline <- rbind(totline[c(1:minx-1), c(1,2)], sv, totline[minx:dim(totline)[1], c(1, 2)])
              rownames(totline) <- NULL
              ppts <- dim(totline)[1]
              }
            }

          }
        }
          if(!is.na(COMMENTS)){#updata COmments field
            COMMENTS <- paste(COMMENTS,"Created New Vertex", sep = "; ")
            } else {
              COMMENTS <- "Created New Vertex"
            }

    #means the closest vertex is upstream end of comid.
    #include next downstream vertex
    if(ppts == 1){
      ppts <- 2
      if(!is.na(COMMENTS)){
        COMMENTS <- paste(COMMENTS,"Moved to closest Vertex Downstream of COMID Start", sep = "; ")
      } else {
      COMMENTS <- "Moved to closest Vertex Downstream of COMID Start"
      }
    }
    #the line from snapped vertex to upstream confluence/headwater
    line_length <- sf::st_length(sf::st_sfc(
      sf::st_linestring(apply(totline[c(1:ppts), c(1:2)], 2,
                              as.numeric)), crs = 5070))

    #total comid line length #nee
    tot_length <- sf::st_length(NHDFlowline[NHDFlowline$COMID == COMID_GNIS[, "COMID"],
                                            c("COMID", "GNIS_NAME")])
    M <- as.numeric(line_length / tot_length)
    temp <- data.frame(snap_dist, snap_vert, M, SITE_ID,
                       orig_xy, vpu, COMID_GNIS, DA, COMMENTS,
                       row.names = NULL)
    out <- rbind(temp, out)
  }

  head(out)

  #unmatched points
  um <- sample_points[sample_points$SITE_ID %in% out[,"SITE_ID"] == F, ]

    if(length(um$SITE_ID) > 0){
      xy <- sf::st_transform(um, CRS)
      xy <- sf::st_coordinates(xy)
      sf::st_geometry(um) <- NULL
      out[,"SITE_ID"] <- as.character(out[,"SITE_ID"])
      out <- rbind(out, data.frame(SITE_ID = um$SITE_ID, xy, snap_dist = NA, M = NA,
                                   snap_x = NA, snap_y = NA, vpu = NA, COMID = NA,
                                   GNIS_NAME = NA, TOTDASQKM = NA, STREAMORDE = NA,
                                   COMMENTS = "Unmatched to COMID"))
  }
  out <- out[, c("SITE_ID", "X", "Y", "snap_dist", "snap_x", "snap_y","M",
                 "COMID", "GNIS_NAME", "vpu", "TOTDASQKM","STREAMORDE", "COMMENTS")]
  names(out) <- c("SITE_ID", "X", "Y", "snap_dist", "snap_x", "snap_y","M",
                "COMID", "GNIS_NAME", "vpu", "ApproxTOTDASQKM","STREAMORDE", "COMMENTS")
  return(out)
}

