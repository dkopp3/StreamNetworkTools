#' Network Confluence Attributes
#'
#' Calculates metrics related to network confluences see \code{Values}
#'
#' requires "/NHDPlusAttributes" directory \code{\link{net_nhdplus}}
#'
#' netdelin projection is transformed to Lambert Conformal Cone (crs = 102004)
#' to preserve angles
#'
#' See Benda et al. (2004) for Triburaty : Mainstem Drainage Ratio and Seybold et
#' al. (2017) for confluence angle calculation
#'
#' @param netdelin output from \code{net_delin}
#' @param vpu vector processing unit
#' @param nhdplus_path directory containing NHDPlusV2 \code{\link{net_nhdplus}}
#'
#' @return \code{data.frame} \code{group.comid} as network identifier;
#'   \code{net.comid} as the reach identifier; \code{vpu} as vector processing
#'   unit; \code{CRS} is coordinate reference system for \code{X,Y};
#'   \code{trib_order} order of COMID downstream of confluence;
#'   \code{area_ratio} ratio of the darinage areas of each reach in a
#'   confluence (i.e. Triburaty Drainage Area / Mainstem Drainage Area);
#'   \code{trib_area} the drainage area downstream of confluence;
#'   \code{junction_num} is a concatenation of of strahler stream orders of
#'   confluence reaches (e.g 11 is concluence of 2 frist orders, and 23 is a
#'   second order joining a thrid order); alpha is the avarage tributary
#'   junction; \code{complex} indicates complex triburaty junction
#'
#' @examples
#' h <- net_conflu(netdelin = c, vpu = "01", nhdplus_path = getwd())
#' @export

net_conflu <- function (netdelin, nhdplus_path, vpu){
  directory <- grep(paste(vpu, "/NHDPlusAttributes", sep = ""),
                    list.dirs(nhdplus_path, full.names = T), value = T)
  flow.files <- grep("PlusFlow.dbf",
                     list.files(directory[1], full.names = T),
                     value = T)
  Vaa <- grep("PlusFlowlineVAA.dbf",
              list.files(directory[1], full.names = T),
              value = T)
  vaa <- foreign::read.dbf(Vaa)
  names(vaa) <- toupper(names(vaa))
  flow<-foreign::read.dbf(flow.files)
  NHD <- sf::st_zm(netdelin$SF_Obj, drop = T, what = "ZM")
  #Lambert Conformal Cone - preserves angles as per seybold et al 2017
  NHD <- sf::st_transform(NHD, crs = 102004)
  net <- netdelin$Network
  net <- merge(net,
               vaa[, c("COMID", "STREAMORDE",
                       "STREAMCALC","STREAMLEVE",
                       "TOTDASQKM")],
               by.x = "net.comid",
               by.y = "COMID")
  t_angle <- data.frame(group.comid = character(),
                        trib_comid = character(),
                        vpu = character(),
                        X = numeric(),
                        Y = numeric(),
                        CRS = numeric(),
                        alpha = numeric(),
                        area_ratio = numeric(),
                        StreamOrde = numeric(),
                        trib_area = numeric(),
                        junction_num=numeric(),
                        complex = numeric())
  for(i in levels(net[, "group.comid"])){
    z <- net[net[, "group.comid"] == i, ]
    z <- z[z[, "STREAMORDE"] == z[, "STREAMCALC"], ]
    for (j in z[,"net.comid"]){
      t <- flow[flow[, "TOCOMID"] == j, c("TOCOMID", "FROMCOMID")]
      #information about streams upstream of confluence junction
      t <- merge(t, z[,c("net.comid","STREAMORDE",
                         "STREAMCALC","STREAMLEVE",
                         "TOTDASQKM")],
                 by.x = "FROMCOMID",
                 by.y = "net.comid")
      #TO is the trib index - holds the information for two tribs
      if(length(t[, 1]) == 2){
        area_ratio <- min(t[, "TOTDASQKM"]) / max(t[, "TOTDASQKM"])
        if(any(duplicated(t[,"STREAMORDE"]))){
          trib_order <- unique(t[, "STREAMORDE"]) + 1
        } else {
          trib_order <- max(t[, "STREAMORDE"])
        }
        trib_area <- sum(t[, "TOTDASQKM"])
        junction_num <- as.numeric(paste(t[order(t[, "STREAMORDE"]), "STREAMORDE"], collapse = ""))
        #coordinates
        trib <- NHD[NHD$group.comid == i &
                      NHD$COMID %in%
                      t$FROMCOMID, ]
        coords <- sf::st_coordinates(trib)
        xy <- aggregate(coords[, c("X", "Y")],
                        by = list(coords[, c("X")],
                                  coords[, c("Y")]),
                        length)
        xy <- xy[xy[, "X"] > 1, c(1, 2)]
        colnames(xy) <- c("X", "Y")
        x1 <- coords[coords[, "L1"] == 1, c(1)]
        y1 <- coords[coords[, "L1"] == 1, c(2)]

        #orthogonal regression (total) on x y coords
        #average tributary direction
        v <- prcomp(cbind(x1, y1))$rotation
        beta1 <- v[2,1] / v[1,1]
        y_int1 <- mean(y1) - beta1 * (mean(x1))
        new_xy1_1 <- cbind(min(x1), beta1 * min(x1)+y_int1)
        new_xy1_2 <- cbind(max(x1), beta1 * max(x1)+y_int1)
        #direction vector
        vec_xy1 <- new_xy1_1 - new_xy1_2
        #repeats for sqcond trib
        x2 <- coords[coords[, "L1"] == 2, c(1)]
        y2 <- coords[coords[, "L1"] == 2, c(2)]
        v <- prcomp(cbind(x2, y2))$rotation
        beta2 <- v[2, 1] / v[1, 1]
        y_int2 <- mean(y2) - beta2 * (mean(x2))
        new_xy2_1 <- cbind(min(x2), beta2 * min(x2) + y_int2)
        new_xy2_2 <- cbind(max(x2), beta2 * max(x2) + y_int2)
        vec_xy2 <- new_xy2_1 - new_xy2_2

        #calculate angel from direction vectors of two tribs
        alpha <- acos(sum(vec_xy1 * vec_xy2) /
                        (sqrt(sum(vec_xy1^2)) *
                           sqrt(sum(vec_xy2^2))))
        #angles in radians convert to degrees
        alpha <- (alpha * 180) / (pi)
        temp<-data.frame(group.comid = i,
                         trib_comid = j,
                         vpu = vpu,
                         CRS = 102004,
                         xy, trib_order,
                         area_ratio,
                         trib_area,
                         junction_num,
                         alpha = alpha,
                         complex = 0)
        t_angle <- rbind(t_angle, temp)
        } else if (length(t[ ,1]) > 2 ){
          #complex junctions... not sure how to handel these
          trib <- NHD[NHD$group.comid == i & NHD$COMID %in% t$FROMCOMID, ]
          coords <- sf::st_coordinates(trib)
          xy <- aggregate(coords[,c("X","Y")],
                          by=list(coords[,c("X")],
                                  coords[,c("Y")]),
                          length)
          xy <- xy[xy[, "X"] > 1, c(1, 2)]
          colnames(xy) <- c("X", "Y")
          junction_num <- as.numeric(paste(t[order(t[, "STREAMORDE"]),
                                             "STREAMORDE"], collapse = ""))
          temp <- data.frame(group.comid = i,
                             trib_comid = j,
                             vpu = vpu,
                             xy,
                             alpha = NA,
                             area_ratio = NA,
                             trib_order = NA,
                             trib_area = NA,
                             junction_num,
                             CRS=102004,
                             complex = length(t[ ,1]))
          t_angle <- rbind(t_angle, temp)
        }
    }
  }
  return(t_angle)
}

  #plotting example
  #plot(coords[,c(1,2)])
  #points(coords[coords[,"L1"]==2,c(1,2)], col="red")
  #points(xy,pch=3)
  #abline(a=y_int1,b = beta1)
  #abline(a=y_int2,b = beta2)
  #points(rbind(new_xy2_1,new_xy2_2),pch=3)

