#' Reach Slope and Sinuosity
#'
#' Calculates sinuosity and slope for each network COMID
#'
#' network scale values (i.e. mean) can be derived with \code{aggregate}
#'
#' warning message: In st_cast.sf(netdelin_Prj, "LINESTRING") : repeating
#' attributes for all sub-geometries for which they may not be constant is from
#' vpu 17 when changing MULTILINE to LINESTRING geometry
#'
#' @param netdelin output from \code{net_delin}
#' @param vpu vector processing unit
#' @param nhdplus_path directory containing NHDPlusV2 \code{\link{net_nhdplus}}
#'
#' @return \code{data.frame} with \code{group.comid} as network identifier;
#'   \code{net.comid} as the reach identifier; \code{seg.id} as the valley
#'   segment identifier; \code{tot.len} is the total length of reach;
#'   \code{str.len} is straight line lenght between begining and end of reach;
#'   \code{sinuosity} \code{tot.len} / \code{str.len}; \code{MaxElevSM} maximum
#'   elevation of reach; \code{MinElevSM} minimum elevtion of reach;
#'   \code{SlopeNHDPlus} slope of reach
#'
#' @examples
#' j<-net_sinu(netdelin = c, vpu = "01", nhdplus_path = getwd())
#' @export

net_sinu <- function (netdelin, nhdplus_path, vpu){
  directory <- grep(paste(vpu, "/NHDPlusAttributes", sep = ""),
                    list.dirs(nhdplus_path, full.names = T),
                    value = T)
  slope <- grep("elevslope.dbf",
                list.files(directory,
                           full.names = T),
                value = T)
  slope <- foreign::read.dbf(slope)
  names(slope) <- toupper(names(slope))
  netdelin_Prj <- sf::st_transform(netdelin$SF_Obj,
                                   crs = 5070)
  mimaslope <- slope[slope[,"COMID"] %in% netdelin_Prj$COMID,
                       c("COMID","MAXELEVSMO", "MINELEVSMO", "SLOPE")]
    names(mimaslope)<-c("COMID","MaxElevSM", "MinElevSM", "SlopeNHDPlus")
    tot.len <- sf::st_length(sf::st_geometry(netdelin_Prj))
    # to ensure the input is linestring geometry -
    # vpu 17 read in a multiline and caused error
    netdelin_Prj<-sf::st_cast(netdelin_Prj,"LINESTRING")
    xy <- sf::st_coordinates(sf::st_geometry(netdelin_Prj))
    xy<-dplyr::group_by(data.frame(xy),L1)
    xy.min<-dplyr::slice(xy, which.min(M))
    xy.max<-dplyr::slice(xy, which.max(M))
    xy<-rbind(xy.min, xy.max)
    xy<-data.frame(xy)
    xy<-xy[order(xy[,"L1"]),]
    xy <- sf::st_as_sf(xy, coords = c(1,2))
    xy <- dplyr::group_by(xy, L1)
    xy <- dplyr::summarize(xy, dplyr::first(L1))
    xy <- sf::st_cast(xy, "LINESTRING")
    xy <- sf::st_set_crs(xy, 5070)
    str.len <- sf::st_length(xy)
    xy.out <- data.frame(group.comid = netdelin_Prj$group.comid,
                         net.comid = netdelin_Prj$COMID,
                         tot.len,
                         str.len,
                         sinuosity = as.numeric(tot.len/str.len))
    xy.out<-merge(xy.out, mimaslope, by.x = "net.comid", by.y = "COMID", all.x = T)
head(xy.out)
  return(xy.out)
}
