#' Sinuosity and Slope
#'
#' Calculates sinuosity and slope for each reach within a NHDPlusV2 network
#'
#' Can \code{aggregate(...)} reach values to network-scale using group by
#' \code{$net.id}
#'
#' \code{$MaxElevSM, $MinElevSM $SlopeNHDPlus} are values at the outlet of
#' group.comid. \code{tot.len, str.len, sinuosity} are scaled by M value
#'
#' warning message: In st_cast.sf(netdelin_Prj, "LINESTRING") : repeating
#' attributes for all sub-geometries for which they may not be constant is from
#' changing MULTILINE to LINESTRING geometry
#'
#' @param netdelin output from \code{net_delin}
#' @param vpu NHDPlusV2 vector processing unit
#' @param nhdplus_path Directory containing NHDPlusV2 \code{\link{net_nhdplus}}
#'
#' @return \code{data.frame}: \code{$net.comid} upstream reaches from
#'   group.comid; \code{$group.comid} network root COMID; \code{$tot.len} length
#'   of reach; \code{$str.len} is straight line length of reach;
#'   \code{$sinuosity} \code{tot.len} / \code{str.len}; \code{$MaxElevSM}
#'   maximum elevation of reach; \code{$MinElevSM} minimum elevtion of reach;
#'   \code{SlopeNHDPlus} slope of reach
#'
#' @examples
#' # identify NHDPlusV2 COMID
#' a <- net_sample(nhdplus_path = getwd(), vpu = "01", ws_order = 6, n = 5)
#' # delineate stream network
#' b <- net_delin(group_comid = as.character(a[,"COMID"]), nhdplus_path = getwd(), vpu = "01")
#' # calculate sinuosity and slope
#' c <- net_sinu(netdelin = b, vpu = "01", nhdplus_path = getwd())
#' @export

net_sinu <- function (netdelin, nhdplus_path, vpu){
  directory <- grep(paste(vpu, "/NHDPlusAttributes", sep = ""),
                    list.dirs(nhdplus_path, full.names = T),
                    value = T)
  slope <- grep("elevslope.dbf", list.files(directory, full.names = T), value = T)
  slope <- foreign::read.dbf(slope)
  names(slope) <- toupper(names(slope))

  netdelin_Prj <- sf::st_transform(netdelin$SF_Obj, crs = 5070)
  mimaslope <- slope[slope[,"COMID"] %in% netdelin_Prj$COMID,
                     c("COMID", "MAXELEVSMO", "MINELEVSMO", "SLOPE")]
  names(mimaslope) <- c("COMID", "MaxElevSM", "MinElevSM", "SlopeNHDPlus")

  tot.len <- sf::st_length(sf::st_geometry(netdelin_Prj))
  # to ensure the input is linestring geometry -
  # vpu 17 read in a multiline and caused error
  netdelin_Prj <- sf::st_cast(netdelin_Prj,"LINESTRING")
  xy <- sf::st_coordinates(sf::st_geometry(netdelin_Prj))
  xy <- dplyr::group_by(data.frame(xy),L1)
  xy.min <- dplyr::slice(xy, which.min(M))
  xy.max <- dplyr::slice(xy, which.max(M))
  xy <- rbind(xy.min, xy.max)
  xy <- data.frame(xy)
  xy <- xy[order(xy[,"L1"]),]
  xy <- sf::st_as_sf(xy, coords = c(1, 2))
  xy <- dplyr::group_by(xy, L1)
  xy <- dplyr::summarize(xy, dplyr::first(L1))
  xy <- sf::st_cast(xy, "LINESTRING")
  xy <- sf::st_set_crs(xy, 5070)
  str.len <- sf::st_length(xy)

  xy.out <- data.frame(net.id = netdelin_Prj$net.id,
                       group.comid = netdelin_Prj$group.comid,
                       net.comid = netdelin_Prj$COMID,
                       tot.len, str.len, sinuosity = as.numeric(tot.len / str.len))
  xy.out <- merge(xy.out, mimaslope, by.x = "net.comid", by.y = "COMID", all.x = T)
  return(xy.out)
}
