#' Catchment Shape
#'
#' Calculates metrics related to catchment shape very slowly.
#'
#' @param netcat output from \code{\link{net_cat}}
#'
#' @return \code{data.frame}: \code{group.comid} root COMID of network;
#'   \code{net.comid} COMID upstream of root COMID, indexes catchments;
#'   \code{$basin_len} length of basin, longest distance between two catchment
#'   vertices; \code{$basin_area} area of catchment; \code{$basin_width}
#'   basin_area / basin_len
#'
#' @examples
#' # identify NHDPlusV2 COMID
#' a <- net_sample(nhdplus_path = getwd(), vpu = "01", ws_order = 6, n = 5)
#' # delineate stream network
#' b <- net_delin(group_comid = as.character(a[,"COMID"]), nhdplus_path = getwd(), vpu = "01")
#' #identify catchments
#' c <- net_cat(netdelin = b, vpu = "01", nhdplus_path = getwd(), dissolve = "N")
#' calculate basin shape
#' d <- cat_shp(netcat = c, vpu = "01", nhdplus_path = getwd())
#'
#' @export

cat_shp<-function(netcat){
  # project to albers
  obj <- st_transform(netcat$sf_obj, 5070)

distance.func <- function(polygon){
  polygon <- st_sfc(polygon)
  max(st_distance(st_cast(polygon, "POINT")))
}

dist <- lapply(st_geometry(obj), distance.func)
basin_len <- units::set_units(unlist(dist),"meters")
basin_area <- st_area(obj)
basin_width <- basin_area / basin_len
group.comid <- as.character(obj$group.comid)
out <- data.frame(group.comid, basin_len, basin_area, basin_width)

if(any(names(obj) == "net.comid")){
  net.comid <- as.character(obj$net.comid)
  out <- data.frame(out, net.comid)
}

out <- out[,c("group.comid", "net.comid", "basin_len", "basin_area", "basin_width")]

return(out)
}
