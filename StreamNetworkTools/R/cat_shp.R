#' Catchment Shape
#'
#' Calculates metrics related to catchment \code{Value}
#'
#' Requires netcat object (see \code{\link{net_cat}})
#'
#' @param netcat output from \code{net_delin}
#'
#' @return a \code{data.frame}: \code{group.comid} of network root node; \code{net.comid}
#'   network flowline comid; and basin.len which is basin lenght
#'
#' @examples
#' g <- cat_shp(netcat = c, vpu = "01", nhdplus_path = getwd())
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
out <- data.frame(group.comid,
               basin_len,
               basin_area,
               basin_width)
if(any(names(obj) == "net.comid")){
  net.comid <- as.character(obj$net.comid)
  out <- data.frame(out, net.comid)
}

return(out)
}
