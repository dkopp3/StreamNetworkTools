#' NHDPlus Catchment
#'
#' Identifies catchments associated with network COMID
#'
#' catchment ploygons can be used to associate landscape attributes to NHDPlusV2
#' flowlines (COMID).
#'
#' Requires NHDPlusCatchment directory
#'
#' see
#' (\url{http://www.horizon-systems.com/NHDPlus/NHDPlusV2_documentation.php})
#' for information about Watershed boundary dataset and its use in NHDPlusV2
#' Only catchments matching network COMID are returned in sf object
#'
#' @param netdelin output form \code{link{net_delin}}
#' @param vpu NHDPlusV2 Vector Processing Unit
#' @param nhdplus_path Directory containing NHDPlusV2 see (\code{net_nhdplus})
#' @param dissolve dissolve catchments \code{(Y/N)}
#'
#' @return named list: \code{$sf_obj} is simple features object;
#'   \code{$group.comid} root COMID of network; \code{$net.comid} COMID upstream
#'   of root COMID; \code{vpu} NHDPlusV2 Vector Processing Unit. \code{$na_cat}
#'   contains comids without catchment \code{$na_cat} records comid's without
#'   catchment (see Details)
#'
#' @examples
#' #' # identify NHDPlusV2 COMID
#' a <- net_sample(nhdplus_path = getwd(), vpu = "01", ws_order = 6, n = 5)
#' # delineate stream network
#' b <- net_delin(group_comid = as.character(a[,"COMID"]), nhdplus_path = getwd(), vpu = "01")
#' C <- net_cat(netdelin = b, vpu = "01", nhdplus_path = getwd(), dissolve = "N")
#' #plot catchments
#' plot(st_geometry(c$sf_obj))
#' #write sf object as shapefile
#' sf::write(c$sf_obj, paste(getwd(),"c.shp",sep = ""))
#' @export

net_cat <- function(netdelin, vpu, nhdplus_path, dissolve){

  dir.spatial <- grep(paste(vpu, "/NHDPlusCatchment", sep = ""),
                      list.dirs(nhdplus_path, full.names = T),
                      value = T)
  dir.spatial <- dir.spatial[grep("NHDPlusCatchment/", dir.spatial, invert = T)]
  catch <- sf::st_read(dir.spatial, layer = "Catchment")
  names(catch)[c(1, 2)] <- toupper(names(catch)[c(1, 2)])
  #some networks do not have matching catchements
  na.cat <- data.frame(vpu = character(), group.comid = character(), net.comid = character())
  ids <- netdelin$Network
  count <- 1

  for (i in unique(ids[ ,"group.comid"])){
    group.comid <- as.character(i)
    net.comid <- as.character(ids[ids[ ,"group.comid"] == i, "net.comid"])
    cat_comid <- dim(catch[catch$FEATUREID %in% net.comid, "FEATUREID"])[1]
    na_len <- length(net.comid[net.comid %in% catch$FEATUREID == F])
    na.cat <- rbind(na.cat, data.frame(net.comid = net.comid[net.comid %in% catch$FEATUREID == F],
                             group.comid = rep(group.comid, na_len),
                             vpu = rep(vpu, na_len)))
    if (cat_comid > 0){
      if (count == 1) {
        save.shp <- catch[catch$FEATUREID %in% net.comid, "FEATUREID"]
        net.ids <- save.shp$FEATUREID
        save.shp <- lwgeom::st_make_valid(save.shp)
        geom <- sf::st_geometry(save.shp)
        save.shp <- sf::st_sf(geom, data.frame(group.comid = rep(group.comid, cat_comid),
                                         net.comid = net.ids,
                                         vpu = rep(vpu, cat_comid)))
      } else {
        temp <- catch[catch$FEATUREID %in% net.comid, "FEATUREID"]
        net.ids <- temp$FEATUREID
        temp <- lwgeom::st_make_valid(temp)
        geom <- sf::st_geometry(temp)
        temp <- sf::st_sf(geom, data.frame(group.comid = rep(group.comid, cat_comid),
                                     net.comid = net.ids,
                                     vpu = rep(vpu, cat_comid)))
        save.shp <- rbind(save.shp, temp)
      }
      count <- count + 1
    } else {
      net.comid <- NA
      na.cat <- rbind(na.cat, cbind(vpu, group.comid, net.comid))
    }
  }

  if (dissolve == "Y"){
    save.shp <- dplyr::group_by(save.shp, group.comid)
    save.shp <- dplyr::summarise(save.shp, subcat_count = length(vpu))
    save.shp <- sf::st_cast(save.shp)
  }

  out.list <- list(sf_obj = save.shp, na_cat = na.cat)
  return(out.list)
}
