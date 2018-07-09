#' Identify NHDPlus Catchment
#'
#' Extracts NHDPlus flowline catchments with optional dissolve
#'
#' Ploygons can be used to associate landscape attributes not included with
#' NHDPlus
#'
#' Requires NHDPlusCatchment directory
#'
#' Some comid's do not have matching catchment (featureID) and some catchments
#' do not have matching comid's (see
#' \url{http://www.horizon-systems.com/NHDPlus/NHDPlusV2_documentation.php})
#'
#' @param netdelin output form \code{link{net_delin}}
#' @param vpu the vector processing unit
#' @param nhdplus_path the directory containing NHDPlus data see(\code{net_nhdplus})
#' @param dissolve collapse subcatchments to create a single network catchmet \code{(Y/N)}
#'
#' @return named list: \code{$sf_obj} is simple features object and \code{$na_cat} records
#'   comid's without catchment
#'
#' @examples
#' d <- net_cat(netdelin = c, vpu = "01", nhdplus_path = getwd(), dissolve = "Y")
#' plot(st_geometry(d$sf_obj))
#' @export

net_cat <- function(netdelin, vpu, nhdplus_path, dissolve){

  dir.spatial <- grep(paste(vpu, "/NHDPlusCatchment", sep = ""),
                      list.dirs(nhdplus_path, full.names = T),
                      value = T)
  dir.spatial <- dir.spatial[grep("NHDPlusCatchment/",dir.spatial,invert=T)]
  catch <- sf::st_read(dir.spatial, layer = "Catchment")
  names(catch)[c(1, 2)] <- toupper(names(catch)[c(1,2)])
  #some networks do not have matching catchements
  na.cat <- data.frame(vpu = character(),
                       group.comid = character(),
                       net.comid = character())
  ids <- netdelin$Network
  count <- 1

  for (i in unique(ids[ ,"group.comid"])){
    group.comid <- as.character(i)
    net.comid <- as.character(ids[ids[ ,"group.comid"] == i, "net.comid"])
    cat_comid <- dim(catch[catch$FEATUREID %in% net.comid, "FEATUREID"])[1]
    na_len<-length(net.comid[net.comid %in% catch$FEATUREID == F])
    na.cat<-rbind(na.cat,
                  data.frame(net.comid = net.comid[net.comid %in% catch$FEATUREID == F],
                             group.comid = rep(group.comid,na_len),
                             vpu = rep(vpu,na_len)))
    if (cat_comid > 0){
      if (count == 1) {
        save.shp <- catch[catch$FEATUREID %in% net.comid, "FEATUREID"]
        net.ids<-save.shp$FEATUREID
        save.shp <- lwgeom::st_make_valid(save.shp)
        geom <- sf::st_geometry(save.shp)
        save.shp <- sf::st_sf(geom,
                              data.frame(group.comid = rep(group.comid, cat_comid),
                                         net.comid=net.ids,
                                         vpu = rep(vpu, cat_comid)))
      } else {
        temp <- catch[catch$FEATUREID %in% net.comid, "FEATUREID"]
        net.ids <- temp$FEATUREID
        temp <- lwgeom::st_make_valid(temp)
        geom <- sf::st_geometry(temp)
        temp <- sf::st_sf(geom,
                          data.frame(group.comid = rep(group.comid, cat_comid),
                                     net.comid=net.ids,
                                     vpu = rep(vpu, cat_comid)))
        save.shp <- rbind(save.shp, temp)
      }
      count <- count + 1
    } else {
      net.comid<-NA
      na.cat <- rbind(na.cat, cbind(vpu, group.comid, net.comid))
    }
  }
  if (dissolve=="Y"){
    save.shp <- dplyr::group_by(save.shp, group.comid)
    save.shp <- dplyr::summarise(save.shp, subcat_count = length(vpu))
    save.shp <- sf::st_cast(save.shp)
  }
  out.list <- list(sf_obj = save.shp, na_cat = na.cat)
  return(out.list)
}
