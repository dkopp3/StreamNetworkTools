#' Random NHDPlusV2 COMID
#'
#' Generates random COMID vector of a specified Strahler order. Can serve as
#' root of steam network (see \code{\link{net_delin}}).
#'
#' \code{"./NHDPlus"} must be a subdirectory of \code{nhdplus_path}.
#'
#' NHDPlusAttributes and NHDSnapshot are required NHDlus downloads (see \code{\link{net_nhdplus}})
#'
#' @param nhdplus_path directory for NHDPlusV2 files \code{\link{net_nhdplus}}
#' @param vpu NHDPlus V2 vector processing unit
#' @param ws_order Strahler order of desired COMID
#' @param n number of COMID's selected
#'
#' @return vector of NHDPlusV2 COMID's.
#'
#' @examples
#' net_sample(nhdplus_path=getwd(), vpu = "01", ws_order = 6, n = 5)
#' @export

net_sample <- function (nhdplus_path, vpu, ws_order, n){
  dir_vaa <- grep(paste( vpu, "/NHDPlusAttributes", sep = ""),
                  list.dirs (nhdplus_path, full.names = T ),
                  value = T)
  dir_hydro <- grep(paste(vpu, "/NHDSnapshot/Hydrography", sep = ""),
                    list.dirs(nhdplus_path, full.names = T),
                    value = T)
  if (length(dir_vaa)==0&length(dir_hydro)==0){
    #z<-paste("invalid nhdplus_path. confirm sub-directory")
      options(error=NULL)
      stop(paste("invalid nhdplus_path. confirm NHDSnapshot and/or NHDPlusAttributes sub-directory"))
    } else {
      vaa <- grep("PlusFlowlineVAA.dbf",
                  list.files(dir_vaa[1], full.names = T),
                  value = T)
      vaa <- foreign::read.dbf(vaa)
      #some column names in NHDPlusV2 attributes are case sensitive
      names(vaa) <- toupper(names(vaa))
      ftype <- grep("NHDFlowline.dbf",
                    list.files(dir_hydro[1], full.names=T),
                    value = T)
      ftype <- foreign::read.dbf(ftype)
      names(ftype) <- toupper(names(ftype))
      # stream segments are identified as unique LEVELPATHI and STREAMORDE.
      # min "HYDROSEQ" ensures the comid selected is the most
      # downstream reach of the segment
      x <- aggregate(vaa[vaa[ ,"STREAMORDE"] == ws_order, "HYDROSEQ"],
                     by = list(LEVELPATHI = vaa[vaa[ ,"STREAMORDE"] == ws_order, "LEVELPATHI"]),
                     function (x) min(x))
      names(x)[2] <- "HYDROSEQ"
      z <- merge(vaa[ ,c("COMID", "LEVELPATHI", "HYDROSEQ", "DIVERGENCE")], x,
                 by = c("LEVELPATHI", "HYDROSEQ"))
      z <- cbind(z[ ,-c(1, 2)], vpu, ws_order)
      z <- merge(z, ftype[ ,c("COMID", "FTYPE")], by = "COMID")
      # To ensure COMID has ftype StreamRiver
      # (not artificial (i.e. lake flow line)) and a major path
      z <- z[z[ ,"DIVERGENCE"] < 2 & z[ ,"FTYPE"] == "StreamRiver", ]
      if (dim(z)[1] >= n){
        z <- sample(z[ ,"COMID"], n)
      } else {
        options(error=NULL)
        stop(paste("sample (", n, ") exceeds total (", length(z[,1]), "): select smaller sample", sep = ""))          }
    }
    return(z)
}
