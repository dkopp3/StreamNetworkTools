#' StreamCat Region Download
#'
#' Downloads zip files for VPU from StreamCat webpage
#' \url{https://www.epa.gov/national-aquatic-resource-surveys/streamcat}
#'
#'
#' @param strmcat_path parent directory for download
#' @param Class StreamCat metric class default \code{c("Disturbance",
#'   "Natural")}
#' @param Scale StreamCat metric scale. Default \code{c("Watershed", "Riparian",
#'   "Stream segment")}
#' @param Metric.Type StreamCat metric type. Default
#'   \code{c("Ag/Urban","Agriculture","Bio","Climate",
#'   "Disturbance","Hydrology", "Infrastructure", "Land Cover", "Land Use",
#'   "Lithology", "Mines", "Pollution", "Riparian", "Soil", "Topography",
#'   "Urban", "Water quality", "Wetness"), vpu = "01")}
#' @param vpu NHDPlusV2 Vector Processing Unit (i.e. Region)
#' @param files NHDPlusV2 data file names. Default are used for SNT functions.
#'   See \code{details}
#'
#' @return StreamCat data files are downloaded to "StreamCat" directory
#'
#' @examples
#' strmcat_download(strmcat_path = getwd(), Class = c("Disturbance"), Scale =
#' c("Watershed", "Riparian", "Stream segment"), Metric.Type =
#' c("Infrastructure"), vpu = "01")
#'
#' @export

strmcat_download <- function (strmcat_path = getwd(),
                         Class = c("Disturbance", "Natural"),
                         Scale = c("Watershed", "Riparian", "Stream segment"),
                         Metric.Type =  c("Agriculture","Bio","Climate",
                                          "Disturbance","Hydrology",
                                          "Infrastructure", "Land Cover",
                                          "Land Use", "Lithology", "Mines",
                                          "Pollution", "Riparian", "Soil", "Topography",
                                          "Urban", "Water quality", "Wetness"),
                         vpu = "01"){

  if(!is.character(vpu)){
    stop("vpu must be character")
  }

  # set as internal data devtools::use_data(x, internal = TRUE, overwrite = T)
  strmcatxwalk <- StreamNetworkTools:::strmcatxwalk2

  class <- strmcatxwalk[as.character(strmcatxwalk[, "Class"]) %in% Class, ]

  if(dim(class)[1] == 0){
    stop(paste0("Class argument should include c(\"",
                paste(levels(strmcatxwalk[, "Class"]),
                      collapse = "\", \""), "\")", sep = " "))

  }

  scale <- class[class[,"Scale"] %in% Scale, ]
  #throw error if all metric.type are unavailable

  if(dim(scale)[1] == 0){
    #suggest scales
    stop(paste0("Scale argument should include c(\"",
                paste(as.character(unique(class[,"Scale"])),
                      collapse = "\", \""), "\")",
                " with Class = ", Class , sep = " "))
  }


  #metric tpye query
  metric.type <- scale[scale[, "Metric.Type"] %in% Metric.Type, ]

  #throw error if all metric.type are unavailable
  if(dim(metric.type)[1] == 0){
    stop(paste0("Metric.Type argument should be c(\"",
                paste(as.character(unique(scale[,"Metric.Type"])),
                      collapse = "\", \""), "\")",
                " with Scale = ", paste0(Scale, collapse = ","), " and Class = ",
                paste0(Class, collapse = ",") , sep = " "))
  }

# warnings for unmatched selections
#class

  if(any(Class %in% metric.type[ ,c("Class")] == F)){
    warning(paste("Class = c(\"", Class[Class %in% metric.type[, "Class"] == F],
                  "\") unavailable give selection", sep = ""))
  }

  #scale warning
  if(any(Scale %in% metric.type[ ,c("Scale")] == F)){
    warning(paste("Scale = c(\"", Scale[Scale %in% metric.type[, "Scale"] == F],
                  "\") unavailable give selection", sep = ""))
    }

  # metric.type warning
  if(any(Metric.Type %in% metric.type[ ,c("Metric.Type")] == F)){
    warning(paste("Metric.Type = c(\"", Metric.Type[Metric.Type %in% metric.type[, "Metric.Type"] == F],
                  "\") unavailable give selection", sep = ""))
  }

#create strmcat subdirectory
  strmcat.dir <- paste(strmcat_path, "/StreamCat", sep="")
  if(dir.exists(strmcat.dir)==F){
      dir.create(strmcat.dir)
  }

  #paste file extensions
  files.download <- unique(as.character(metric.type[, "StreamCat.Table.Name"]))
  files.download <- paste0(files.download, "Region", vpu, ".zip")


  #check to see if files exist
  if(any(files.download %in% list.files (strmcat.dir))){
    warning(paste(files.download[files.download %in% list.files (strmcat.dir)]),
            " exist in strmcat_path, not downloading")
    files.download <- files.download[files.download %in% list.files (strmcat.dir)==F]
  }

  print(paste("downloading", length(files.download),
              "StreamCat files for vpu =", vpu, sep = " "))

  #list all file names on StreamCat ftp directory
  ftpPath <- "ftp://newftp.epa.gov/EPADataCommons/ORD/NHDPlusLandscapeAttributes/StreamCat/HydroRegions/"
  url = ftpPath
  filenames = RCurl::getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  filenames <- strsplit(filenames, "\r\n")
  filenames = unlist(filenames)

  for (i in files.download){
    download.file(paste(ftpPath, i, sep= ""),
                  paste(strmcat.dir,i,sep="/"),
                  mode = "wb")
    utils::unzip(paste(strmcat.dir,i,sep="/"), exdir = strmcat.dir)
  }
}
