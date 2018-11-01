#' Network StreamCat Data
#'
#' Extracts StreamCat variables for network
#'
#' @param strmcat_path directory containing StreamCat data
#' @param netdelin see \code{net_delin}
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
#'
#' @return \code{data.frame} of streamCat variables
#'
#'   function still underdevelopment  Need to imporve crosswalk table
#'
#' @examples
#' strmcat_download(strmcat_path = getwd(), Class = c("Disturbance"), Scale =
#' c("Watershed", "Riparian", "Stream segment"), Metric.Type =
#' c("Infrastructure"), vpu = "01")
#' net_comid()
#' net_strmcat(netdelin = )
#' @export

net_strmcat <- function (netdelin, strmcat_path = getwd(),
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

  #strmcat_path <- "C:/Users/Darin/Dropbox/Dissertation/Network_Modeling/Actual/StreamCat"
  strmcat.dir <- grep("StreamCat", list.dirs(strmcat_path), value = T)

  if(length(strmcat.dir)==0){
    stop(paste("No StreamCat directory in", strmcat_path))
  }

  #paste file extensions
  files.download <- unique(as.character(metric.type[, "StreamCat.Table.Name"]))
  files.download <- paste0(files.download, "Region", vpu, ".csv")

  #check to see if files exist
  if(any(files.download %in% list.files (strmcat.dir))==F){
    stop(paste("missing", files.download[any(files.download %in% list.files (strmcat.dir))==F]))
  }

  full.net <- netdelin$Network
  out <- data.frame(net.comid= character(), group.comid = character(), vpu = character(), M = numeric(), net.id = numeric(),
                    StreamCat.Table.Name= character(),
                    CatArea = numeric(), WsArea = numeric(), WsPct = numeric(), CatPct= numeric(), metric.name = character(),
                    StrmCat_value = numeric())
  count<-0
  for (i in files.download){
  # read in, merge with netdelin,
    count<-count+1
    print(paste("processing", count, "of", length(files.download)))
    strmdat <- read.csv(paste(strmcat.dir, i, sep = "/"))

    m <- merge(full.net, strmdat, by.x = "net.comid", by.y = "COMID", all.x = T)
    m <- m[as.character(m[,"net.comid"]) == as.character(m[,"group.comid"]), ]

  if(length(grep("CatArea", names(m)))!=0){
    CatArea <- m[,grep("CatArea", names(m))]
  } else {
    CatArea<- NA
  }
  if(length(grep("WsArea", names(m)))!=0){
    WsArea <- m[,grep("WsArea", names(m))]
  } else {
    WsArea<- NA
  }
  if(length(grep("CatPct", names(m)))!=0){
    CatPct <- m[,grep("CatPct", names(m))]
  } else {
    CatPct<- NA
  }
  if(length(grep("WsPct", names(m)))!=0){
    WsPct <- m[,grep("WsPct", names(m))]
  } else {
    WsPct<- NA
  }

  #file.q <-
  #need to do a better job parcing landscape metrics
  #info <- strmcatxwalk2[strmcatxwalk2[,""]==file.q,
   #                     c("Landscape.Metrics", "Units", "Class", "Metric.Type", "StreamCat.Table.Name")]
  StreamCat.Table.Name <- unlist(strsplit(i,"Region"))[1]
  #lapply(strsplit(as.character(info[,"Landscape.Metrics"])," – "),"[[",1)

  metric.values <- grep(c("COMID|CatArea|WsArea|CatPct|WsPct"), names(strmdat),  invert = T, value = T)
    for (j in metric.values){
      metric.name <- j
      StrmCat_value <- m[,j]

    out <- rbind(out, data.frame(m[,c("net.comid","group.comid","vpu","M","net.id")], StreamCat.Table.Name,
                 CatArea, WsArea, WsPct, CatPct, metric.name, StrmCat_value))
    }
  }
  return(out)
}
  #need variable description with cross walk table... to be continued
  #head(out)
  #unique(out[,"metric.name"])
  #xwalk <- (StreamNetworkTools:::strmcatxwalk2[,"Landscape.Metrics"])
  #needed str split twice bc different - after deg idfk
  #z <- unlist(lapply(strsplit(as.character(xwalk), "–"),"[[",1))
  #z <- unlist(lapply(strsplit(as.character(z), "-"), "[[", 1))
#View(z)
