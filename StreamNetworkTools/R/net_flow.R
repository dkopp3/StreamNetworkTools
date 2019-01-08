#' Network Discharge and Veolcity Summary
#'
#' Network flow summaries derived form NHDPlusV2 EROM mean annual and mean
#' monthly discharge (Q0001E) and velocity (V0001E) estimates
#'
#' Requires /VPUAttributeExtension directory see (\code{\link{net_nhdplus}})
#'
#' If M value is supplied, discharge (Q0001E) values are scaled proportionally.
#' Velocity (V0001E) and RUNOFFVC are values for COMID outlet
#' \code{\link{net_delin}}
#'
#'@param netdelin output from \code{\link{net_delin}}
#'@param vpu NHDPlusV2 Vector Processing Unit
#'@param nhdplus_path Directory for NHDPlusV2 files (\code{\link{net_nhdplus}})
#'
#'@return \code{data.frame}: \code{$RUNOFFVC} cumulative mean annual runoff
#'  (mm); \code{$MAQ0001E} Mean Annual discharge (cf); \code{$minMMQ0001E}
#'  minimum mean monthly discharge; \code{$maxMMQ0001E} maximum mean monthly
#'  discharge (cf);\code{$covMMQ0001E} coeffficient of variation of mean monthly
#'  discharge; \code{$V0001E} mean annual velocity (cfs); \code{minMMV0001E}
#'  minimum mean monthly velocity (cfs) \code{maxMMV0001E} maximum mean monthly
#'  velocity;\code{covMMV0001E} coefficient of variation in mean monthly velocity
#'  estimates.
#'
#' @examples
#' # identify NHDPlusV2 COMID
#' a <- net_sample(nhdplus_path = getwd(), vpu = "01", ws_order = 6, n = 5)
#' # delineate stream network
#' b <- net_delin(group_comid = as.character(a[,"COMID"]), nhdplus_path = getwd(), vpu = "01")
#' # derive discharge and velocity estimates
#' c <- net_flow(netdelin = b, vpu = "01", nhdplus_path = getwd())
#'@export

net_flow <- function (nhdplus_path, vpu, netdelin, Mscale = "N"){
  directory <- grep(paste(vpu, "/NHDPlusAttributes", sep = ""),
                    list.dirs(nhdplus_path, full.names = T), value = T)
    full.net <- netdelin$Network
    data.out <- as.data.frame(unique(full.net[,c("net.id", "group.comid", "vpu")]))

    clim.dir <- grep(paste(vpu, "/VPUAttributeExtension", sep = ""),
                   list.dirs(nhdplus_path, full.names = T),
                   value = T)

  # Runofff and discharge CUMMA...
    ROMA.files <- grep("CumTotROMA", list.files(clim.dir[1], full.names = T), value = T)
    ROMA <- read.table(ROMA.files, header = T, sep = ",")
    names(ROMA) <- toupper(names(ROMA))
    ROMA <- merge (full.net, ROMA, by.y = "COMID", by.x = "net.comid")
    cumtotROMA <- ROMA[as.character(ROMA[,"net.comid"]) == as.character(ROMA[,"group.comid"]),
                       c("net.id","RUNOFFVC")]
    data.out <- merge(data.out, cumtotROMA, by = "net.id", all.x = T)


    EROM.dir <- grep(paste(vpu, "/EROMExtension", sep = ""),
                     list.dirs(nhdplus_path, full.names = T),
                     value = T)
    erom <- grep("EROM_", list.files(EROM.dir[1], full.names = T), value = T)

  #read files function
  read_EROM_files <- function (file_type){
    Q <- foreign::read.dbf(file_type[1])
    colnames(Q) <- toupper(colnames(Q))
    colnames(Q)[1] <- "COMID"
    #set month of dataframe, pulls off last name in filepath
    month <- unlist(lapply(strsplit(file_type[1], "/"), "[[", lengths(strsplit(file_type[1], "/"))))
    month <- unlist(lapply(strsplit(month,"\\."), "[[", 1))
    month <- substr(month, nchar(month) - 5, nchar(month)-4)

    #cbind month to ppt data
    Q <- data.frame(Q, month = month)
    #merge with fill.net to maintain group comid.
    #Unique values are now set as net.comid.group.comid & month
    Q <- merge(full.net, Q, by.y = "COMID", by.x = "net.comid")
    #loop over remainder files
    for (j in 2:length(file_type)) {
      month <- unlist(lapply(strsplit(file_type[j],"/"), "[[",lengths(strsplit(file_type[j], "/"))))
      month <- unlist(lapply(strsplit(month,"\\."),"[[", 1))
      month <- substr(month, nchar(month) - 5, nchar(month)-4)

      temp <- foreign::read.dbf(file_type[j])
      colnames(temp) <- toupper(colnames(temp))
      colnames(temp)[1] <- "COMID"
      temp <- data.frame(temp, month = month)
      temp <- merge (full.net, temp, by.y = "COMID", by.x = "net.comid")
      Q <- rbind(Q, temp)
    }

    return(Q)
  }

  erom_dat <- read_EROM_files(erom)

  #add option to scale by proportion of flow line
  if (Mscale == "N"){
    erom_dat[,"M"] <- 1
  }

#discharge values aggregated as sum of incremental
cumtot <- erom_dat[as.character(erom_dat[ ,"net.comid"]) == as.character(erom_dat[ ,"group.comid"]),
                   c("net.id", "group.comid", "M", "month", "Q0001E", "QINCR0001E")]
#scaled by Mvalue
cumtot$M_scale0001E <- (cumtot[,"Q0001E"] - cumtot[,"QINCR0001E"]) + (cumtot[,"QINCR0001E"] * cumtot[,"M"])

MA <- cumtot[cumtot[,"month"]=="MA",c("net.id","M","M_scale0001E")]
names(MA) <- c("net.id", "M", "MAQ0001E")
minMM <- aggregate(cumtot[cumtot[,"month"] != "MA",("M_scale0001E")],
                by = list(cumtot[cumtot[,"month"] != "MA","net.id"]), min)
names(minMM) <- c("net.id","minMMQ0001E")
maxMM <- aggregate(cumtot[cumtot[,"month"] != "MA",("M_scale0001E")],
                   by = list(cumtot[cumtot[,"month"] != "MA","net.id"]), max)
names(maxMM) <- c("net.id", "maxMMQ0001E")
covMM <- aggregate(cumtot[cumtot[,"month"] != "MA",("M_scale0001E")],
                   by = list(cumtot[cumtot[,"month"] != "MA","net.id"]),
                   function (x) mean(x)/sd(x))
names(covMM) <- c("net.id", "covMMQ0001E")

data.out <- Reduce(function(x, y)
  merge(x, y, by = "net.id", all.x = T),
  list(data.out, MA, minMM, maxMM, covMM))

#how is volicity accumilated
#from NHDPLUSV2 user manual... Johnson 1996 regression
#Dont feel comfortable changing for M so left at outlet
#names(erom_dat)
cumtotv <- erom_dat[as.character(erom_dat[ ,"net.comid"]) == as.character(erom_dat[ ,"group.comid"]),
                  c("net.id", "group.comid", "M", "month", "V0001E")]

MAV <- cumtotv[cumtotv[,"month"]=="MA",c("net.id","V0001E")]
names(MAV) <- c("net.id", "MAV0001E")
minVMM <- aggregate(cumtotv[cumtotv[,"month"] != "MA",("V0001E")],
                   by = list(cumtotv[cumtotv[,"month"] != "MA","net.id"]), min)
names(minVMM) <- c("net.id","minMMV0001E")
maxVMM <- aggregate(cumtotv[cumtotv[,"month"] != "MA",("V0001E")],
                   by = list(cumtotv[cumtotv[,"month"] != "MA","net.id"]), max)
names(maxVMM) <- c("net.id", "maxMMV0001E")
covVMM <- aggregate(cumtotv[cumtotv[,"month"] != "MA",("V0001E")],
                   by = list(cumtotv[cumtotv[,"month"] != "MA","net.id"]),
                   function (x) mean(x)/sd(x))
names(covVMM) <- c("net.id", "covMMV0001E")

data.out <- Reduce(function(x, y)
  merge(x, y, by = "net.id", all.x = T),
  list(data.out, MAV, minVMM, maxVMM, covVMM))

data.out <- data.out[complete.cases(data.out),]

if(any(data.out[,"MAV0001E"] < -99)){
  data.out[data.out[,"MAV0001E"]< -99, grep("V0001E",names(data.out))]<- -9999
}
if (any(data.out[,"MAQ0001E"]> -99)){
  data.out[data.out[,"MAQ0001E"]< -99, grep("Q0001E",names(data.out))]<- -9999
}
return(data.out)
}
