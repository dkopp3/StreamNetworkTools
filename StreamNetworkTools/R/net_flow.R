#'Network Discharge (Q0001E) Summary
#'
#'Requires "/VPUAttributeExtension"
#'directory see(\code{\link{net_nhdplus}}) will scale value by measure value
#'from \code\{link{net_delin}}
#'
#'@param netdelin output from \code{\link{net_delin}}
#'@param vpu the vector processing unit
#'@param nhdplus_path directory containing NHDPlus
#'  see(\code{\link{net_nhdplus}})
#'
#'@return \code{RUNOFFVC}cumulative mean annual runoff (mm) and EROM velociety
#'  (V0001E) at network outlet; Mean Annual discharge (\code{MAQ0001E}), min
#'  mean monthly dischagre (\code{minMMQ0001E}), maximum monthly discharge
#'  (\code{maxMMQ0001E}) and coeffficient of variation of mean monthly discharge
#'  (\code{covMMQ0001E}) were EROM estimations scaled by mvalue of sampling
#'  point of mean mean annual discharge;
#'
#' @examples
#' f <- net_flow(netdelin = c, vpu = "01", nhdplus_path = getwd())
#'@export

net_flow <- function (nhdplus_path, vpu, netdelin){
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

#discharge values aggregated as sum of incremental
cumtot <- erom_dat[as.character(erom_dat[ ,"net.comid"]) == as.character(erom_dat[ ,"group.comid"]),
                   c("net.id", "group.comid", "M", "month", "Q0001E", "QINCR0001E")]
#scaled by Mvalue
cumtot$M_scale0001E <- (cumtot[,"Q0001E"] - cumtot[,"QINCR0001E"]) + (cumtot[,"QINCR0001E"] * cumtot[,"M"])

MA <- cumtot[cumtot[,"month"]=="MA",c("net.id","M_scale0001E")]
names(MA) <- c("net.id", "MAQ0001E")
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
names(MA) <- c("net.id", "MAV0001E")
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
#dropped vogel method
#vogel.dir <- grep(paste(vpu, "/VogelExtension", sep = ""),
 #                 list.dirs(nhdplus_path, full.names = T),
  #                value = T)

#vogel <- list.files(vogel.dir[1], full.names = T)
#vogel.flow <- foreign::read.dbf(vogel)
#names(vogel.flow) <- toupper(names(vogel.flow))
#vogel.flow <- vogel.flow[vogel.flow[, "COMID"] %in% levels(full.net[, "group.comid"]),
 #                        c("COMID","MAFLOWV","MAVELV")]

}
