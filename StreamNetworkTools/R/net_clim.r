#' Network Climate Metrics
#'
#' Climate metrics derived from PRISM 1971-2001 mean monthly and mean annual
#' temperature and precipitation normals in NHDPlusV2 value added attribure
#' extension tables
#'
#' See (\url{http://www.worldclim.org/bioclim}) for more informaton on climate
#' metrics
#'
#' Requires "/VPUAttributeExtension" directory see(\code{\link{net_nhdplus}})
#'
#' @param netdelin output from \code{\link{net_delin}}
#' @param vpu NHDPlusV2 Vector Processing Unit
#' @param nhdplus_path Directory for NHDPlusV2 files (\code{\link{net_nhdplus}})
#'
#' @return \code{data.frame}: \code{$net.id} Unique identifier of network;
#'   \code{$group.comid} Root COMID of network; \code{$vpu} NHDPlusV2 Vector
#'   Processing Unit; \code{$MISSDATAA.x} area of missing temperature data;
#'   \code{$TEMPVC} mean annual temperature (deg C); \code{$seasonality_t}
#'   Coefficient of variation of mean monthly temperatures; \code{$warm_mo}
#'   2-digit warmest month; \code{$warm_mo_t} mean temperature of warmest month;
#'   \code{$cold_mo} 2-digit coldest month; \code{$cold_mo_t} mean temperature
#'   of coldest month; \code{$diff_t} difference between warm and cold monthly
#'   temperatures; \code{$warm_q_t} mean temperature of warmest quarter;
#'   \code{$warm_q} 2-digit warmest quarter; \code{$cold_q_t} mean temperature
#'   of coldest quarter; \code{$cold_q} 2-digit coldest quarter;
#'   \code{MISSDATAA.y} area of missing precipitation data; \code{$PRECIPVC}
#'   cumulative mean annual precipiration (mm); \code{$wet_mo} 2-digit wettest
#'   month; \code{$wet_mo_p} cumulative mean precipitation of wettest month;
#'   \code{$dry_mo} 2-digit driest month; \code{$dry_mo_p} cumulative mean
#'   precipitation of driest month; \code{$seasonality_p} coefficient of
#'   vatiation of mean monthly precipitation; \code{$wet_q_p} cumulaltive mean
#'   precipitation of wettest quarter; \code{$wet_q} 2-digit wettest quarter;
#'   \code{$dry_q_p} cumulaltive mean precipitation of driest quarter;
#'   \code{dry_q} 2-digit driest quarter; \code{$dry_q_t} mean temperature of
#'   driest quarter; \code{$wet_q_t} mean temperature of wettest quarter;
#'   \code{$warm_q_p} cumulaltive mean precipitation of warmest quarter;
#'   \code{$cold_q_p} cumulaltive mean precipitation of coldest quarter
#'
#' @examples
#' # identify NHDPlusV2 COMID
#' a <- net_sample(nhdplus_path = getwd(), vpu = "01", ws_order = 6, n = 5)
#' # delineate stream network
#' b <- net_delin(group_comid = as.character(a[,"COMID"]), nhdplus_path = getwd(), vpu = "01")
#' # derive climate summary
#' c <- net_clim(netdelin = b,vpu = "01", nhdplus_path = getwd())
#'
#' @export

net_clim<-function(nhdplus_path, vpu, netdelin){
  directory <- grep(paste(vpu, "/NHDPlusAttributes", sep = ""),
                    list.dirs(nhdplus_path, full.names = T),
                    value = T)
  vaa <- grep("PlusFlowlineVAA.dbf",
              list.files(directory[1], full.names = T),
              value = T)
  vaa <- foreign::read.dbf(vaa)
  names(vaa) <- toupper(names(vaa))

  full.net <- netdelin$Network
  full.net <- merge(full.net, vaa[,c("COMID","AREASQKM")],
                    by.x = "net.comid", by.y = "COMID")

  clim.dir <- grep(paste(vpu, "/VPUAttributeExtension", sep = ""),
                   list.dirs(nhdplus_path, full.names = T),
                   value = T)
  mappt.files <- grep("CumTotPrecip",
                      list.files(clim.dir[1], full.names = T),
                      value = T)
  mappt.incr.files <- grep("IncrPrecip",
                           list.files(clim.dir[1], full.names = T),
                           value = T)
  matemp.files <- grep("CumTotTemp",
                       list.files(clim.dir[1], full.names = T),
                       value = T)
  matemp.incr.files <- grep("IncrTempM",
                       list.files(clim.dir[1], full.names = T),
                       value = T)

  #read files function
  read_files <- function (file_type){
    map <- read.table(file_type[1], header = T, sep = ",")
    colnames(map) <- toupper(colnames(map))
    colnames(map)[1] <- "COMID"
    #set month of dataframe, pulls off last name in filepath
    month <- unlist(lapply(strsplit(file_type[1], "/"), "[[", lengths(strsplit(file_type[1], "/"))))
    month <- unlist(lapply(strsplit(month,"\\."),"[[", 1))
    month <- substr(month,nchar(month)-1, nchar(month))

    #cbind month to ppt data
    map <- data.frame(map, month = month)
    #merge with fill.net to maintain group comid.
    #Unique values are now set as net.comid.group.comid & month
    map <- merge(full.net, map, by.y = "COMID", by.x = "net.comid")
    #loop over remainder files
    for (j in 2:length(file_type)) {
      month <- unlist(lapply(strsplit(file_type[j],"/"), "[[",lengths(strsplit(file_type[j], "/"))))
      month <- unlist(lapply(strsplit(month,"\\."),"[[", 1))
      month <- substr(month,nchar(month)-1, nchar(month))

      temp <- read.table(file_type[j], header = T, sep = ",")
      colnames(temp) <- toupper(colnames(temp))
      colnames(temp)[1] <- "COMID"
      temp <- data.frame(temp, month = month)
      temp <- merge (full.net,temp, by.y = "COMID", by.x = "net.comid")
      map <- rbind(map, temp)
    }
    return(map)
  }

  map <- read_files(mappt.files)
  matemp <- read_files(matemp.files)
  mapincr <- read_files(mappt.incr.files)
  matempincr <- read_files(matemp.incr.files)

  # temp in NHD given centegrate * 100
  # scale temp data to deg C
  matemp[, "TEMPVC"] <- matemp[, "TEMPVC"] / 100
  matempincr[, "TEMPV"] <- matempincr[, "TEMPV"] / 100
  data.out <- as.data.frame(unique(full.net[,c("net.id", "group.comid", "vpu")]))
  #names(data.out)[1] <- "COMID"

  ##### mean annual temperature
  #CumTotTemp is cumulative down the network
  #consequently, the value at the COMID is the network total
  #cumtemp removes values for each reach in the network (tot already incorporates this)
  #cumtottemp is area weighted average of sub-basins

  #area weighted average is modified my M value
  #if m value = 1 for all comid them result is cumtot value below
  cumtemp.data <- matemp[as.character(matemp[, "net.comid"]) == as.character(matemp[, "group.comid"]), ]
  matempincr$wt <- matempincr$AREASQKM*matempincr$M

  incrtemp.data <- by(matempincr, list(matempincr$net.id, matempincr$month),
          function(x) weighted.mean(x$TEMPV, x$wt))
  #changes "by" class into data.frame
  incrtemp.data <- do.call(rbind, list(incrtemp.data))
  incrtemp.data <- reshape::melt(incrtemp.data)
  names(incrtemp.data) <- c("net.id", "month", "TEMPV")

  cumtemp.data <- merge(incrtemp.data, cumtemp.data, by = c("net.id", "month"))

  # bio1 = annual mean temperature
  MAT <- cumtemp.data[cumtemp.data[,"month"] == "MA", c("net.id", "MISSDATAA", "TEMPV")]
  #assigns field names
  #merges mean annual temp to output file
  data.out <- merge(data.out, MAT, by= "net.id",#by.x = "COMID", by.y = "group.comid",
                    all.x = T)

  #bio 4 - seasonality (do not have max or min temps so skipped bio3 and bio4)
  #aggregate all months other than mean annual temp calculate
  CUMTEMPMM <- cumtemp.data[cumtemp.data[ ,"month"] != "MA", ]

  seasonality_t <- aggregate(CUMTEMPMM[ ,"TEMPV"],
                       by = list(CUMTEMPMM[ ,"net.id"]),
                       sd)
  names(seasonality_t) <- c("net.id", "seasonality_t")
  data.out <- merge(data.out,seasonality_t, by = "net.id", all.x = T)

  # Bio 5 temp of warmest month (NOT BIOCLIM VARIABLE dont have minT)
  warm_mo_t <- aggregate(CUMTEMPMM[ ,"TEMPV"], by = list(CUMTEMPMM[,"net.id"]), max)
  names(warm_mo_t) <- c("net.id", "warm_mo_t")
  #left here undat V and net.id
  warm_mo <- CUMTEMPMM[CUMTEMPMM[,"net.id"] %in% warm_mo_t[,"net.id"] &
                         CUMTEMPMM[,"TEMPV"] %in% warm_mo_t[,"warm_mo_t"],
                           c("net.id", "month")]
  names(warm_mo) <- c("net.id", "warm_mo")
  warm_mo <- merge(warm_mo, warm_mo_t, by = "net.id", all.x = T)
  data.out <- merge(data.out, warm_mo,  by = "net.id", all.x = T)

  #Bio 6 temp of coldest month (NOT BIOCLIM VARIABLE dont have minT)
  cold_mo_t <- aggregate(CUMTEMPMM[ ,"TEMPV"],
                         by=list(CUMTEMPMM[ ,"net.id"]),
                         min)
  names(cold_mo_t)<-c("net.id", "cold_mo_t")
  cold_mo <- CUMTEMPMM[CUMTEMPMM[,"net.id"] %in% cold_mo_t[,"net.id"] &
                            CUMTEMPMM[,"TEMPV"] %in% cold_mo_t[,"cold_mo_t"],
                           c("net.id", "month")]
  names(cold_mo) <- c("net.id", "cold_mo")
  cold_mo <- merge(cold_mo, cold_mo_t, by = "net.id", all.x = T)
  data.out <- merge(data.out, cold_mo, by = "net.id", all.x = T)

  #bioclim 7 - modified temp of warmest mo minus tem of coldest month
  diff_t <- data.out[,"warm_mo_t"] - data.out[,"cold_mo_t"]
  data.out <- data.frame(data.out, diff_t)
  # identify three consecutive months which highest and lowest precip values
  comid.str <- unique(CUMTEMPMM[,"net.id"])
  hot.cold.q<-data.frame(net.id= character(), COMID=character(),
                         warm_q_t = character(),
                         warm_q = character(), cold_q_t = character(),
                         cold_q = character())
  for (i in 1:length(comid.str)) {
    # select from CUMTEMPMM the focal comid
    f <- CUMTEMPMM[CUMTEMPMM[,"net.id"] == comid.str[i], ]
    quarter.mean <- data.frame(q.mean = character(),
                               month = character())
    for (j in 1:length(f[,1])){
      if (j <= 12){
            # if the sum is less than 12
        if (j + 2 <= 12){
              q.mean <- mean(f[c(j, j+1, j+2), "TEMPV"])
              month <- paste(f[j, "month"], f[j+1, "month"], f[j+2, "month"], sep =",")
              quarter.mean <- rbind(quarter.mean, cbind(q.mean, month))
              } else if (j + 2 == 13){
                q.mean <- mean(f[c(j, j + 1, 1), "TEMPV"])
                month <- paste(f[j, "month"], f[j + 1, "month"], f[1, "month"], sep =",")
                quarter.mean <- rbind(quarter.mean, cbind(q.mean, month))
                } else if (j + 2 == 14){
                  q.mean <- mean(f[c(j, 1, 2), "TEMPV"])
                  month <- paste(f[j,"month"],f[1,"month"],f[2,"month"], sep = ",")
                  quarter.mean <- rbind(quarter.mean, cbind(q.mean, month))
                  }
          }
    }

        warm_q_t <- quarter.mean[
          quarter.mean[,"q.mean"] ==
            max(as.numeric(as.character(quarter.mean[,"q.mean"]))),
          c("q.mean","month")]
         names(warm_q_t) <- c("warm_q_t", "warm_q")
         cold_q_t <- quarter.mean[
           quarter.mean[,"q.mean"] ==
             min(as.numeric(as.character(quarter.mean[,1]))),
           c("q.mean", "month")]

         names(cold_q_t) <- c("cold_q_t","cold_q")
         net.id <- as.character(comid.str[i])
         hot.cold.q <- rbind(hot.cold.q,
                             data.frame(net.id, warm_q_t, cold_q_t))
  }

  data.out <- merge(data.out,hot.cold.q, by = "net.id", all.x = T)

  ######mean annaul ppt
  #CumTotPrecip is cumulative down the network
  #consequently, the value at the COMID is the network total
  map[,"PRECIPVC"] <- map[,"PRECIPVC"] / 100
  mapincr[,"PRECIPV"] <- mapincr[,"PRECIPV"] / 100

  cumppt.data <- map[as.character(map[, "net.comid"]) == as.character(map[, "group.comid"]), ]

  incrmap.data <- by(mapincr, list(mapincr$net.id, mapincr$month),
                      function(x) weighted.mean(x$PRECIPV, x$AREASQKM*x$M))

  #changes "by" class into data.frame
  incrmap.data <- do.call(rbind, list(incrmap.data))

  incrmap.data <- reshape::melt(incrmap.data)
  names(incrmap.data) <- c("net.id", "month", "PRECIPV")

  cumppt.data <- merge(incrmap.data, cumppt.data, by = c("net.id", "month"))

  #total mean monthly precipitation (sum of all monthly means) cf with sum.  BIO12
  tot_ppt <- cumppt.data[cumppt.data[,"month"]=="MA", c("net.id" , "MISSDATAA" , "PRECIPV")]

  data.out <- merge(data.out,tot_ppt, by = "net.id", all.x = T)

  #select monthly ppt data
  CUMPPTMM <- cumppt.data[cumppt.data[ ,"month"] != "MA", ]
  #ppt of wettest month bio13
  wet_mo_p <- aggregate(CUMPPTMM[,"PRECIPV"],
                   by = list(CUMPPTMM[,"net.id"]),
                   max)
  names(wet_mo_p) <- c("net.id", "wet_mo_p")
  wet_mo <- CUMPPTMM[CUMPPTMM[,"net.id"] %in% wet_mo_p[,"net.id"]&
                           CUMPPTMM[,"PRECIPV"] %in% wet_mo_p[,"wet_mo_p"],
                            c("net.id", "month")]
  names(wet_mo) <- c("net.id", "wet_mo")
  wet_mo_p <- merge(wet_mo, wet_mo_p, by = "net.id",
                    all.x = T)
  data.out <- merge(data.out, wet_mo_p, by ="net.id", all.x = T)

  #ppt of driest month bio14
  dry_mo_p <- aggregate(CUMPPTMM[,"PRECIPV"],
                   by = list(CUMPPTMM[,"net.id"]),
                   min)
  names(dry_mo_p) <- c("net.id", "dry_mo_p")
  dry_mo <- CUMPPTMM[CUMPPTMM[,"net.id"] %in% dry_mo_p[,"net.id"]&
                       CUMPPTMM[,"PRECIPV"] %in% dry_mo_p[,"dry_mo_p"],
                          c("net.id","month")]
  names(dry_mo) <- c("net.id", "dry_mo")
  dry_mo_p <- merge(dry_mo, dry_mo_p, by = "net.id", all.x = T)
  data.out <- merge(data.out, dry_mo_p, by = "net.id", all.x = T)

  #Precipitaton seasonality - Coeffieient of variation (BIO15)
  seasonality_p <- aggregate(CUMPPTMM[,"PRECIPV"],
                               by = list(net.id = CUMPPTMM[,"net.id"]),
                           function (x) c(stdev = sd(x), mean = mean(x)))
  seasonality_p <- data.frame(net.id = as.character(seasonality_p[,"net.id"]),
                              seasonality_p = as.numeric(seasonality_p[ ,"x"][,"stdev"] /
                                                         seasonality_p[ ,"x"][ ,"mean"]))
  data.out <- merge(data.out, seasonality_p, by = "net.id", all.x = T)

  #Identify dry and wet quarters
  comid.str <- unique(CUMPPTMM[,"net.id"])
  dry.wet.q <- data.frame(net.id = character(), COMID = character(),
                          wet_q_p = numeric(), wet_q = character(), dry_q_p = numeric(),
                          dry_q = character())
  for(i in 1:length(comid.str)){
      f <- CUMPPTMM[CUMPPTMM[,"net.id"] == comid.str[i], ]
      quarter.sum <- data.frame(q.sum = numeric(), month = character())
    #loop over each row in focal comid "f"
    for (j in 1:length(f[,1])){
      if (j <= 12){
        if ( j+2 <= 12){
          q.sum <- sum(f[c(j, j+1, j+2), "PRECIPV"])
          month <- paste(f[j, "month"], f[j+1, "month"], f[j+2, "month"],
                         sep = ",")
          quarter.sum<-rbind(quarter.sum,data.frame(q.sum,month))
          } else if (j+2 == 13){
            q.sum <- sum(f[c(j, j+1, 1), "PRECIPV"])
            month <- paste(f[j, "month"], f[j+1, "month"], f[1, "month"],
                           sep = ",")
            quarter.sum <- rbind(quarter.sum, data.frame(q.sum, month))
            } else if (j+2 == 14){
              q.sum <- sum(f[c(j, 1, 2), "PRECIPV"])
              month<-paste(f[j, "month"], f[1, "month"], f[2, "month"],
                           sep = ",")
              quarter.sum <- rbind(quarter.sum, data.frame(q.sum, month))
        }
      }
    }
      wet_q_p <- quarter.sum[quarter.sum[,1] == max(quarter.sum[,1]), 1]
      wet_q <- quarter.sum[quarter.sum[,1] == max(quarter.sum[,1]), 2]
      dry_q_p <- quarter.sum[quarter.sum[,1] == min(quarter.sum[,1]), 1]
      dry_q <- quarter.sum[quarter.sum[,1] == min(quarter.sum[,1]),2]

      net.id <- as.character(comid.str[i])
      dry.wet.q <- rbind(dry.wet.q, data.frame(net.id, wet_q_p, wet_q, dry_q_p, dry_q))
  }

  data.out <- merge(data.out, dry.wet.q, by = "net.id", all.x = T)

  # quarter sum funtion:
  # mean temp of dry/wet quarter or mean ppt of warm/cold q
  quart_sum <- function(data, quarter, data.out) {
    if (any(names(data) == "TEMPV")){
      s <- "TEMPV"
      } else if (any(names(data) == "PRECIPV")){
        s <- "PRECIPV"
        }
    q <- strsplit(as.character(data.out[ ,quarter]), ",")
    names(q) <- data.out[,"net.id"]
    pt <- data.frame(net.id = character(), x = numeric())

    for (i in names(q)) {
      p <- data[data[,"net.id"] == i,
                c(s, "month")]
      pt <- rbind(pt, data.frame(net.id = i, x = mean(p[p[,"month"] %in% q[i][[1]], s])))
    }
    return(pt)
  }
  # mean temp of dry/wet quarter
  dry_q_t <- quart_sum(CUMTEMPMM, quarter ="dry_q", data.out)
  names(dry_q_t)[2] <-"dry_q_t"
  # mean temp of wet quarter
  wet_q_t <- quart_sum(CUMTEMPMM, "wet_q", data.out)
  names(wet_q_t)[2] <- "wet_q_t"
  # mean ppt of warm q
  warm_q_p <- quart_sum(CUMPPTMM, "warm_q", data.out)
  names(warm_q_p)[2] <- "warm_q_p"
  # mean ppt of cold q
  cold_q_p <- quart_sum(CUMPPTMM, "cold_q", data.out)
  names(cold_q_p)[2] <- "cold_q_p"

  data.out <- Reduce(function(x, y)
    merge(x, y, by = "net.id", all.x = T),
    list(data.out, dry_q_t, wet_q_t, warm_q_p, cold_q_p))

  return(data.out)
str(data.out)
  }
