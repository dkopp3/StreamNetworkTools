#' Network Climate Summary
#'
#' network scale summeries of bioclim
#' (\url{http://www.worldclim.org/bioclim}) variables
#'
#' Requires "/VPUAttributeExtension" directory see(\code{\link{net_nhdplus}})
#'
#' @param netdelin output from \code{\link{net_delin}}
#' @param vpu the vector processing unit
#' @param nhdplus_path directory containing NHDPlus
#'   see(\code{\link{net_nhdplus}})
#'
#'@return \code{data.frame} containing: \code{COMID} root node of network;
#'  \code{vpu} vector processing unit; \code{MISSDATAA.Temp} area of missing
#'  temperature data; \code{TEMPVC} mean annual temperature (deg C); \code{seasonality_t}
#'  Coefficient of variation of mean monthly temperatures; \code{warm_mo} warmest
#'  month; \code{warm_mo_t} mean temperature of warmest month; \code{cold_mo}
#'  coldest month; \code{cold_mo_t} mean temperature of coldest month;
#'  \code{diff_t} temperature difference between warm and cold monthly
#'  temperatures; \code{warm_q_t} mean temperature of warmest quarter;
#'  \code{warm_q}  warmest quarter; \code{cold_q_t} mean temperature of coldest
#'  quarter; \code{cold_q} coldest quarter; \code{MISSDATAA.Ppt} area of missing
#'  precipitation data; \code{PRECIPVC} cumulative mean annual precipiration (mm);
#'  \code{wet_mo} wettest month; \code{wet_mo_p} cumulative mean precipitation
#'  of wettest month; \code{dry_mo} driest month; \code{dry_mo_p} cumulative
#'  mean precipitation of driest month; \code{seasonality_p} coefficient of
#'  vatiation of mean monthly precipitation; \code{wet_q_p} cumulaltive mean
#'  precipitation of wettest quarter; \code{wet_q} wettest quarter;
#'  \code{dry_q_p} cumulaltive mean precipitation of driest quarter; \code{dry_q}
#'  driest quarter; \code{dry_q_t} mean temperature of driest quarter;
#'  \code{wet_q_t} mean temperature of wettest quarter; \code{warm_q_p}
#'  cumulaltive mean precipitation of warmest quarter; \code{cold_q_p}
#'  cumulaltive mean precipitation of coldest quarter; \code{RUNOFFVC}
#'  cumulative mean annual runff (mm); \code{MAFLOWV} Vogal Estimation of mean
#'  annual discharge; \code{MAVELV} Vogal estimation of mean annual velocity at
#'  network outlet; \code{Q0001E} EROM estimation of mean mean annual
#'  discharge; \code{V0001E} EROM estimation of mean annual velocity
#'
#' @examples
#' f <- net_clim(netdelin = c,vpu = "01", nhdplus_path = getwd())

net_clim<-function(nhdplus_path, vpu, netdelin){
  full.net <- netdelin$Network
  clim.dir <- grep(paste(vpu, "/VPUAttributeExtension", sep = ""),
                   list.dirs(nhdplus_path, full.names = T),
                   value = T)
  mappt.files <- grep("CumTotPrecip",
                      list.files(clim.dir[1], full.names=T),
                      value = T)
  matemp.files <- grep("CumTotTemp",
                       list.files(clim.dir[1], full.names = T),
                       value = T)
  ROMA.files <- grep("CumTotROMA",
                     list.files(clim.dir[1], full.names = T),
                     value = T)
  ROMA <- read.table(ROMA.files, header = T, sep = ",")
  names(ROMA) <- toupper(names(ROMA))
  EROM.dir<-grep(paste(vpu, "/EROMExtension", sep = ""),
                 list.dirs(nhdplus_path, full.names = T),
                 value = T)
  vogel.dir <- grep(paste(vpu, "/VogelExtension", sep = ""),
                    list.dirs(nhdplus_path, full.names = T),
                    value = T)
  #EROM flow and velocity estimates at outlet
  #look at other EROM files
  erom<-grep("EROM_MA0001.DBF",
             list.files(EROM.dir[1],full.names=T),
             value = T)
  vogel <- list.files(vogel.dir[1], full.names = T)
  erom.flow <- foreign::read.dbf(erom)
  vogel.flow <- foreign::read.dbf(vogel)
  names(erom.flow) <- toupper(names(erom.flow))
  names(vogel.flow) <- toupper(names(vogel.flow))
  #read in first file, then loop over all other months
  map <- read.table(mappt.files[1], header = T, sep = ",")
  colnames(map) <- toupper(colnames(map))
  #set month of dataframe
  month <- unlist(lapply(strsplit(mappt.files[1], "/"),
                         "[[", lengths(strsplit(mappt.files[1], "/"))))
  #cbind month to ppt data
  map<-cbind(map, month)
  #merge with fill.net to maintain group comid.
  #Unique values are now set as net.comid.group.comid & month
  map <- merge(full.net, map,
               by.y = "COMID",
               by.x = "net.comid")
  #loop over remainder files
  for (j in 2:length(mappt.files)) {
    month <- unlist(lapply(strsplit(mappt.files[j],"/"),
                           "[[",lengths(strsplit(mappt.files[j],"/"))))
    temp <- read.table(mappt.files[j], header = T, sep = ",")
    colnames(temp) <- toupper(colnames(temp))
    temp <- cbind(temp, month)
    temp <- merge (full.net,temp,
                   by.y="COMID",
                   by.x="net.comid")
    map <- rbind(map,temp)
  }
  #temperatrue
  matemp <- read.table(matemp.files[1], header = T, sep = ",")
  colnames(matemp) <- toupper(colnames(matemp))
  month <- unlist(lapply(strsplit(matemp.files[1],"/"),
                         "[[",lengths(strsplit(matemp.files[1],"/"))))
  matemp <- cbind(matemp, month)
  matemp <- merge(full.net, matemp,
                  by.y="COMID",
                  by.x="net.comid")
  #loop over remainder files
  for (j in 2:length(matemp.files)){
    month<-unlist(lapply(strsplit(matemp.files[j], "/"),
                         "[[", lengths(strsplit(matemp.files[j], "/"))))
    temp <- read.table(matemp.files[j], header = T, sep = ",")
    colnames(temp) <- toupper(colnames(temp))
    temp <- cbind(temp,month)
    temp <- merge (full.net, temp,
                   by.y="COMID",
                   by.x="net.comid")
    matemp <- rbind(matemp,temp)
  }
  #temp in NHD given centegrate * 100
  matemp[,"TEMPVC"] <- matemp[,"TEMPVC"] / 100
  data.out <- as.data.frame(unique(full.net[,c( "group.comid", "vpu")]))
  names(data.out)[1] <- "COMID"
  #####mean annual temperature
  #CumTotTemp is cumulative down the network
  #consequently, the value at the COMID is the network total
  #cumtemp removes values for each reach in the network (tot already incorporates this)
  cumtemp.data <- matemp[as.character(matemp[,"net.comid"]) ==
                           as.character(matemp[,"group.comid"]), ]
  # bio1 = annual mean temperature
  MAT <- cumtemp.data[cumtemp.data[,"month"] == "CumTotTempMA.txt",
                      c("group.comid", "MISSDATAA", "TEMPVC")]
  #assigns field names
  #merges mean annual temp to output file
  data.out <- merge(data.out, MAT,
                    by.x = "COMID",
                    by.y = "group.comid",
                    all.x = T)
  #bio 4 - seasonality (do not have max or min temps so skipped bio3 and bio4)
  #aggregate all months other than mean annual temp calculate
  CUMTEMPMM <- cumtemp.data[cumtemp.data[ ,"month"] != "CumTotTempMA.txt", ]
  month <- substr(CUMTEMPMM[ ,"month"],
                  nchar(as.character(CUMTEMPMM[ ,"month"]))-5,
                  nchar(as.character(CUMTEMPMM[ ,"month"]))-4)
  CUMTEMPMM[,"month"]<-month
  seasonality_t <- aggregate(CUMTEMPMM[ ,"TEMPVC"],
                       by = list(group.comid = CUMTEMPMM[ ,"group.comid"]),
                       sd)
  names(seasonality_t) <- c("COMID", "seasonality_t")
  data.out<-merge(data.out,seasonality_t,
                  by.x = "COMID",
                  by.y = "COMID",
                  all.x = T)
  # Bio 5 temp of warmest month (NOT BIOCLIM VARIABLE dont have minT)
  warm_mo_t <- aggregate(CUMTEMPMM[ ,"TEMPVC"],
                         by = list(CUMTEMPMM[,"group.comid"]),
                         max)
  names(warm_mo_t) <- c("COMID", "warm_mo_t")
  warm_mo <- CUMTEMPMM[CUMTEMPMM[,"group.comid"] %in% warm_mo_t[,"COMID"] &
                         CUMTEMPMM[,"TEMPVC"] %in% warm_mo_t[,"warm_mo_t"],
                           c("group.comid", "month")]
  names(warm_mo) <- c("COMID", "warm_mo")
  warm_mo <- merge(warm_mo, warm_mo_t,
                   by = "COMID",
                   all.x = T)
  data.out <- merge(data.out, warm_mo,
                    by = "COMID",
                    all.x = T)
  #Bio 6 temp of coldest month (NOT BIOCLIM VARIABLE dont have minT)
  cold_mo_t <- aggregate(CUMTEMPMM[ ,"TEMPVC"],
                         by=list(CUMTEMPMM[ ,"group.comid"]),
                         min)
  names(cold_mo_t)<-c("COMID", "cold_mo_t")
  cold_mo <- CUMTEMPMM[CUMTEMPMM[,"group.comid"] %in% cold_mo_t[,"COMID"] &
                            CUMTEMPMM[,"TEMPVC"] %in% cold_mo_t[,"cold_mo_t"],
                           c("group.comid", "month")]
  names(cold_mo) <- c("COMID", "cold_mo")
  cold_mo <- merge(cold_mo, cold_mo_t,
                   by = "COMID",
                   all.x = T)
  data.out <- merge(data.out, cold_mo,
                    by = "COMID",
                    all.x = T)
  #bioclim 7 - modified temp of warmest mo minus tem of coldest month
  diff_t <- data.out[,"warm_mo_t"] - data.out[,"cold_mo_t"]
  data.out <- cbind(data.out, diff_t)
  # identify three consecutive months which highest and lowest precip values
  comid.str <- unique(CUMTEMPMM[,"group.comid"])
  hot.cold.q<-data.frame(COMID=character(),
                         warm_q_t = character(),
                         warm_q = character(),
                         cold_q_t = character(),
                         cold_q = character())
  for (i in 1:length(comid.str)) {
    # select from CUMTEMPMM the focal comid
    f <- CUMTEMPMM[CUMTEMPMM[,"group.comid"] == comid.str[i], ]
        quarter.mean <- data.frame(q.mean = character(),
                               month = character())
        for (j in 1:length(f[,1])){
          if (j <= 12){
            # if the sum is less than 12
            if (j + 2 <= 12){
              q.mean <- mean(f[c(j, j+1, j+2), "TEMPVC"])
              month <- paste(f[j, "month"], f[j+1, "month"], f[j+2, "month"], sep =",")
              quarter.mean <- rbind(quarter.mean, cbind(q.mean, month))
              } else if (j+2==13){
                q.mean <- mean(f[c(j,j+1,1), "TEMPVC"])
                month <- paste(f[j,"month"], f[j+1,"month"], f[1,"month"], sep =",")
                quarter.mean <- rbind(quarter.mean, cbind(q.mean, month))
                } else if (j+2==14){
                  q.mean <- mean(f[c(j, 1, 2), "TEMPVC"])
                  month <- paste(f[j,"month"],f[1,"month"],f[2,"month"], sep = ",")
                  quarter.mean <- rbind(quarter.mean, cbind(q.mean, month))
                  }
          }
        }
        warm_q_t <- quarter.mean[quarter.mean[,"q.mean"] ==
                                   max(as.numeric(
                                     as.character(
                                       quarter.mean[,"q.mean"]))),
                                 c("q.mean","month")]
         names(warm_q_t) <- c("warm_q_t", "warm_q")
         cold_q_t <- quarter.mean[quarter.mean[,"q.mean"] ==
                                     min(as.numeric(
                                       as.character(
                                         quarter.mean[,1]))),
                                  c("q.mean", "month")]
         names(cold_q_t) <- c("cold_q_t","cold_q")
         COMID <- as.character(comid.str[i])
         hot.cold.q <- rbind(hot.cold.q,
                             cbind(COMID, warm_q_t, cold_q_t))
    }
  data.out <- merge(data.out,hot.cold.q,
                    by = "COMID",
                    all.x = T)
  ######mean annaul ppt
  #CumTotPrecip is cumulative down the network
  #consequently, the value at the COMID is the network total
  map[,"PRECIPVC"] <- map[,"PRECIPVC"] / 100
  cumppt.data <- map[as.character(map[,"net.comid"]) ==
                     as.character(map[,"group.comid"]), ]
  #total mean monthly precipitation (sum of all monthly means) cf with sum.  BIO12
  tot_ppt <- cumppt.data[cumppt.data[,"month"] == "CumTotPrecipMA.txt",
                        c("group.comid", "MISSDATAA", "PRECIPVC")]
  data.out<-merge(data.out,tot_ppt,
                  by.x = "COMID",
                  by.y = "group.comid",
                  all.x = T)

  #select monthly ppt data
  CUMPPTMM <- cumppt.data[cumppt.data[ ,"month"] != "CumTotPrecipMA.txt", ]
  month <- substr(CUMPPTMM[ ,"month"],
                  nchar(as.character(CUMPPTMM[ ,"month"]))-5,
                  nchar(as.character(CUMPPTMM[ ,"month"]))-4)
  CUMPPTMM[,"month"] <- month

  #ppt of wettest month bio13
  wet_mo_p <- aggregate(CUMPPTMM[,"PRECIPVC"],
                   by = list(CUMPPTMM[,"group.comid"]),
                   max)
  names(wet_mo_p) <- c("COMID", "wet_mo_p")
  wet_mo <- CUMPPTMM[CUMPPTMM[,"group.comid"] %in% wet_mo_p[,"COMID"]&
                           CUMPPTMM[,"PRECIPVC"] %in% wet_mo_p[,"wet_mo_p"],
                            c("group.comid", "month")]
  names(wet_mo) <- c("COMID", "wet_mo")
  wet_mo_p <- merge(wet_mo, wet_mo_p,
                    by = "COMID",
                    all.x = T)
  data.out <- merge(data.out, wet_mo_p,
                    by ="COMID",
                    all.x = T)
  #ppt of driest month bio14
  dry_mo_p <- aggregate(CUMPPTMM[,"PRECIPVC"],
                   by = list(CUMPPTMM[,"group.comid"]),
                   min)
  names(dry_mo_p) <- c("COMID", "dry_mo_p")
  dry_mo <- CUMPPTMM[CUMPPTMM[,"group.comid"] %in% dry_mo_p[,"COMID"]&
                       CUMPPTMM[,"PRECIPVC"] %in% dry_mo_p[,"dry_mo_p"],
                          c("group.comid","month")]
  names(dry_mo) <- c("COMID", "dry_mo")
  dry_mo_p <- merge(dry_mo, dry_mo_p,
                  by = "COMID",
                  all.x = T)
  data.out <- merge(data.out, dry_mo_p,
                    by = "COMID",
                    all.x = T)

  #Precipitaton seasonality - Coeffieient of variation (BIO15)
  seasonality_p <- aggregate(CUMPPTMM[,"PRECIPVC"],
                               by = list(COMID = CUMPPTMM[,"group.comid"]),
                           function (x) c(stdev = sd(x), mean = mean(x)))
  seasonality_p <- data.frame(COMID = as.character(seasonality_p[,"COMID"]),
                              seasonality_p = as.numeric(seasonality_p[ ,"x"][,"stdev"] /
                                                         seasonality_p[ ,"x"][ ,"mean"]))
  data.out <- merge(data.out, seasonality_p,
                    by = "COMID",
                    all.x = T)
  #Identify dry and wet quarters
  comid.str <- unique(CUMPPTMM[,"group.comid"])
  dry.wet.q <- data.frame(COMID = character(),
                          wet_q_p = character(),
                          wet_q = character(),
                          dry_q_p = character(),
                          dry_q = character())
    for(i in 1:length(comid.str)){
      f <- CUMPPTMM[CUMPPTMM[,"group.comid"] == comid.str[i], ]
      quarter.sum<-data.frame(q.sum = numeric(), month = character())
    #loop over each row in focal comid "f"
    for (j in 1:length(f[,1])){
      if (j <= 12){
        if ( j+2 <= 12){
          q.sum <- sum(f[c(j, j+1, j+2), "PRECIPVC"])
          month <- paste(f[j, "month"], f[j+1, "month"], f[j+2, "month"],
                         sep = ",")
          quarter.sum<-rbind(quarter.sum,cbind(q.sum,month))
          } else if (j+2 == 13){
            q.sum <- sum(f[c(j, j+1, 1), "PRECIPVC"])
            month <- paste(f[j, "month"], f[j+1, "month"], f[1, "month"],
                           sep = ",")
            quarter.sum <- rbind(quarter.sum, cbind(q.sum, month))
            } else if (j+2 == 14){
              q.sum <- sum(f[c(j, 1, 2), "PRECIPVC"])
              month<-paste(f[j, "month"], f[1, "month"], f[2, "month"],
                           sep = ",")
              quarter.sum <- rbind(quarter.sum, cbind(q.sum, month))
        }
      }
    }
      wet_q_p <- quarter.sum[quarter.sum[,1] ==
                             max(as.numeric(
                               as.character(quarter.sum[,1]))),1]
      wet_q <- quarter.sum[quarter.sum[,1] ==
                           max(as.numeric(
                             as.character(quarter.sum[,1]))),2]
      dry_q_p <- quarter.sum[quarter.sum[,1] ==
                               min(as.numeric(
                                 as.character(quarter.sum[,1]))),1]
      dry_q <- quarter.sum[quarter.sum[,1] ==
                             min(as.numeric(
                               as.character(quarter.sum[,1]))),2]
      COMID<-as.character(comid.str[i])
      dry.wet.q <- rbind(dry.wet.q,
                         data.frame(COMID, wet_q_p, wet_q, dry_q_p, dry_q))
  }
  data.out <- merge(data.out, dry.wet.q,
                  by = "COMID",
                  all.x = T)
  # quarter sum funtion:
  # mean temp of dry/wet quarter or mean ppt of warm/cold q
  quart_sum <- function(data, quarter, data.out) {
    if (any(names(data) == "TEMPVC")){
      s <- "TEMPVC"
      } else if (any(names(data) == "PRECIPVC")){
        s <- "PRECIPVC"
        }
    q <- strsplit(as.character(data.out[ ,quarter]), ",")
    names(q) <- data.out[,"COMID"]
    pt <- data.frame(COMID = character(), x = numeric())

    for (i in names(q)) {
      p <- data[data[,"group.comid"] == i,
                c(s, "month")]
      pt <- rbind(pt, data.frame(COMID = i,
                                 x = mean(p[p[,"month"] %in% q[i][[1]], s])))
    }
    return(pt)
  }
  # mean temp of dry/wet quarter
  dry_q_t <- quart_sum(CUMTEMPMM, "dry_q", data.out)
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
  data.out <- Reduce(function(x, y) merge(x, y,
                                by = "COMID",
                                all.x = T),
                     list(data.out, dry_q_t, wet_q_t, warm_q_p, cold_q_p))

  #read in precipitation files (CumTotPrecipMA, CumTotPrecipMM)
  ROMA <- merge (full.net,
                 ROMA,
                 by.y = "COMID",
                 by.x = "net.comid")
  cumtotROMA <- ROMA[as.character(ROMA[,"net.comid"]) ==
                       as.character(ROMA[,"group.comid"]),
                     c("group.comid","RUNOFFVC")]
  data.out <- merge(data.out, cumtotROMA,
                    by.x = "COMID",
                    by.y = "group.comid",
                    all.x = T)
  erom.flow <- erom.flow[erom.flow[, "COMID"] %in% levels(full.net[, "group.comid"]),
                 c("COMID","Q0001E", "V0001E")]
  vogel.flow <- vogel.flow[vogel.flow[, "COMID"] %in% levels(full.net[, "group.comid"]),
                 c("COMID","MAFLOWV","MAVELV")]
  data.out <- merge(data.out, vogel.flow,
                    by = "COMID", all.x = T)
  data.out <- merge(data.out, erom.flow,
                    by = "COMID", all.x = T)
  names(data.out)[c(3,15)]<-c("MISSDATAA.Temp","MISSDATAA.Ppt")
  return(data.out)
}
