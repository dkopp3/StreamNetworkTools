#' @export

net_posit <- function (nhdplus_path, vpu, comids){

  #the TO and FROM are switiched ... need to create an error message
  #comids <- data.frame(path.id = c(1),
  #                     TOCOMID = c(4290117),
  #                     FROMCOMID = c(4289983))

  directory <- grep(paste(vpu, "/NHDPlusAttributes", sep = ""),
                  list.dirs(nhdplus_path, full.names = T), value = T)
  #to/from comids
  flow.files <- grep("PlusFlow.dbf", list.files(directory[1],
                                              full.names = T), value = T)
  flow <- foreign::read.dbf(flow.files)
  Vaa <- grep("PlusFlowlineVAA.dbf",
              list.files(directory[1], full.names=T),
              value = T)
  vaa <- foreign::read.dbf(Vaa)
  names(vaa) <- toupper(names(vaa))
  netpos <- data.frame(COMID = character(),
                       path.id = character(),
                       altpath = numeric(),
                       sequence=numeric(),
                       LENGTHKM = numeric(),
                       TOCOMID = character(),
                       FROMCOMID = character())

for (i in 1:length(comids[,"TOCOMID"])){
  i<-1
  strt <- comids[i,"TOCOMID"]
  end <- comids[i,"FROMCOMID"]
  fcomid <- flow[flow[, "TOCOMID"] %in% strt, c("TOCOMID","FROMCOMID")]
  net <- flow[flow[, "TOCOMID"] %in% strt, c("TOCOMID","FROMCOMID")]
  #may need to add zero here to refer to headwaters? i.e. end is not in path
  #delineate network
  while (length(flow[flow[, "TOCOMID"] %in% fcomid[,"FROMCOMID"], "FROMCOMID"]) >= 1 &
         all(net[,"FROMCOMID"] != end)) {
    fcomid <- flow[flow[, "TOCOMID"] %in% fcomid[,"FROMCOMID"], c("TOCOMID","FROMCOMID")]
    fcomid <- fcomid[fcomid[,"FROMCOMID"] != 0, ]
    net <- rbind(net,fcomid)
    }
  focus <- net[net[,"TOCOMID"] == strt, ]
  row.names(focus) <- NULL
  names(focus) <- c("X_1", "X_2")
  name.cnt <- c(1,2)

  while(any(focus[, dim(focus)[2]] %in% net[, "TOCOMID"])){
    name.cnt <- name.cnt + 1
    #Which net[TOCOMID] match
    x <- net[net[ ,"TOCOMID"] %in% focus[, dim(focus)[2]], ]
    names(x)<-paste("X",name.cnt,sep="_")
    focus<-merge(focus, x, by.x = names(x)[1], all.x = T)
  }
  q <- apply(focus,2,function(x) which(x==end))
  path <- unique(focus[unlist(q),])

  if(length(path[,"X_1"]) == 0){
    print("path end not upstream of start?")
    } else {
      altpath <- seq(1, length(path[,1]), by=1)
      path<-data.frame(path,altpath)
      path<-reshape::melt(path, id = "altpath")
      names(path)<-c("altpath", "sequence", "COMID")
      path[,"sequence"]<-unlist(lapply(strsplit(as.character(path[,"sequence"]),"_"),"[[",2))
      path<-path[order(path[,"sequence"]),]
    }
  path <- merge(path,vaa[,c("COMID","LENGTHKM","DIVERGENCE")],by="COMID")
  path <- data.frame(path, TOCOMID = strt, FROMCOMID = end, path.id = comids[i,"path.id"])
  path <- path[order(path[,"altpath"],
                     as.numeric(as.character(path[,"sequence"]))),]
  netpos <- rbind(netpos, path)
  }
  return(netpos)
}
