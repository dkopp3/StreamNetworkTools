#' Network Area, Length and Bifurcation Ratios
#'
#' Uses \code{lm(...)} to estimates number, length and area ratios for
#' a stream network (i.e. Horton Laws).
#'
#' requires /NHDPlusAttributes directory (see \code{net_nhdplus})
#'
#' \code{ohm} is the strahler order of the (network - 1) because sampling point
#' (or COMID) my not necessarily encorpporate the entire edge of the largest
#' order
#'
#' @param netdelin output from \code{net_delin}
#' @param vpu vector processing unit
#' @param nhdplus_path directory containing NHDPlusV2 \code{\link{net_nhdplus}}
#'
#' @return named \code{list}: \code{$topology} is \code{data.frame} with count
#'   of stream numbers (\code{str_num}), sum of stream lengths (\code{str_len})
#'   and sum of drainage areas (\code{str_area}); and \code{$Horton_Est} are
#'   horotn estimates \code{Rb,Rl,Ra} (bifurcation, lenght and area ratio,
#'   respectively). \code{Rb.rsqr, Rl.rsqr, Ra.rsqr} are R^2 values for the
#'   \code{lm(...)} model. \code{ohm} is the strahler order of the (network -
#'   1) (see \code{Details}.
#'
#' @examples
#' net_hort(netdelin = c, vpu = "01", nhdplus_path = getwd())
#' @export

net_hort<-function ( netdelin, vpu, nhdplus_path){

  directory<-grep(paste(vpu,"/NHDPlusAttributes",sep=""),
                  list.dirs(nhdplus_path,full.names=T),
                  value = T)
  vaa<-grep("PlusFlowlineVAA.dbf",
            list.files(directory[1], full.names = T),
            value = T)
  vaa<-foreign::read.dbf(vaa)
  names(vaa) <- toupper(names(vaa))
  full.net <- netdelin$Network
  vaa <- merge(full.net, vaa,
               by.x = "net.comid",
               by.y = "COMID")

  #remove diveregences
  vaa <- vaa[vaa[,"STREAMORDE"] == vaa[, "STREAMCALC"], ]
  N <- aggregate(vaa[, "group.comid"],
                 by = list(vaa[, "group.comid"],
                           vaa[, "STREAMORDE"],
                           vaa[, "LEVELPATHI"]),
                 length)
  N <- aggregate(N[,"Group.2"],
                 by = list(N[,"Group.1"],
                           N[,"Group.2"]),
                 length)
  names(N) <- c("group.comid", "str_ord", "str_num")
  L <- aggregate(as.numeric(vaa[, "LENGTHKM"]),
                 by = list(vaa[, "group.comid"],
                           vaa[, "STREAMORDE"],
                           vaa[, "LEVELPATHI"]),
                 sum)
  L <- aggregate(L[,"x"],
                 by = list(L[,"Group.1"],
                           L[,"Group.2"]),
                 mean)
  names(L) <- c("group.comid","str_ord","str_len")
  A <- aggregate(as.numeric(vaa[,"TOTDASQKM"]),
                 by=list(vaa[,"group.comid"],
                         vaa[,"STREAMORDE"],
                         vaa[,"LEVELPATHI"]),
                 max)
  A <- aggregate(A[,"x"],
                 by=list(A[,"Group.1"],
                         A[,"Group.2"]),
                 mean)
  names(A) <- c("group.comid", "str_ord", "str_area")
  data <- Reduce(function(x, y) merge(x, y,
                                by = c("group.comid","str_ord"),
                                all.x = T),
           list(N, L, A))
  hor.laws <- data.frame(group.comid = character(),
                         ohm = numeric(),
                         Rb = numeric(),
                         Rb.rsqr = numeric(),
                         Rl = numeric(),
                         Rl.rsqr = numeric(),
                         Ra = numeric(),
                         Ra.rsqr = numeric())

  for (i in unique(data[,"group.comid"])){
      horton <- data[data[,"group.comid"] == i, ]
      # drops the comid because it may be incomplete.
      # a 5th order comid could occur in middle of reach
      # cannot horton ratios from 1 data point...
      # probably should for 2 eiither, especially
      # since we dont know if the comid includes whole path

      if (length(horton[,"group.comid"]) > 2){
        # for instance: Nw=stream number of order w;
        # Rb^ohm-w is bifurcation ratio (i.e. fixed value)
        # raised to watershed order minus the order
        # thus Nw=Rb^ohm-w #here i estimate that fixed value (Rb)
        # take log to linear model
        # log(Nw)= ohm*logRb-w*logRb
        # log(Nw)=log(Rb)*(ohm-w)

        horton <- horton[horton[,"str_ord"] != max(horton[,"str_ord"]), ]
        ohm <- max(horton[, "str_ord"])
        x <- ohm - horton[, "str_ord"]
        # estimate log(Rb) as slope
        lmRb <- summary(lm(log(horton[,"str_num"]) ~ (x)))
        # log(Rb)=0.9367344; log() has default base of exp(1)
        # thus slove for Rb w/base e
        Rb <- exp(lmRb$coefficients["x", 1])
        lmRb.r.squared <- (lmRb$r.squared)
        #Lw=L1*RL^(w-1)
        x <- horton[, "str_ord"] - 1
        L1 <- rep(horton[horton[,"str_ord"] ==1, "str_len"], length(horton[, "str_len"]))
        lmRl <- summary(lm(log(horton[, "str_len"]) ~ log(L1) + (x)))
        Rl <- exp(lmRl$coefficients["x", 1])
        lmRl.r.squared <- (lmRl$r.squared)
        #Aw=A1*RA^(w-1)
        x <- horton[,"str_ord"] - 1
        A1 <- rep(horton[horton[,"str_ord"] == 1, "str_area"], length(horton[, "str_area"]))
        lmRa <- summary(lm(log(horton[, "str_area"]) ~ log(A1) + (x)))
        Ra <- exp(lmRa$coefficients["x", 1])
        lmRa.r.squared <- (lmRa$r.squared)
        group.comid <- i
        out <- data.frame(COMID = group.comid,
                          ohm = ohm,
                          Rb = Rb,
                          Rb.rsqr = lmRb.r.squared,
                          Rl = Rl,
                          Rl.rsqr = lmRl.r.squared,
                          Ra = Ra,
                          Ra.rsqr = lmRa.r.squared)
        hor.laws <- rbind(hor.laws, out)
        } else {
          ohm <- max(horton[,"str_ord"])-1
          out <- data.frame(COMID = i,
                            ohm = ohm,
                            Rb = NA,
                            Rb.rsqr = NA,
                            Rl = NA,
                            Rl.rsqr = NA,
                            Ra = NA,
                            Ra.rsqr = NA)
          hor.laws <- rbind(hor.laws, out)
          }
      }
  data.out<-list(topology = data,
                 Horton_est = hor.laws)

  return(data.out)
  }
