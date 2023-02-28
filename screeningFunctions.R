###############################################################################
# Author: Halley Brantley
# 05/09/2017
# brantley.halley@epa.gov
# 08/31/2017 JRC  adjusted offLow and offHigh to better represent new sensor boards

# MM: Only offscreenhigh used in SENTINEL (Feb 2023)
###############################################################################
# External packages needed
require(plyr)
require(zoo)

screenOffScale <- function(pid, time, timeBase="5 min"){
  ############################################################
  # Function to screen out off-scale SPod data
  # Args:
  # pid : numeric vector of values reported by pid sensor
  # time : POSIXct vector of times corresponding to pid values
  # timeBase: Character of time interval at which to aggregate
  # data before applying thresholds
  # Returns:
  # pid : numeric vector of pid values with off-scale values
  # set to missing
  ############################################################

  # Remove missing values using last observation carry forward
  pidComplete <- zoo::na.locf(pid, na.rm=FALSE)
  # Replace leading missing values with zeros
  pidComplete[which(is.na(pidComplete))] <- 0
  # Aggregate data to time base
  df <- data.frame(pid = pidComplete, time=time)
  timeBreaks <- seq(round(min(time), "hour"),
                    round(max(time), "hour"), timeBase)
  df$timeCut <- cut(df$time, timeBreaks)
  dfStats <- ddply(df, .(timeCut), summarize,
    mean = mean(pid),
    sd = sd(pid),
    COV = mean/sd
  )
  # Remove off-scale values
  offLow <- dfStats$timeCut[which(dfStats$mean < 300)]
  offHigh <- dfStats$timeCut[which(dfStats$mean > 80000)]
  df$pid[which(df$timeCut %in% offLow)] <- NA
  df$pid[which(df$timeCut %in% offHigh)] <- NA
  # Exclude values that were already missing
  df$pid[which(is.na(pid))] <- NA
  return(df$pid)
}

getSlope <- function(y){
  ###########################################################
  # Function to find slope of y when plotted against an index
  # Args:
  # y: a numeric vector
  # Returns
  # slope: the slope of y versus an indexing variable
  ###########################################################
  if (length(which(!is.na(y))) == 0) {return (0)}
  x <- seq(1,length(y), 1)
  fit <- lm(y~x)
  return(fit$coefficients[2])
}


screenRH <- function(pid, time, rh){
  ###########################################################
  # Function to screen SPod data during rapid relative
  # humidity swings
  # Args:
  # pid : numeric vector of values reported by pid sensor
  # time : POSIXct vector of times corresponding to pid values
  # rh : numeric vector of relative humidity from SPod sensor
  # Returns:
  # pid : numeric vector of pid values with values before and
  # after rapid rh swings set to missing
  ###########################################################
  timeBase <- "1 min"
  # Smooth rh using 10min rolling average
  rhFilt <- stats::filter(rh, rep(1/300, 300))
  df <- data.frame(time=time, rh=rhFilt, pid=pid)
  timeBreaks <- seq(round(min(time), "hour"),
                    round(max(time), "hour"), timeBase)
  df$timeCut <- cut(df$time, timeBreaks)
  timeCuts <- unique(df$timeCut)

  # Get rh slope for each minute
  suppressWarnings(
    dfStats <- ddply(df, .(timeCut), summarize,
                     slopeRH = getSlope(rh))
  )
  # Smooth slopes over 10 mins
  slopeFilt <- stats::filter(dfStats$slopeRH, rep(1/10, 10))
  # Identify slopes greater than 10 for removal
  rhChange <- as.POSIXct(as.character(dfStats[which(abs(slopeFilt) > .1), "timeCut"]))
  if (length(rhChange) > 0){
    minRemove <- min(rhChange)-5*60
    maxRemove <- max(rhChange)+90*60
    df$pid[which(df$time > minRemove & df$time < maxRemove)] <- NA
  }
  # Remove PID data when there is no RH data
  df$pid[which(is.na(rh))] <- NA
  return(df$pid)
}
##>4 /10

screenDrops <- function(pid, time, max_iter){
  ###########################################################
  # Function to screen SPod data for rapid drops in signal
  # Args:
  # pid : numeric vector of values reported by pid sensor
  # time : POSIXct vector of times corresponding to pid values
  # max_iter : maximum number of times to run iterative algorithm
  # Returns:
  # pid : numeric vector of pid values with values during
  # drops set to missing

  # Create time vector to divide data
  timeBreaks <- seq(round(min(time), "min"),
                    round(max(time), "min"), "5 sec")
  df <- data.frame(pid=pid, time=time)
  df$timeCut <- cut(df$time, timeBreaks)
  drops <- 1
  iter <- 1
  removeList <- {}
  numBases <- 50
  while(length(drops) > 0 & iter < max_iter){

    if (iter > 10) {
      numberOfBases <- 20
    }
    dfAgg <- ddply(df, .(timeCut), summarize,
                   pid = mean(pid, na.rm=TRUE))
    dfAgg <- na.omit(dfAgg)
    dfAgg$time <- as.POSIXct(as.character(dfAgg$timeCut))
    fitS <- rq(pid~bs(time, df=50), data = dfAgg, tau=.20)
    fitS2 <- rq(pid~bs(time, df=50), data = dfAgg, tau=.10)
    dfAgg$quantileDiff <- fitS$fitted.values-fitS2$fitted.values
    dfAgg$resid <- dfAgg$pid - fitS2$fitted.values
    # plot(pid~time, dfAgg, type="l")
    # lines(fitS$fitted.values~dfAgg$time, col="red")
    # lines(fitS2$fitted.values~dfAgg$time, col="blue")
    # plot(dfAgg$quantileDiff, type="l")
    # plot(dfAgg$resid)
    threshQ <- mean(dfAgg$quantileDiff) + 6*sd(dfAgg$quantileDiff)
    threshR <- mean(dfAgg$resid[which(dfAgg$resid <0)]) -
      3*sd(dfAgg$resid[which(dfAgg$resid <0)])
    threshR <- min(threshR, -20)
    drops <- dfAgg$timeCut[which(dfAgg$quantileDiff > threshQ |
                                   dfAgg$resid < threshR)]
    removeList <- c(removeList, which(df$timeCut %in% drops))
    df$pid[removeList] <- NA
    iter <- iter + 1
    print(iter)
  }
  return(df$pid)
}
