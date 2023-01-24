require(quantreg)   ## Added 2019/10 by HL 
require(splines)   ## Added 2019/10 by HL 

getBaseline <- function(pid, timestamp, df=NA){
  
  pid0 <- as.data.frame(pid)[,1]
  time0 <- as.data.frame(timestamp)[,1]

  # Input checks
  if (NCOL(pid) != 1){      ## Updated 2019/10 by HL [ncol -> NCOL]
    stop("pid must one column.") 
  }
  
  if (any(is.na(time0))){
    stop("timestamp cannot include missing values")
  }
  
  if (class(time0)[1] != "POSIXct") {
    stop ("timestamp must be POSIXct class")
  }

  # Set degrees of freedom based on duration of time series
  if (is.na(df)){
    df <- 3*round(difftime(max(time0), min(time0), units="hours"))
  } 
    
  # Remove missing values from pid using last observation carry forward
  if(is.na(pid0[length(pid0)])) {pid0[length(pid0)] <- 
    pid0[max(which(!is.na(pid0)))]}
  pid0 <- zoo::na.locf(pid0, fromLast = TRUE)
  
  # Quantile regression with splines
  fit <- rq(pid0~ns(time0, df), tau=.02)
  baseline <- predict(fit)  

  # plot(pid0~time0, type="l")
  # lines(baseline~time0, col="red")
  
  return(baseline)
}