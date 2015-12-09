clean.data <- function(data) {
  data$datetime <- 
    strptime(with(data[, 1:5], 
                  paste(YY, MM, DD, hh, mm, sep="-")), 
             format = "%Y-%m-%d-%H-%M", tz="GMT")
  
  data$WVHT <- ifelse(data$WVHT==99, NA, data$WVHT)
  data$DPD <-  ifelse(data$DPD==99, NA, data$DPD)
  data$MWD <-  ifelse(data$MWD==999, NA, data$MWD)
  
  data$WDIR <- ifelse(data$WDIR==999, NA, data$WDIR)
  data$WSPD <- ifelse(data$WSPD==99, NA, data$WSPD)
  data$GST <- ifelse(data$GST==99, NA, data$GST)
  data$APD <- ifelse(data$APD==99, NA, data$APD)
  data$PRES <- ifelse(data$PRES==9999, NA, data$PRES)
  data$ATMP <- ifelse(data$ATMP==999, NA, data$ATMP)
  data$WTMP <- ifelse(data$WTMP==999, NA, data$WTMP)
  data$DEWP <- ifelse(data$DEWP==999, NA, data$DEWP)
  data$VIS <- ifelse(data$VIS==99, NA, data$VIS)
  data$TIDE <- ifelse(data$TIDE==99, NA, data$TIDE)
  
  data
}


go.surf <- function(data) {
  conditions <- 
    (data$WVHT > 2) & (data$WVHT < 6) &
    (data$DPD > 12) & (data$DPD < 20) 
  conditions
}

make.lag.list <- function(xts.data, lags, lag.hrs) {
  lapply((1:lags)*lag.hrs*3600, function(lagsecs) {
    xts(xts.data, index(xts.data)+lagsecs) 
  })
}

merge.lag.list <- function(xts.base, lag.list) {
  for(l in lag.list) {
    xts.base <- merge(xts.base, l)
  }
  xts.base
}
