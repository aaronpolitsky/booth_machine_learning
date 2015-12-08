source('get_data.R')
source('clean.data.helper.R')

#empty.timestamp.seq <- seq(from=as.POSIXct(begin.2014), length.out=365*24*2, by = "30 mins") 
  

waimea.12 <- clean.data(waimea.12)
waimea.13 <- clean.data(waimea.13)
waimea.14 <- clean.data(waimea.14)

waimea <- rbind(waimea.12, waimea.13, waimea.14)

waimea.12$go.surf <- go.surf(waimea.12)
waimea.13$go.surf <- go.surf(waimea.13)
waimea.14$go.surf <- go.surf(waimea.14)

ak70.12 <- clean.data(ak70.12)
ak70.13 <- clean.data(ak70.13)
ak70.14 <- clean.data(ak70.14)

ak70 <- rbind(ak70.12, ak70.13, ak70.14)

sapply(ak70.12, function(x) mean(is.na(x))) # solid data
sapply(ak70.13, function(x) mean(is.na(x))) # missing wind data
sapply(ak70.14, function(x) mean(is.na(x))) # missing wind data, 20% missing wave data

ak72.12 <- clean.data(ak72.12)
ak72.13 <- clean.data(ak72.13)
ak72.14 <- clean.data(ak72.14)

ak72 <- rbind(ak72.12, 
              ak72.13, 
              ak72.14)

sapply(ak72.12, function(x) mean(is.na(x)))
sapply(ak72.13, function(x) mean(is.na(x)))
# sparse 2013 data
sapply(ak72.14, function(x) mean(is.na(x)))
# good 2014 data, despite it starting in June.

library(xts)

ak.active.cols <- c("WVHT", "DPD", "APD", "PRES", "ATMP", "WTMP", "DEWP")

xt.ak70.12 <- xts(ak70.12[,ak.active.cols], order.by = ak70.12$datetime)
xt.ak70.13 <- xts(ak70.13[,ak.active.cols], order.by = ak70.13$datetime)
xt.ak70.14 <- xts(ak70.14[,ak.active.cols], order.by = ak70.14$datetime)
sapply(xt.ak70.12, function(x) mean(is.na(x)))
sapply(xt.ak70.13, function(x) mean(is.na(x)))
sapply(xt.ak70.14, function(x) mean(is.na(x)))

xt.ak72.12 <- xts(ak72.12[,ak.active.cols], order.by = ak72.12$datetime)
xt.ak72.13 <- xts(ak72.13[,ak.active.cols], order.by = ak72.13$datetime)
xt.ak72.14 <- xts(ak72.14[,ak.active.cols], order.by = ak72.14$datetime)
xt.ak72    <- rbind(xt.ak72.12, xt.ak72.13, xt.ak72.14)

sapply(xt.ak72.12, function(x) mean(is.na(x)))
sapply(xt.ak72.13, function(x) mean(is.na(x)))
sapply(xt.ak72.14, function(x) mean(is.na(x)))

wa.active.cols <- c("WVHT", "DPD", "APD", "MWD", "go.surf")

sapply(waimea, function(x) mean(is.na(x)))

xt.wa.12 <- xts(waimea.12[,wa.active.cols], order.by = waimea.12$datetime)
xt.wa.13 <- xts(waimea.13[,wa.active.cols], order.by = waimea.13$datetime)
xt.wa.14 <- xts(waimea.14[,wa.active.cols], order.by = waimea.14$datetime)

sapply(xt.wa.12, function(x) mean(is.na(x)))
sapply(xt.wa.13, function(x) mean(is.na(x)))
sapply(xt.wa.14, function(x) mean(is.na(x)))

#xt.ak70.13 <- na.locf(xt.ak70.13, m)
#xt.ak70.14 <- na.locf(xt.ak70.14)
#xt.wa.13 <- na.locf(xt.wa.13)
#xt.wa.14 <- na.locf(xt.wa.14)

# create 6 days of 6h lags?
lags <- 12
lag.hrs <- 6

make.lag.list <- function(xts.data, lags, lag.hrs, orig.datetime) {
  lapply((1:lags)*lag.hrs*3600, function(lagsecs) {
    xts(xts.data, orig.datetime+lagsecs) 
  })
}

lagged.ak70.14.list <- make.lag.list(xt.ak70.14, lags, lag.hrs, ak70.14$datetime)

lagged.ak72.12.list <- make.lag.list(xt.ak72.12, lags, lag.hrs, ak72.12$datetime)
lagged.ak72.14.list <- make.lag.list(xt.ak72.14, lags, lag.hrs, ak72.14$datetime)

merged.ak70.14 <- xt.ak70.14
for(i in 1:lags) {
  merged.ak70.14 <- merge(merged.ak70.14, lagged.ak70.14.list[[i]])
}
merged.ak72.12 <- xt.ak72.12
for(i in 1:lags) {
  merged.ak72.12 <- merge(merged.ak72.12, lagged.ak72.12.list[[i]])
}
merged.ak72.14 <- xt.ak72.14
for(i in 1:lags) {
  merged.ak72.14 <- merge(merged.ak72.14, lagged.ak72.14.list[[i]])
}



lagged.ak70.13 <- 
  lapply((1:lags)*lag.hrs*3600, function(lagsecs) {xts(xt.ak70.13, ak70.13$datetime+lagsecs)})
merged.ak70.13 <- xt.ak70.13
for(i in 1:lags) {
  merged.ak70.13 <- merge(merged.ak70.13, lagged.ak70.13[[i]])
}

base.ak.to.wa.lag <- 84*3600
  
lagged.wa.14 <- lapply((1:lags)*lag.hrs*3600 + base.ak.to.wa.lag, function(lagsecs) {
  xts(xt.wa.14, waimea.14$datetime+lagsecs)
})

lagged.wa.13 <- lapply((1:lags)*lag.hrs*3600 + base.ak.to.wa.lag, function(lagsecs) {
  xts(xt.wa.13, waimea.13$datetime+lagsecs)
})

lagged.wa.12 <- lapply((1:lags)*lag.hrs*3600 + base.ak.to.wa.lag, function(lagsecs) {
  xts(xt.wa.12, waimea.12$datetime+lagsecs)
})

