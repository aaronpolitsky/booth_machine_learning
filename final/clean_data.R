source('get_data.R')

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

create.lags <- function(data, lag.num.lags) {
  
  
}
  
diablo.13 <- clean.data(diablo.13)
diablo.14 <- clean.data(diablo.14)

diablo <- rbind(diablo.13, diablo.14)

se.hawaii <- clean.data(se.hawaii)

waimea.13 <- clean.data(waimea.13)
waimea.14 <- clean.data(waimea.14)

waimea <- rbind(waimea.13, waimea.14)


alaska.13 <- clean.data(alaska.13)
alaska.14 <- clean.data(alaska.14)

alaska <- rbind(alaska.13, alaska.14)


library(xts)
xt.ak.13 <- xts(alaska.13[,-19], order.by = alaska.13$datetime)
xt.ak.14 <- xts(alaska.14[,-19], order.by = alaska.14$datetime)
xt.wa.13 <- xts(waimea.13[,-19], order.by = waimea.13$datetime)
xt.wa.14 <- xts(waimea.14[,-19], order.by = waimea.14$datetime)

# create 6 days of 6h lags
lagged.ak.14 <- 
  lapply((1:24)*6*3600, function(lagsecs) {xts(xt.ak.14[,-19], alaska.14$datetime+lagsecs)})
merged.ak.14 <- xt.ak.14
for(i in 1:24) {
  merged.ak.14 <- merge(merged.ak.14, lagged.ak.14[[i]])
}
