source('clean.data.helper.R')

# West to East:
# 46070:  SOUTHWEST BERING SEA SOUTHWEST BERING SEA - 142NM NNE OF ATTU IS, AK
# 46072:  CENTRAL ALEUTIANS 230NM
# 46073:  SOUTHEAST BERING SEA -205NM WNW of Dutch Harbor, AK
# 46066:  SOUTH KODIAK, AK  (good years:  2003, 2007, 2010, 2014)

# 51201:  Waimea, North Shore of Oahu, HI
#buoys <- c("46070", "46072", "46073", "46066", "51201")
buoys <- c("46070", "46066", "51201")
years <- c("2007", "2014")
source('get_data.R')

data.list <- lapply(data.list, clean.data)
sapply(data.list[c(1:(length(data.list)/2))], function(d)
  sapply(d[,c(6:15)], function(x) mean(is.na(x))))
# '70 has poor wind data
# most others look good.  

sapply(data.list[-c(1:(length(data.list)/2))], function(d)
  sapply(d[,c(6:15)], function(x) mean(is.na(x))))
# '70 again, has poor wind data.  we would need to exclude wind from the model
# '70 is missing 20% of its wave data.  we may be able to deal with this, 
#    depending on when it occurs.  Let's find out

input.cols <- c("WVHT", "DPD", "APD", "PRES", "ATMP", "WTMP")

sapply(data.list, dim)
# potential trouble with '27 in '07

# train on 07, test on 14
train.list <- data.list[c(1:length(buoys))]
train.input.list <- train.list[1:(length(buoys)-1)]
train.output.list <- train.list[length(buoys)]
test.list  <- data.list[-c(1:length(buoys))]
test.input.list <- test.list[1:(length(buoys)-1)]
test.output.list <- test.list[length(buoys)]

lapply(train.list, function(x) head(x[c(4:5)],10))
lapply(test.list, function(x) head(x[c(4:5)], 10))
# data appears to be hourly at beginning

library(xts)

xts.train.input.list <- lapply(train.input.list, function(x)
  xts(x[,input.cols], order.by = x$datetime)
)
xts.test.input.list <- lapply(test.input.list, function(x)
  xts(x[,input.cols], order.by = x$datetime)
)

sapply(names(xts.train.input.list), function(name) {
  plot(xts.train.input.list[[name]]$WVHT, main = name)
})
sapply(names(xts.train.input.list), function(name) {
  plot(xts.train.input.list[[name]]$DPD, main = name)
})

# in 2007, 66, 73, and 70 are great, 72 is bad

sapply(names(xts.test.input.list), function(name) {
  plot(xts.test.input.list[[name]]$WVHT, main = name)
})
sapply(names(xts.test.input.list), function(name) {
  plot(xts.test.input.list[[name]]$DPD, main = name)
})

# in 2008, 66 stops in september, 72 is good, 70 fades in october
# in 2014, 66 is great, but 70 fades in november
#  Let's use 66 and 70, which are geographically distant, 
#    train on 2007, test on 2014




