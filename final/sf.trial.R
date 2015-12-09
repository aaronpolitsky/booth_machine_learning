source('get_data.R')
source('clean.data.helper.R')

data.list <- lapply(data.list, clean.data)
sapply(data.list, function(d)
  sapply(d, function(x) mean(is.na(x))))

# SF 237 doesn't measure wind and has very few missing wave height and period readings
# West SF has good wind, wave, and weather data.  

sapply(data.list, dim)
# we have about twice as many output rows.  This could be to time-resoultion

lapply(data.list, function(x) x[1:15,c(4:5)])
# at least at the beginning, our data appears to be either hourly or half-hourly.  This does not pose a problem in itself.  

# create our go.surf condition:
data.list$`46237h2008.txt.gz`$go.surf <- go.surf(data.list$`46237h2008.txt.gz`)
table(data.list$`46237h2008.txt.gz`$go.surf)
data.list$`46237h2011.txt.gz`$go.surf <- go.surf(data.list$`46237h2011.txt.gz`)
table(data.list$`46237h2011.txt.gz`$go.surf)

# Converting to time series
library(xts)

# "WDIR" "WSPD" "GST"  "WVHT" "DPD"  "APD"  "PRES" "ATMP" "WTMP"
input.cols <- c(6:11,13:15)

input.list <- data.list[c(1,3)]
output.list <- data.list[c(2,4)]

xts.input.list <- lapply(input.list, function(x)
  xts(x[,input.cols], order.by = x$datetime)
)

sapply(xts.input.list, function(x) {
  plot(x$WVHT)
  plot(x$DPD)
})

output.cols <- c(9:15,20)
xts.output.list <- lapply(output.list, function(x)
  xts(x[,output.cols], order.by = x$datetime)
)

library(ggplot2)
merged <- merge(xts.input.list$`46059h2011.txt.gz`, xts.output.list$`46237h2011.txt.gz`)
merged <- na.locf(merged, maxgap = 10)

window <- "2011-02/2011-03"
ggplot(merged[window], aes(x=index(merged[window]))) + 
  geom_line(aes(y=WVHT, color = "input")) + 
  geom_line(aes(y=WVHT.1, color="output")) 

window <- "2011-03-15/2011-03-25"
ggplot(merged[window], aes(x=index(merged[window]))) + 
  geom_line(aes(y=WVHT, color = "input")) + 
  geom_line(aes(y=WVHT.1, color="output")) 

window <- "2011-03-24/2011-03-26"
ggplot(merged[window], aes(x=index(merged[window]))) + 
  geom_line(aes(y=WVHT, color = "input")) + 
  geom_line(aes(y=WVHT.1, color="output")) 
# there could be about a 18hr travel time here

window <- "2011-03-18/2011-03-20"
ggplot(merged[window], aes(x=index(merged[window]))) + 
  geom_line(aes(y=WVHT, color = "input")) + 
  geom_line(aes(y=WVHT.1, color="output")) 
# and maybe a 10hr travel time here

# let's create a window of time in our input buoy:  how about 3 days of half-day lags
half.day.lag.list <- make.lag.list(xts.input.list$`46059h2008.txt.gz`, lags = 6, lag.hrs = 12)
six.hr.lag.list <- make.lag.list(xts.input.list$`46059h2008.txt.gz`, lags=12, lag.hrs = 6)
three.hr.lag.list <- make.lag.list(xts.input.list$`46059h2008.txt.gz`, lags=24, lag.hrs = 3)

train.12hr <- merge.lag.list(xts.input.list$`46059h2008.txt.gz`, half.day.lag.list)
train.6hr  <- merge.lag.list(xts.input.list$`46059h2008.txt.gz`, six.hr.lag.list)
train.3hr  <- merge.lag.list(xts.input.list$`46059h2008.txt.gz`, three.hr.lag.list)  

# lets work on our output.  First, standardize its time. 
# Create an empty timestamp sequence
empty.timestamp.seq <- seq(from=as.POSIXct(index(xts.output.list$`46237h2008.txt.gz`)[1]), 
                           to = as.POSIXct(tail(index(xts.output.list$`46237h2008.txt.gz`), n=1))+900, 
                           by = "30 mins") 

# how do they compare?
length(empty.timestamp.seq)
length(index(xts.output.list$`46237h2008.txt.gz`))

# we are missing some values.  Let's identify the distribution of missing time

count(diff(index(xts.output.list$`46237h2008.txt.gz`)))

# Mostly 30min intervals, a number of hour-long ones that we can fill in, and the occasional longer interval. 

# Let's merge them together now

