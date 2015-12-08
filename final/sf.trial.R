source('get_data.R')
source('clean.data.helper.R')

data.list <- lapply(data.list, clean.data)
sapply(data.list, function(d)
  sapply(d, function(x) mean(is.na(x))))

# SF 237 doesn't measure wind and has very few missing wave height and period readings
# West SF has good wind, wave, and weather data.  

# "WDIR" "WSPD" "GST"  "WVHT" "DPD"  "APD"  "PRES" "ATMP" "WTMP"
input.cols <- c(6:11,13:15)

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

