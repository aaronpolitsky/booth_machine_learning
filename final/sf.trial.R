# 46059: 359NM west of San Francisco
# 46237: San Francisco Bar
# 46002:  275NM West of Coos Bay, OR
buoys <- c("46059", "46002", "46237")
years <- c("2008")

source('get_data.R')
source('clean.data.helper.R')

data.list <- lapply(data.list, clean.data)
sapply(data.list, function(d)
  sapply(d, function(x) mean(is.na(x))))

# SF 237 doesn't measure wind and has very few missing wave height and period readings
# West SF has good wind, wave, and weather data.  
# West OR has great wind and wave data, except for a few columns

sapply(data.list, dim)
# we have about twice as many output rows.  This could be to time-resoultion

lapply(data.list, function(x) x[1:15,c(4:5)])
# at least at the beginning, our data appears to be either hourly or half-hourly.  This does not pose a problem in itself.  

# create our go.surf condition:
data.list$`46237h2008.txt.gz`$go.surf <- go.surf(data.list$`46237h2008.txt.gz`)
table(data.list$`46237h2008.txt.gz`$go.surf)
#data.list$`46237h2011.txt.gz`$go.surf <- go.surf(data.list$`46237h2011.txt.gz`)
#table(data.list$`46237h2011.txt.gz`$go.surf)

# Converting to time series
library(xts)

# "WDIR" "WSPD" "GST"  "WVHT" "DPD"  "APD"  "PRES" "ATMP" "WTMP"
input.cols <- c(6:11,13:15)

input.list <- data.list[c(1,2)]
output.list <- data.list[3]

xts.input.list <- lapply(input.list, function(x)
  xts(x[,input.cols], order.by = x$datetime)
)

sapply(xts.input.list, function(x) {
  plot(x$WVHT)
  plot(x$DPD)
})

output.cols <- c(9:12,15,20)
xts.output.list <- lapply(output.list, function(x)
  xts(x[,output.cols], order.by = x$datetime)
)

library(ggplot2)
merged <- merge(xts.input.list$`46059h2008.txt.gz`,
                xts.input.list$`46002h2008.txt.gz`,
                xts.output.list$`46237h2008.txt.gz`)
merged <- na.locf(merged, maxgap = 2)
sapply(merged, function(x) mean(is.na(x)))
# not bad.

window <- "2008-02/2008-03"
ggplot(merged[window], aes(x=index(merged[window]))) + 
  geom_line(aes(y=WVHT, color = "input")) + 
  geom_line(aes(y=WVHT.1, color="output")) 

window <- "2008-02-17/2008-02-22"
ggplot(merged[window], aes(x=index(merged[window]))) + 
  geom_line(aes(y=WVHT, color = "input")) + 
  geom_line(aes(y=WVHT.1, color="output")) 
# not a big lag at all. 

# window <- "2011-03-18/2011-03-20"
# ggplot(merged[window], aes(x=index(merged[window]))) + 
#   geom_line(aes(y=WVHT, color = "input")) + 
#   geom_line(aes(y=WVHT.1, color="output")) 
# # and maybe a 10hr travel time here

# lets work on our output.  First, standardize its time. 
# Create an empty timestamp sequence
xts.empty <- xts(order.by = 
                   seq(from=as.POSIXct(index(xts.output.list$`46237h2008.txt.gz`)[1]), 
                       to = as.POSIXct(tail(index(xts.output.list$`46237h2008.txt.gz`), n=1))+900, 
                       by = "30 mins")
                 )

# how do they compare in length?
length(index(xts.empty))
length(index(xts.output.list$`46237h2008.txt.gz`))

# we are missing some values.  Let's identify the distribution of missing time
count(diff(index(xts.output.list$`46237h2008.txt.gz`)))
# Mostly 30min intervals, a number of hour-long ones that we can fill in, and the occasional longer interval. 

# Let's merge them together now
xts.merged.output <- merge(xts.empty, xts.output.list$`46237h2008.txt.gz`)

# And fill in gaps of up to 120 min
xts.output <- na.locf(xts.merged.output, maxgap = 3)

# now we have some observations that are on :30  and between half hours.  toss the in-betweeners
xts.output <- merge(xts.empty, na.locf(xts.output), join = "inner")
# now we have our output series on the :30 and :60

sapply(xts.output, function(x) mean(is.na(x)))

# Let's convert to hourly data, the easy way
xts.output <- xts.output[seq(1, nrow(xts.output), by = 2)]

##############
#### Input ###

xts.empty <- xts(order.by = 
                   seq(from=as.POSIXct(index(xts.input.list$`46059h2008.txt.gz`)[1]), 
                       to = as.POSIXct(tail(index(xts.input.list$`46059h2008.txt.gz`), n=1)), 
                       by = "60 mins")
)

length(index(xts.empty))
length(index(xts.input.list[[1]]))
length(index(xts.input.list[[2]]))

# some missing observations. 

count(diff(index(xts.input.list$`46059h2008.txt.gz`)))
# Nice. only a few hourly obs are missing

count(diff(index(xts.input.list$`46002h2008.txt.gz`)))
# only a couple outings over two hours.  Plus that big one in the summer.  

# Let's merge them together now
xts.merged.input <- merge(xts.empty, 
                          xts.input.list[[1]],
                          xts.input.list[[2]])
sum(complete.cases(xts.merged.input))

# And fill in gaps of up to 120 min
xts.input <- na.locf(xts.merged.input, maxgap = 2)
sum(complete.cases(xts.input))

# now we have some observations that are on our hourly grid  toss the in-betweeners
xts.input <- merge(xts.empty, na.locf(xts.input), join = "inner")


# let's create a window of time in our input buoys:  how about 3 days of lags
#half.day.lag.list <- make.lag.list(xts.input.list$`46059h2008.txt.gz`, lags = 6, lag.hrs = 12)
#six.hr.lag.list <- make.lag.list(xts.input.list$`46059h2008.txt.gz`, lags=12, lag.hrs = 6)
#input.lag.list <- make.lag.list(xts.input, lags=24, lag.hrs = 3)
input.lag.list <- make.lag.list(xts.input, lags=72, lag.hrs = 1)


#train.12hr <- merge.lag.list(xts.input.list$`46059h2008.txt.gz`, half.day.lag.list)
#train.6hr  <- merge.lag.list(xts.input.list$`46059h2008.txt.gz`, six.hr.lag.list)
input.train  <- merge.lag.list(xts.input, input.lag.list)
#input.train.hourly <- merge.lag.list(xts.input, every.hr.lag.list)

count(diff(index(input.train)))

dim(input.train)
sum(complete.cases(input.train))
# plenty of complete rows

# OK, but it takes swells time to travel to our output buoy, so for our
# observation at the beach, we need to consider input that happened some time
# before.  Our input window should be enough to cover this.  The middle lags
# will be more important.

all <- merge(xts.output$go.surf, input.train)
#all <- merge(xts.output$go.surf, input.train.hourly)

library(h2o)

set.seed(99) 
train.indices <- sample(1:nrow(all), size = nrow(all)*.75)
train <- all[train.indices,]
test <- all[-train.indices,]

mean(train$go.surf, na.rm=T)
mean(test$go.surf, na.rm=T)
# balanced

#########################
# modeling

# start or connect to h2o server
h2oServer <- h2o.init(max_mem_size="4g", nthreads=-1)

# we need to load data into h2o format
train_hex = as.h2o(data.frame(x=train[,-1], y=as.factor(train[,1])))
test_hex = as.h2o(data.frame(x=test[,-1], y=as.factor(test[,1])))

predictors <- 1:(ncol(train_hex)-1)
response <- ncol(train_hex)

hyper.params <- 
  list(
    epochs=c(2,10), 
    hidden=list(c(1024, 1024, 1024), c(1024,512,256), 
                c(512, 512),
                c(1024), c(512), c(256), c(128), 
                c(64)),
    activation=c("Tanh", "TanhWithDropout")
  )

set.seed(99)
system.time(
  dl.grid <- h2o.grid(
    algorithm = "deeplearning",
    x=predictors, y=response,
    training_frame=train_hex,
    classification_stop=-1,  # Turn off early stopping
    l1=1e-5,
    hyper_params = hyper.params
  ) 
)
summary(dl.grid)
dl.grid.models <- lapply(dl.grid@model_ids, function(id) h2o.getModel(id))
model.paths <- lapply(dl.grid.models, function(m) h2o.saveModel(m, path="models"))

ptest.list <- lapply(dl.grid.models, function(m) h2o.performance(m, test_hex))
cm.test.list <- lapply(ptest.list, function(ptest) h2o.confusionMatrix(ptest))

library(plyr)
ptest.df <- ldply(cm.test.list, 
                  function(cm) 
                    c(tot.test.error.rate = cm$Error[3]))
ptest.df <- cbind(ptest.df, expand.grid((hyper.params)))

best.model.index <- which.min(ptest.df$tot.test.error.rate)
best.dl.model <- dl.grid.models[[best.model.index]]
cm.test.list[[best.model.index]]

plot(ptest.list[[best.model.index]])
