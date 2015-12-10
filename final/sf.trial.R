source('clean.data.helper.R')

# 46059: 359NM west of San Francisco
# 46237: San Francisco Bar
# 46002:  275NM West of Coos Bay, OR
buoys <- c("46059", "46002", "46237")
years <- c("2008", "2011")

source('get_data.R')

data.list <- lapply(data.list, clean.data)
sapply(data.list, function(d)
  sapply(d, function(x) mean(is.na(x))))

# SF 237 doesn't measure wind and has very few missing wave height and period readings
# West SF has good wind, wave, and weather data.  
# West OR has great wind and wave data in '08, but only wave data in '11

sapply(data.list, dim)
# we have about twice as many output rows.  This could be to time-resoultion

# split out data from 2011 for now.
data.2011 <- data.list[c(4:6)]
data.2008 <- data.list[c(1:3)]

lapply(data.2008, function(x) x[1:15,c(4:5)])
lapply(data.2011, function(x) x[1:15,c(4:5)])
# at least at the beginning, our data appears to be either hourly or
# half-hourly.  This does not pose a problem in itself.

# create our go.surf condition:
data.2008[[3]]$go.surf <- go.surf(data.2008[[3]])
table(data.2008[[3]]$go.surf)
data.2011[[3]]$go.surf <- go.surf(data.2011[[3]])
table(data.2011[[3]]$go.surf)

# Converting to time series
library(xts)

input.cols <- c("WVHT", "DPD",  "APD",  "PRES", "ATMP", "WTMP")
input.list.2008 <- data.2008[c(1,2)]
output.list.2008 <- data.2008[3]
input.list.2011 <- data.2011[c(1,2)]
output.list.2011 <- data.2011[3]

xts.input.list.2008 <- lapply(input.list.2008, function(x)
  xts(x[,input.cols], order.by = x$datetime)
)
xts.input.list.2011 <- lapply(input.list.2011, function(x)
  xts(x[,input.cols], order.by = x$datetime)
)

sapply(xts.input.list.2008, function(x) {
  plot(x$WVHT)
  plot(x$DPD)
})
sapply(xts.input.list.2011, function(x) {
  plot(x$WVHT)
  plot(x$DPD)
})

output.cols <- c("WVHT", "DPD",  "APD",  "MWD",  "WTMP", "go.surf")
xts.output.list.2008 <- lapply(output.list.2008, function(x)
  xts(x[,output.cols], order.by = x$datetime)
)
xts.output.list.2011 <- lapply(output.list.2011, function(x)
  xts(x[,output.cols], order.by = x$datetime)
)

library(ggplot2)
merged <- merge(xts.input.list.2011[[1]],
                xts.input.list.2011[[2]],
                xts.output.list.2011[[1]])
merged <- na.locf(merged, maxgap = 2)
sapply(merged, function(x) mean(is.na(x)))
# not bad.

window <- "2011-10/2011-11"
ggplot(merged[window], aes(x=index(merged[window]))) + 
  geom_line(aes(y=WVHT, color = "input CA")) + 
  geom_line(aes(y=WVHT.1, color="input OR")) +  
  geom_line(aes(y=WVHT.2, color="output CA")) 
  
window <- "2011-11-1/2011-11-15"
ggplot(merged[window], aes(x=index(merged[window]))) + 
  geom_line(aes(y=WVHT, color = "input CA")) + 
  geom_line(aes(y=WVHT.1, color="input OR")) +  
  geom_line(aes(y=WVHT.2, color="output CA")) 

window <- "2011-11-10/2011-11-12"
ggplot(merged[window], aes(x=index(merged[window]))) + 
  geom_line(aes(y=WVHT, color = "input CA")) + 
  geom_line(aes(y=WVHT.1, color="input OR")) +  
  geom_line(aes(y=WVHT.2, color="output CA")) 

# lets work on our output.  First, standardize its time. 
# Create an empty timestamp sequence
xts.empty.2008 <- 
  xts(order.by = 
        seq(from=as.POSIXct("2008-01-01", tz="GMT"), 
            to = as.POSIXct("2009-01-01", tz="GMT"), 
            by = "30 mins") 
  )
xts.empty.2011 <- 
  xts(order.by = 
        seq(from=as.POSIXct("2011-01-01", tz="GMT"), 
            to = as.POSIXct("2012-01-01", tz="GMT"), 
            by = "30 mins") 
  )

# how do they compare in length?
length(index(xts.empty.2008))
length(index(xts.output.list.2008[[1]]))
length(index(xts.empty.2011))
length(index(xts.output.list.2011[[1]]))

# we are missing some values.  Let's identify the distribution of missing time
count(diff(index(xts.output.list.2008[[1]])))
# Mostly 30min intervals, a number of hour-long ones that we can fill in, and the occasional longer interval. 
count(diff(index(xts.output.list.2011[[1]])))

# Let's merge them together now
xts.merged.output.2008 <- merge(xts.empty.2008, xts.output.list.2008[[1]])
xts.merged.output.2011 <- merge(xts.empty.2011, xts.output.list.2011[[1]])

# And fill in gaps of up to 120 min
xts.output.2008 <- na.locf(xts.merged.output.2008, maxgap = 3)
xts.output.2011 <- na.locf(xts.merged.output.2011, maxgap = 3)

# now we have some observations that are on :30  and between half hours.  toss the in-betweeners
xts.output.2008 <- merge(xts.empty.2008, xts.output.2008, join = "inner")
xts.output.2011 <- merge(xts.empty.2011, xts.output.2011, join = "inner")
# now we have our output series on the :30 and :60

sapply(xts.output.2008, function(x) mean(is.na(x)))
sapply(xts.output.2011, function(x) mean(is.na(x)))

# Let's convert to hourly data, the easy way
xts.output.2008 <- xts.output.2008[seq(1, nrow(xts.output.2008), by = 2)]
xts.output.2011 <- xts.output.2011[seq(1, nrow(xts.output.2011), by = 2)]

##############
#### Input ###

xts.empty.2008 <- 
  xts(order.by = 
        seq(from=as.POSIXct("2008-01-01", tz="GMT"), 
            to = as.POSIXct("2009-01-01", tz="GMT"), 
            by = "1 hour") 
  )
xts.empty.2011 <- 
  xts(order.by = 
        seq(from=as.POSIXct("2011-01-01", tz="GMT"), 
            to = as.POSIXct("2012-01-01", tz="GMT"), 
            by = "1 hour") 
  )


length(index(xts.empty.2008))
length(index(xts.input.list.2008[[1]]))
length(index(xts.input.list.2008[[2]]))
length(index(xts.empty.2011))
length(index(xts.input.list.2011[[1]]))
length(index(xts.input.list.2011[[2]]))

# some missing observations. especially in OR in 2011

count(diff(index(xts.input.list.2008[[1]])))
# Nice. only a few hourly obs are missing

count(diff(index(xts.input.list.2008[[2]])))
# only a couple outings over two hours.  Plus that big one in the summer.  

count(diff(index(xts.input.list.2011[[1]])))
count(diff(index(xts.input.list.2011[[2]])))

# Let's merge them together now
xts.merged.input.2008 <- merge(xts.empty.2008, 
                          xts.input.list.2008[[1]],
                          xts.input.list.2008[[2]])
sum(complete.cases(xts.merged.input.2008))
xts.merged.input.2011 <- merge(xts.empty.2011, 
                               xts.input.list.2011[[1]],
                               xts.input.list.2011[[2]])
sum(complete.cases(xts.merged.input.2011))

# And fill in gaps of up to 120 min
xts.input.2008 <- na.locf(xts.merged.input.2008, maxgap = 3)
sum(complete.cases(xts.input.2008))
xts.input.2011 <- na.locf(xts.merged.input.2011, maxgap = 3)
sum(complete.cases(xts.input.2011))

# now we have some observations that are on our hourly grid  toss the in-betweeners
xts.input.2008 <- merge(xts.empty.2008, xts.input.2008, join = "inner")
xts.input.2011 <- merge(xts.empty.2011, xts.input.2011, join = "inner")
sum(complete.cases(xts.input.2008))
sum(complete.cases(xts.input.2011))

# let's create a window of time in our input buoys:  how about 3 days of lags
#input.lag.list <- make.lag.list(xts.input, lags=72, lag.hrs = 1)
input.lag.list.2008 <- make.lag.list(xts.input.2008, lags=12, lag.hrs = 1, offset.hrs = 10)
input.2008 <- merge.lag.list(input.lag.list.2008)
count(diff(index(input.2008)))

input.lag.list.2011 <- make.lag.list(xts.input.2011, lags=12, lag.hrs = 1, offset.hrs = 10)
input.2011 <- merge.lag.list(input.lag.list.2011)
count(diff(index(input.2011)))

dim(input.2008)
sum(complete.cases(input.2008))
# plenty of complete rows
plot(y=complete.cases(input.2008), x=index(input.2008))

dim(input.2011)
sum(complete.cases(input.2011))
plot(y=complete.cases(input.2011), x=index(input.2011))
# not so many, but winter is OK

# Train on 2008, test on 2011
train <- merge(xts.output.2008$go.surf, input.2008)
test  <- merge(xts.output.2011$go.surf, input.2011)

sum(complete.cases(train))
sum(complete.cases(test))

mean(train$go.surf, na.rm=T)
mean(test$go.surf, na.rm=T)
# enough, reasonably balanced

library(h2o)

#set.seed(99) 
#train.indices <- sample(1:nrow(all), size = nrow(all)*.75)
#train <- all[train.indices,]
#test <- all[-train.indices,]

#mean(train$go.surf, na.rm=T)
#mean(test$go.surf, na.rm=T)
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
    epochs=c(2), 
    hidden=list(#c(1024, 1024, 1024), #c(1024,512,256), 
                #c(512, 512),
                c(1024), #c(512), c(256), c(128), 
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
#model.paths.12hr.10offset.ca.or.sf.2008.train <- 
#  lapply(dl.grid.models, function(m) h2o.saveModel(m, path="models"))
#save(model.paths.12hr.10offset.ca.or.sf.2008.train, 
#     file="model.paths.12hr.10offset.ca.or.sf.2008.train")

ptest.list <- lapply(dl.grid.models, function(m) h2o.performance(m, test_hex))
cm.test.list <- lapply(ptest.list, function(ptest) h2o.confusionMatrix(ptest))

library(plyr)
ptest.df <- ldply(cm.test.list, 
                  function(cm) 
                    c(tot.test.error.rate = cm$Error[3]))
ptest.df <- cbind(ptest.df, expand.grid((hyper.params)))
ptest.df
best.model.index <- which.min(ptest.df$tot.test.error.rate)
best.dl.model <- dl.grid.models[[best.model.index]]
cm.test.list[[best.model.index]]

plot(ptest.list[[best.model.index]])
prediction.2011 <- as.data.frame(h2o.predict(dl.grid.models[[best.model.index]], newdata = test_hex))









hyper.params <- 
  list(
    epochs=c(2), 
    hidden=list(c(1024, 1024, 1024),# c(1024,512,256), 
                #c(512, 512),
                c(1024), #c(512), c(256), c(128), 
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
dl.grid.models.short <- lapply(dl.grid@model_ids, function(id) h2o.getModel(id))
#model.paths.short <- lapply(dl.grid.models.short, function(m) h2o.saveModel(m, path="models"))
#save(model.paths.short, file="model.paths.short.Rda")

ptest.list.short <- lapply(dl.grid.models.short, function(m) h2o.performance(m, test_hex))
cm.test.list.short <- lapply(ptest.list.short, function(ptest) h2o.confusionMatrix(ptest))

library(plyr)
ptest.df.short <- ldply(cm.test.list.short, 
                  function(cm) 
                    c(tot.test.error.rate = cm$Error[3]))
ptest.df.short <- cbind(ptest.df.short, expand.grid((hyper.params)))

best.model.index.short <- which.min(ptest.df.short$tot.test.error.rate)
best.dl.model.short <- dl.grid.models.short[[best.model.index.short]]
cm.test.list.short[[best.model.index.short]]

plot(ptest.list.short[[best.model.index.short]])
# not bad at all. 

#######################################
# It may not be fair to be training and testing on the same year since we use lags

data.2011 <- lapply(data.2011, clean.data)
data.2011[[3]]$go.surf <- go.surf(data.2011[[3]])
table(data.2011[[3]]$go.surf)

input.list <- data.2011[c(1,2)]
output.list <- data.2011[3]

sapply(input.list, function(d)
  sapply(d, function(x) mean(is.na(x))))
# oregon doesn't seem to have wind data. we'll have to exclude that.

xts.input.list <- list(xts(input.list$`46059h2011.txt.gz`[, input.cols], 
                           order.by = input.list$`46059h2011.txt.gz`$datetime),
                       xts(input.list$`46002h2011.txt.gz`[, c(9:11,13:15)], 
                           order.by = input.list$`46002h2011.txt.gz`$datetime))

sapply(output.list, function(d)
  sapply(d, function(x) mean(is.na(x))))

output.cols <- c(9:12,15, 20)
xts.output.list <- lapply(output.list, function(x)
  xts(x[,output.cols], order.by = x$datetime)
)

merged <- merge(xts.input.list[[1]],
                xts.input.list[[2]],
                xts.output.list[[1]])
merged <- na.locf(merged, maxgap = 2)
sapply(merged, function(x) mean(is.na(x)))


