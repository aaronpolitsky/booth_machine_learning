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

#########################################
#  Input Prep

lapply(xts.train.input.list, function(x) count(diff(index(x))))
lapply(xts.test.input.list, function(x) count(diff(index(x))))
# input is hourly

xts.empty.train <- 
  xts(order.by = 
        seq(from=as.POSIXct("2007-01-01", tz="GMT"), 
            to = as.POSIXct("2008-01-01", tz="GMT"), 
            by = "60 mins") 
  )
xts.empty.test <- 
  xts(order.by = 
        seq(from=as.POSIXct("2014-01-01", tz="GMT"), 
            to = as.POSIXct("2015-01-01", tz="GMT"), 
            by = "60 mins") 
  )

dim(xts.train.input.list[[1]])
dim(xts.train.input.list[[2]])
dim(xts.test.input.list[[1]])
dim(xts.test.input.list[[2]])

lapply(xts.train.input.list, function(x) count(diff(index(x))))
# training data is pretty tight given it was active

lapply(xts.test.input.list, function(x) count(diff(index(x))))
# as is test, but a bit less so


# merge into our standardized time
xts.merged.train.input <-
  merge(xts.empty.train, 
        xts.train.input.list[[1]],
        xts.train.input.list[[2]])
sum(complete.cases(xts.merged.train.input))
dim(xts.merged.train.input)

xts.merged.test.input <-
  merge(xts.empty.test, 
        xts.test.input.list[[1]],
        xts.test.input.list[[2]])
sum(complete.cases(xts.merged.test.input))
dim(xts.merged.test.input)
# some time issues

# fill some gaps up to a limit
xts.train.input <- na.locf(xts.merged.train.input, maxgap = 4)
sum(complete.cases(xts.train.input))
plot(complete.cases(xts.merged.train.input))
plot(complete.cases(xts.merged.train.input))

xts.test.input  <- na.locf(xts.merged.test.input, maxgap = 4)
mean(complete.cases(xts.test.input))
plot(complete.cases(xts.merged.test.input))
plot(complete.cases(xts.test.input))
dim(xts.test.input) # we still need to collapse down to the grid

# now we need to only keep those obs that are on our hourly grid.  toss the in-betweeners
xts.train.input <- merge(xts.empty.train, xts.train.input, join = "inner")
mean(complete.cases(xts.train.input))
xts.test.input <- merge(xts.empty.test, xts.test.input, join = "inner")
mean(complete.cases(xts.test.input))
# interpolate with a spline curve up to a point
xts.test.input <- na.spline(xts.test.input, maxgap = 36) # needed to do something
mean(complete.cases(xts.test.input))
plot(complete.cases(xts.test.input))

dim(xts.train.input)
dim(xts.test.input)
# very nice.

# Lets create a lagged window of input.  Since we are far from our destination,
# we can and will need to use a wider window centered at a longer time away from
# the destination due to swell travel time


input.train.window <- 
  merge.lag.list(
    make.lag.list(xts.train.input, 
                  lags=96,  
                  lag.hrs = 2,   
                  offset.hrs = 72))

dim(input.train.window)
mean(complete.cases(input.train.window))

input.test.window <- 
  merge.lag.list(
    make.lag.list(xts.test.input, 
                  lags=96, 
                  lag.hrs = 2, 
                  offset.hrs = 72)
  )
plot(complete.cases(input.test.window))
dim(input.test.window)
mean(complete.cases(input.test.window))


#########################################
#  Output Prep

train.output.list[[1]]$go.surf <- go.surf(train.output.list[[1]])
test.output.list[[1]]$go.surf <- go.surf(test.output.list[[1]])

xts.train.output.list <- lapply(train.output.list, function(x)
  xts(x$go.surf, order.by = x$datetime)
)
xts.test.output.list <- lapply(test.output.list, function(x)
  xts(x$go.surf, order.by = x$datetime)
)

xts.train.output <- xts.train.output.list[[1]]
xts.test.output <- xts.test.output.list[[1]]

names(xts.train.output) = "go.surf"
names(xts.test.output) = "go.surf"

length(index(xts.empty.train))
length(index(xts.train.output))
length(index(xts.empty.test))
length(index(xts.test.output))
# probably double time

# we are missing some values.  Let's identify the distribution of missing time
count(diff(index(xts.train.output)))
count(diff(index(xts.test.output)))

# Let's merge them together now
xts.merged.train.output <- merge(xts.empty.train, xts.train.output)
xts.merged.test.output  <- merge(xts.empty.test,  xts.test.output)

xts.train.output <- na.locf(xts.merged.train.output, maxgap = 3)
xts.test.output  <- na.locf(xts.merged.test.output,  maxgap = 3)

# now we have some observations that are on :30  and between half hours.  toss the in-betweeners
xts.train.output <- merge(xts.empty.train, xts.train.output, join = "inner")
xts.test.output <- merge(xts.empty.test, xts.test.output, join = "inner")
# now we have our output series on the :30 and :60

dim(xts.train.output)
dim(xts.test.output)
mean(is.na(xts.train.output))
mean(is.na(xts.test.output))

count(xts.test.output[complete.cases(input.train.window)])


##### Training and Testing


train <- merge(xts.train.output$go.surf, input.train.window)
test  <- merge(xts.test.output$go.surf,   input.test.window)

mean(train$go.surf, na.rm=T)
mean(test$go.surf, na.rm=T)

library(h2o)

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
    hidden=list(#c(1024, 1024, 1024), 
      #c(1024,512,256)
      c(64,64,64),
      c(512, 512),
      #,c(1024), #c(512), c(256), c(128)
      c(256),
      c(64)
    ),
    activation=c("Tanh", "TanhWithDropout"),
    #    input_dropout_ratio=c(0, .2),
    #    l1 = c(1e-5, 0.2)
    #activation=c("TanhWithDropout"),
    input_dropout_ratio=c(0, .2),
    l1 = c(1e-5, 0.2)
  )
expand.grid(hyper.params)

set.seed(99)
runtime <- system.time(
  dl.grid <- h2o.grid(
    algorithm = "deeplearning",
    x=predictors, y=response,
    training_frame=train_hex,
    classification_stop=-1,  # Turn off early stopping
    #l1=1e-5,
    variable_importances = T,
    hyper_params = hyper.params
  ) 
)
summary(dl.grid)
dl.grid.models <- lapply(dl.grid@model_ids, function(id) h2o.getModel(id))
model.paths <- 
  lapply(dl.grid.models, function(m) h2o.saveModel(m, path="models"))
#save(model.paths, 
#     file="model.paths.12.11.125pm.Rda")
#save(hyper.params, 
#     file="hyper.params.12.11.125pm.Rda")
#load(file = "model.paths.12hr.10offset.ca.or.sf.2008.train.Rda")
#dl.grid.models <- lapply(model.paths.12hr.10offset.ca.or.sf.2008.train, function(p) h2o.loadModel(p))

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
#prediction.2011 <- as.data.frame(h2o.predict(dl.grid.models[[best.model.index]], newdata = test_hex))
print(runtime)





