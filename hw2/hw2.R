library(data.table)
library(ggplot2)
library(kknn)

# download data and read data into data.table format
if(!exists("used_cars")) {
  used_cars <- fread(
    'https://raw.githubusercontent.com/ChicagoBoothML/DATA___UsedCars/master/UsedCars_small.csv')  
}

# count number of samples
nb_samples <- nrow(used_cars)
# sort data set by increasing mileage
setkey(used_cars, mileage)
used_cars

n.folds <- 5

set.seed(99) # to be consistent with hw1
kv <- 2:100
cv <- docvknn(x=matrix(used_cars$mileage), 
              y=used_cars$price, 
              k=kv,
              nfold=n.folds)

# convert to RMSE
cv <- sqrt(cv/length(used_cars$price))

# how's it look?
rgy <- range(cv)
plot(log(1/kv),cv,type="l",col="red",ylim=rgy,lwd=2,cex.lab=2.0,
     xlab="log(1/k)", ylab="RMSE")

# save this cv as cv.mileage
cv.mileage <- cv

# Choose the k with the minimum Cross-validation RMSE
k.cv <- which.min(cv)

# Produce a model with this k
cv.model <- kknn(price ~ mileage,
                 train=used_cars, test=used_cars[ , .(mileage)],
                 k=k.cv, kernel='rectangular')

# add the predicted values to our data.table
used_cars$cv.predicted <- cv.model$fitted.values

# Produce the eyeball fit from hw1
k <- 30
eyeball <- kknn(price ~ mileage,
                train=used_cars, test=used_cars[ , .(mileage)],
                k=k, kernel='rectangular')

# add this prediction to used_cars data.table
used_cars$eyeball.predicted <- eyeball$fitted.values

g <- ggplot(used_cars) + 
  geom_point(aes(x=mileage, y=price, color='actual'), size=1) + 
  ggtitle('Used Cars: price vs. mileage') +
  xlab('mileage') + ylab('price')
g <- g + 
  geom_line(aes(x=mileage, y=cv.predicted, color='cv predicted'), size=0.6) +
  geom_line(aes(x=mileage, y=eyeball.predicted, color='eyeball predicted'), size=0.6) + 
  scale_colour_manual(name='price',
                      values=c(actual='blue', 
                               "cv predicted"='red',
                               "eyeball predicted"="green"))
plot(g)

# simulate a car with 100k miles
car.w.100k.miles <- data.frame(mileage=100000)
predicted.price.car.w.100k.miles <- 
  kknn(price ~ mileage,
       train=used_cars, test=car.w.100k.miles,
       k=k.cv, kernel='rectangular')$fitted.values

# 6.1
# Let's build a model including year and mileage in our covariates

# First, define our rescaling function
rescale <- function(x, xs) {
  (x - min(xs)) / (max(xs) - min(xs))
} 

# first, lets scale our covariates
used_cars$normalized.mileage <- rescale(used_cars$mileage, used_cars$mileage)
used_cars$normalized.year <- rescale(used_cars$year, used_cars$year)

cv <- docvknn(x=used_cars[, .(normalized.mileage, normalized.year)], 
              y=used_cars$price, 
              k=kv,
              nfold=n.folds)


# convert to RMSE
cv <- sqrt(cv/length(used_cars$price))

# how does our CV plot look?
rgy <- range(cv)
plot(log(1/kv),cv,type="l",col="red",ylim=rgy,lwd=2,cex.lab=2.0,
     xlab="log(1/k)", ylab="RMSE")

# Choose the k with the minimum Cross-validation RMSE
k.cv <- which.min(cv)

# fit the whole model
predicted.mileage.year <- 
  kknn(price ~ normalized.mileage + normalized.year,
     train=used_cars, test=used_cars[, .(normalized.mileage, normalized.year)],
     k=k.cv, kernel='rectangular')$fitted.values

# let's predict the value of a 2008 car having 75,000 miles
test.car <- 
  data.frame(normalized.mileage=rescale(75000, used_cars$mileage), 
             normalized.year=rescale(2008, used_cars$year))

predicted.price <- 
  kknn(price ~ normalized.mileage + normalized.year,
       train=used_cars, test=test.car,
       k=k.cv, kernel='rectangular')$fitted.values

# is our RMSE from using mileage and year better than our RMSE from using only mileage?
ggplot() + 
  geom_line(aes(x=kv, y=cv, color='mileage and year')) + 
  geom_line(aes(x=kv, y=cv.mileage, color='mileage only')) + 
  ggtitle('RMSE across Model') +
  xlab('k') + ylab('RMSE') + 
  scale_colour_manual(name='Model',
                      values=c("mileage and year"='red',
                               "mileage only"="green"))

fits <-
  data.frame(y=used_cars$price, 
             mileage=used_cars$cv.predicted, 
             mileage.year=predicted.mileage.year)

cor(fits)
pairs(fits)

