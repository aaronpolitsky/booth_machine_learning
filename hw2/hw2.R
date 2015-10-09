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
cv <- docvknn(x=matrix(used_cars$mileage), 
              y=used_cars$price, 
              k=2:100,
              nfold=n.folds)

cv <- sqrt(cv/length(used_cars$price))

# how's it look?
plot(cv)

# Choose the k with the minimum Cross-validation RMSE
k.cv <- which.min(cv)

# Produce a model with this k
cv.model <- kknn(price ~ mileage,
                 train=used_cars, test=used_cars[ , .(mileage)],
                 k=k.cv, kernel='rectangular')

# add this to our data.table
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

plot_used_cars_data <- function(used_cars_data,
                                title='Used Cars: price vs. mileage',
                                plot_predicted=TRUE) {
  g <- ggplot(used_cars_data) +
    geom_point(aes(x=mileage, y=price, color='actual'), size=1) +
    ggtitle(title) +
    xlab('milage') + ylab('price')
  
  if (plot_predicted) {
    g <- g +
      geom_line(aes(x=mileage, y=predicted_price, color='predicted'), size=0.6) +
      scale_colour_manual(name='price',
                          values=c(actual='blue', predicted='darkorange'))
  } else {
    g <- g +
      scale_colour_manual(name='price',
                          values=c(actual='blue'))
  }
  
  g <- g +
    theme(plot.title=element_text(face='bold', size=24),
          axis.title=element_text(face='italic', size=18))
  
  g
}

plot_used_cars_data(used_cars, plot_predicted=FALSE)

