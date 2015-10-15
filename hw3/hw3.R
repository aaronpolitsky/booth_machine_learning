#--------------------------------------------------
#load libraries
library(rpart)
library(randomForest)
library(gbm) #boosting

# import the data
used.cars <- read.csv("UsedCars.csv")

# Split the data into train, validate, and test subsets.
n <- nrow(used.cars)
n1 <- floor(n/2)
n2 <- floor(n/4)
n3 <- n - n1 - n2

set.seed(14) # Choosing 14 for consistency with slides
ii <- sample(1:n, n)
train <- used.cars[ii[1:n1],]
val   <- used.cars[ii[n1+1:n2],]
test  <- used.cars[ii[n1+n2+1:n3],]

# Experiment with and validate different approaches
# First, using mileage and year

# Single Trees
big.mileage.year.tree <- 
  rpart(price ~ mileage + year, data=train, method="anova", 
        control=rpart.control(minsplit = 5, cp = 0.00005))

index.of.best.cp <- which.min(big.mileage.year.tree$cptable[,"xerror"])
best.cp <- big.mileage.year.tree$cptable[index.of.best.cp, "CP"]
best.size <- big.mileage.year.tree$cptable[index.of.best.cp, "nsplit"] + 1

# prune to best tree size
mileage.year.tree <- prune(mileage.year.tree, cp=best.cp)
#plot(mileage.year.tree, uniform = T)
#text(mileage.year.tree, digits=4, n=T)

# Validate
pred.val.mileage.year.tree <- predict(mileage.year.tree, newdata=val)

# Random Forests
# try out different parameters
m.try <- c(floor(sqrt(ncol(used.cars)-1)), ncol(used.cars)-1) # random forests, bagging
n.tree <- c(100, 500, 1000)
random.forest.params <- expand.grid(data.frame(cbind(m.try, n.tree)))

# for each combo of options, fit trees on train while validating on val
rf.list <- dlply(random.forest.params, .(m.try, n.tree), function(x) {
  randomForest(price ~ mileage + year, data=train, mtry=x$m.try, ntree=x$n.tree,
               xtest=val[,c("mileage", "year")], ytest=val$price)
}, .progress="text")

# generate in-bag and oob loss: 1-rsq
rf.perf <-
  ldply(rf.list, c(
    ilrf=function(rf) {1-max(rf$rsq)}, 
    olrf=function(rf) {1-max(rf$test$rsq)}
  ))
  

# predict 
predicted.price.pruned.mileage.year <- predict(mileage.year.tree) 
plot(used.cars$price, predicted.price.pruned.mileage.year)
abline(0,1, col="red", lwd=3)
