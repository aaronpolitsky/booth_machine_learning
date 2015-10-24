library(caret)
library(data.table)
library(doParallel)
library(dplyr)

# load modules from the common HelpR repo
source('EvaluationMetrics.R')

# set randomizer's seed
set.seed(99)   # Gretzky was #99

# Set up Parallelization

cl <- makeCluster(detectCores())   # I don't mind using all of my cores
clusterEvalQ(cl, library(foreach))
registerDoParallel(cl)   # register this cluster

# download data and read data into data.table format

data_folder_path <- 'data'

# Common NAs:
na_strings <- c(
  '',
  'na', 'n.a', 'n.a.',
  'nan', 'n.a.n', 'n.a.n.',
  'NA', 'N.A', 'N.A.',
  'NaN', 'N.a.N', 'N.a.N.',
  'NAN', 'N.A.N', 'N.A.N.',
  'nil', 'Nil', 'NIL',
  'null', 'Null', 'NULL')

orange <- as.data.table(read.table(
  file.path("data", 'orange_small_train.data'),
  header=TRUE, sep='\t', stringsAsFactors=TRUE, na.strings=na_strings))

num.features <- ncol(orange)
input.feature.names <- names(orange)
num.samples <- nrow(orange)

churn <- factor(
  read.table(
    file.path(data_folder_path, 'orange_small_train_churn.labels.txt'),
    header=FALSE, sep='\t')[[1]],
  levels=c(-1, 1),
  labels=c('no', 'yes'))

appetency <- factor(
  read.table(
    file.path(data_folder_path, 'orange_small_train_appetency.labels.txt'),
    header=FALSE, sep='\t')[[1]],
  levels=c(-1, 1),
  labels=c('no', 'yes'))

upsell <- factor(
  read.table(
    file.path(data_folder_path, 'orange_small_train_upselling.labels.txt'),
    header=FALSE, sep='\t')[[1]],
  levels=c(-1, 1),
  labels=c('no', 'yes'))

table(churn)
table(appetency)
table(upsell)

# upsell seems to be the least rare of the three, which might make it easier to
# predict.  Let's choose it and remove the others. 
remove(churn)
remove(appetency)

# Split into our train, validation, and test sets

train.proportion <- .8
train.indices <- createDataPartition(
  y=upsell,
  p=train.proportion,
  list=FALSE)

orange.train <- orange[train.indices, ]
orange.test <- orange[-train.indices, ]
upsell.train <- upsell[train.indices]
upsell.test <- upsell[-train.indices]

num.test.samples <- length(upsell.test)

# and Validation sets

valid.proportion <- .25
valid.indices <- createDataPartition(
  y=upsell.train,
  p=valid.proportion,
  list=FALSE)

orange.valid <- orange.train[valid.indices, ]
orange.train <- orange.train[-valid.indices, ]
upsell.valid <- upsell.train[valid.indices]
upsell.train <- upsell.train[-valid.indices]

num.train.samples <- length(upsell.train)
num.valid.samples <- length(upsell.valid)

# Let's check how balanced our upsell rates are across these:
lapply(list(upsell.train, upsell.valid, upsell.test), function(x) sum(x=='yes')/length(x))
# pretty balanced.  Well done, caret. 

##############################################################################
#  Cleaning:  very much in the spirit of the "get me outta here" code
#    But in some cases, with hopefully a cleaner/faster approach. 
##############################################################################

## Getting Rid of Input Features $x$'s with Too Many Missing Values

# examine the proportions of missing values per input feature column:

input.features.missing.proportions <-
  sapply(orange.train, function(col) sum(is.na(col))) / num.train.samples
  
hist(input.features.missing.proportions)

# Based on this, we will remove the following features:
input.feature.names[input.features.missing.proportions > .2]

# Let's remove features which exceed 20% missing data.  
input.feature.names <-
  input.feature.names[input.features.missing.proportions <= .2]

num.input.features <- length(input.feature.names)

# Throw out unused columns
orange.train <- orange.train[ , input.feature.names, with=FALSE]

# Which of these are numeric and which are categorical?
input.feature.classes <- factor(sapply(orange.train, class))

input.feature.classes

# Replacing missing values:  Numeric features

numeric.input.feature.names <-
  input.feature.names[input.feature.classes != 'factor']

# Lets check if any of these are constant across row
orig.orange.train <- copy(orange.train)

sd <- 
  summarise_each(orange.train[,numeric.input.feature.names, with=F],
                 funs(sd(., na.rm=T)))

numeric.input.feature.means <-
  sapply(orange.train[ , numeric.input.feature.names, with=FALSE],
         function(col) mean(col, na.rm=TRUE))

asdf <- copy(orig.orange.train)
mus <- sapply(asdf[, numeric.input.feature.names, with=F],
              function(col) mean(col, na.rm=T))
system.time({
  lapply(numeric.input.feature.names, 
         function(col) {
           na.rows <- is.na(asdf[[col]])
           if(sum(na.rows) > 0) {
             asdf[, col := replace(asdf[[col]], which(na.rows), mus[col]), with=F]
           }
         })
})

system.time({
  for (numeric.col in numeric.input.feature.names) {
    x <- orange.train[[numeric.col]]
    missing.value.row.yesno <- is.na(x)
    if (sum(missing.value.row.yesno) > 0) {
      orange.train[ , numeric.col := as.numeric(x), with=FALSE]
      mu <- numeric.input.feature.means[numeric.col]
      orange.train[missing.value.row.yesno, numeric.col := mu, with=FALSE]
    }
  }  
}
)
identical(asdf, orange.train)

# check our work
all.equal(
  numeric.input.feature.means,
  sapply(orange.train[ , numeric.input.feature.names, with=FALSE], mean))


#########
# Categorical features:  cleaning

categorical.input.feature.names <- 
  input.feature.names[input.feature.classes == 'factor']

num.categorical.input.feature.levels <-
  sapply(orange.train[ , categorical.input.feature.names, with=FALSE],
         function(col) length(levels(col)))

num.categorical.input.feature.levels

# discard those with greater than 500 levels, as they're probably text.  

categorical.input.feature.names <-
  categorical.input.feature.names[num.categorical.input.feature.levels <= 500]

orange.train <-
  orange.train[ , c(numeric.input.feature.names, categorical.input.feature.names), with=FALSE]

# let's make NA values its own category called zzzMissing

orig.orange.train <- copy(orange.train)
system.time(
  lapply(categorical.input.feature.names, function(colname) {
    col <- orange.train[[colname]]
    col <- addNA(col, ifany=T)
    levels(col)[is.na(levels(col))] <- 'zzzMISSING'
    orange.train[, colname := col, with=F]
  })
)

# For each categorical feature, collapse the long tail categories (less than 5%)
# keep track of the categories we collapsed, per feature

collapsed.categories.per.feature <- 
  lapply(categorical.input.feature.names, function(x) {character()})

orig.categorical.input.feature.names <- categorical.input.feature.names

asdf <- copy(orange.train)


system.time( {
  lapply(categorical.input.feature.names, function(colname) {
    col <- asdf[[colname]]
    lapply(levels(col), function(level) {
      
      level.rows <-  col == level
      if(sum(level.rows) < 0.05 * length(col)) {
        collapsed.categories.per.feature[[colname]] <- 
          c(collapsed.categories.per.feature[[colname]], level)
        asdf[level.rows, colname := 'zzzOTHER', with=F]
      }
    })
    col <- droplevels(col)
    asdf[,colname := col, with=F]
  })
  
  for(colname in categorical.input.feature.names) {
    # discard columns which have only one non-missing or non-other category
    levels <- levels(asdf[[colname]])
    if (length(levels[(levels != 'zzzMISSING') & (levels != 'zzzOTHER')]) < 2) {
      #removed.columns <- c(removed.columns, colname)
      categorical.input.feature.names <- setdiff(categorical.input.feature.names, colname)
    }
  }
}
)

my.cat.features <- categorical.input.feature.names


orig.orange.train <- copy(orange.train)

categorical.input.feature.names <- orig.categorical.input.feature.names

system.time({
  collapsed_categories <- list()
  
  for (cat_col in categorical.input.feature.names) {
    
    missing_value_row_yesno <- is.na(orange.train[[cat_col]])
    if (sum(missing_value_row_yesno) > 0) {
      orange.train[missing_value_row_yesno, cat_col := 'zzzMISSING', with=FALSE]
    }
    
    x <- orange.train[[cat_col]]
    for (cat in levels(x)) {
      cat_rows_yesno <- x == cat
      if (sum(cat_rows_yesno) < .05 * num.train.samples) {
        if (!(cat_col %in% names(collapsed_categories))) {
          collapsed_categories[[cat_col]] <- character()
        }
        collapsed_categories[[cat_col]] <- c(collapsed_categories[[cat_col]], cat)
        orange.train[cat_rows_yesno, cat_col := 'zzzOTHER', with=FALSE]
        levels(orange.train[[cat_col]])[levels(orange.train[[cat_col]]) == cat] <- NA
      }
    }
    
    cats <- levels(orange.train[[cat_col]]) 
    if ((length(cats) == 1) ||
        (length(cats[(cats != 'zzzMISSING') & (cats != 'zzzOTHER')]) < 2)) {
      categorical.input.feature.names <- setdiff(categorical.input.feature.names, cat_col)
    }
  }
})

identical(orange.train, asdf)
identical(my.cat.features, categorical.input.feature.names)



########################################################################################
# Choosing input variables
input.feature.names <- c(numeric.input.feature.names, categorical.input.feature.names)

orange.train <- orange.train[, input.feature.names, with=F]

caret_optimized_metric <- 'logLoss'   # equivalent to 1 / 2 of Deviance

caret_train_control <- trainControl(
  classProbs=TRUE,             # compute class probabilities
  summaryFunction=mnLogLoss,   # equivalent to 1 / 2 of Deviance
  method='repeatedcv',         # repeated Cross Validation
  number=5,                    # number of folds
  repeats=1,                   # number of repeats
  allowParallel=TRUE)


# Pit the features against each other.  First compare in division, then do a bracket.  
 
# Regular season:  Split into 4 divisions and grow forests  
#  for each of these, store the variable importance.  

# At the end of the round, rank the variables by importance, across all forests. 
# Choose the top 32 and form a bracket.  

if(T) {

# First, shuffle the order of features and set up divisions
set.seed(99)
competitors <- sample(input.feature.names)

north <- (1:13)
south <- (14:26)
east <- (27:39)
west <- (40:53)

B <- 500

importance <-data.frame() 

# division play
division.rf.models <- list()
for(i in 1:4) {
  division <- list(north, south, east, west)[[i]]
  print(system.time(
    division.rf.models[[i]] <- train(
      x=orange.train[, competitors[division], with=FALSE],
      y=upsell.train,
      method='parRF',     # parallel Random Forest
      metric=caret_optimized_metric,
      ntree=B,            # number of trees in the Random Forest
      nodesize=100,       # minimum node size set small enough to allow for complex trees,
      # but not so small as to require too large B to eliminate high variance
      importance=T,       #  evaluate importance of predictors
      keep.inbag=FALSE,   # not relevant as we're using Cross Validation
      trControl=caret_train_control,
      tuneGrid=NULL)
  ))
  div.imp <- varImp(division.rf.models[[i]], scale = F)
  importance <- rbind(importance, div.imp$importance[2])
}


plot(importance[order(importance, decreasing = T),])

# the top 32 teams make the playoffs bracket. 
playoff.teams <- rownames(importance)[order(importance$yes, decreasing=T)][1:32]


# Round 1:  

group.a <- playoff.teams[c(seq(1,16,4),seq(32,17,-4))]
group.b <- playoff.teams[c(seq(2,16,4),seq(31,17,-4))]
group.c <- playoff.teams[c(seq(3,16,4),seq(30,17,-4))]
group.d <- playoff.teams[c(seq(4,16,4),seq(29,17,-4))]

B <- 300
set.seed(99)
round.1.importance <- data.frame()
round.1.rf.models <- list()
for(i in 1:4) {
  g <- list(group.a, group.b, group.c, group.d)[[i]]
  print(system.time(
    round.1.rf.models[[i]] <- train(
      x=orange.train[, g, with=FALSE],
      y=upsell.train,
      method='parRF',     # parallel Random Forest
      metric=caret_optimized_metric,
      ntree=B,            # number of trees in the Random Forest
      nodesize=100,       # minimum node size set small enough to allow for complex trees,
      # but not so small as to require too large B to eliminate high variance
      importance=T,       #  evaluate importance of predictors
      keep.inbag=FALSE,   # not relevant as we're using Cross Validation
      trControl=caret_train_control,
      tuneGrid=NULL)
  ))
  imp <- varImp(round.1.rf.models[[i]], scale = F)
  round.1.importance <- rbind(round.1.importance, imp$importance[2])
}

plot(round.1.importance[order(round.1.importance, decreasing = T),])

round.2.teams <- 
  rownames(round.1.importance)[order(round.1.importance$yes, decreasing=T)][1:16]


# Round 2
alcs <- round.2.teams[c(1,3,5,7,16,14,12,10)]
nlcs <- round.2.teams[c(2,4,6,8,15,13,11,9)]

B <- 300
set.seed(99)
round.2.importance <- data.frame()
round.2.rf.models <- list()
for(i in 1:2) {
  teams <- list(alcs, nlcs)[[i]]
  print(system.time(
    round.2.rf.models[[i]] <- train(
      x=orange.train[, teams, with=FALSE],
      y=upsell.train,
      method='parRF',     # parallel Random Forest
      metric=caret_optimized_metric,
      ntree=B,            # number of trees in the Random Forest
      nodesize=100,       # minimum node size set small enough to allow for complex trees,
      # but not so small as to require too large B to eliminate high variance
      importance=T,       #  evaluate importance of predictors
      keep.inbag=FALSE,   # not relevant as we're using Cross Validation
      trControl=caret_train_control,
      tuneGrid=NULL)
  ))
  imp <- varImp(round.2.rf.models[[i]], scale = F)
  round.2.importance <- rbind(round.2.importance, imp$importance[2])
}

finalists <- 
  round.2.importance[order(round.2.importance, decreasing = T),]
names(finalists) <- 
  rownames(round.2.importance)[order(round.2.importance$yes, decreasing=T)]

plot(finalists)

# lets choose the ones that have positive importance.
champs <- finalists[finalists >= 0]

# what type of predictors are we working with?
input.feature.classes[names(champs)]

##################
# Fit our championship model
B <- 1000 #YOLO
set.seed(99)
print(system.time(
  champ.model <- train(
    x=orange.train[, names(champs), with=FALSE],
    y=upsell.train,
    method='parRF',     # parallel Random Forest
    metric=caret_optimized_metric,
    ntree=B,            # number of trees in the Random Forest
    nodesize=100,       # minimum node size set small enough to allow for complex trees,
    # but not so small as to require too large B to eliminate high variance
    importance=FALSE,       #  Don't much care now.
    keep.inbag=FALSE,   # not relevant as we're using Cross Validation
    trControl=caret_train_control,
    tuneGrid=NULL)
))

}
########################################################################################
# Prep our validation and test data

champ.factors <- names(champs)[input.feature.classes[names(champs)]=='factor']

# NA values?  
sapply(champ.factors, function(cf) sum(is.na(orange.valid[[cf]])))
sapply(champ.factors, function(cf) sum(is.na(orange.test[[cf]])))

# replace with missing tag
lapply(champ.factors, function(cf) {
  col <- orange.valid[[cf]]
  col <- addNA(col, ifany=T)
  levels(col)[is.na(levels(col))] <- 'zzzMISSING'
  orange.valid[, cf := col, with=F]
})
lapply(champ.factors, function(cf) {
  col <- orange.test[[cf]]
  col <- addNA(col, ifany=T)
  levels(col)[is.na(levels(col))] <- 'zzzMISSING'
  orange.test[, cf := col, with=F]
})


# are levels in our validation set not in our train set?  
lapply(champ.factors, function(cf) {
  levels(orange.valid[[cf]]) %in% levels(orange.train[[cf]])
})
lapply(champ.factors, function(cf) {
  levels(orange.test[[cf]]) %in% levels(orange.train[[cf]])
})

lapply(champ.factors, function(cf) {
  # for each level that is not in ouor orange.train, we collapse into 'zzzOTHER'
  levels.not.in.train <- 
    levels(orange.valid[[cf]])[!(levels(orange.valid[[cf]]) %in% 
                                   levels(orange.train[[cf]]))]
  for(l in levels.not.in.train) {
    rows <- orange.valid[[cf]] == l
    orange.valid[rows, cf := 'zzzOTHER', with=F]
  }
  orange.valid[,cf := droplevels(orange.valid[[cf]]), with=F]
})

lapply(champ.factors, function(cf) {
  # for each level that is not in ouor orange.train, we collapse into 'zzzOTHER'
  levels.not.in.train <- 
    levels(orange.test[[cf]])[!(levels(orange.test[[cf]]) %in% 
                                   levels(orange.train[[cf]]))]
  for(l in levels.not.in.train) {
    rows <- orange.test[[cf]] == l
    orange.test[rows, cf := 'zzzOTHER', with=F]
  }
  orange.test[,cf := droplevels(orange.test[[cf]]), with=F]
})

# how about now?
lapply(champ.factors, function(cf) {
  levels(orange.valid[[cf]]) %in% levels(orange.train[[cf]])
  levels(orange.test[[cf]]) %in% levels(orange.train[[cf]])
})

# good.  nothing more to prep with categorical features 

prep.numeric.data <- function(data, means, numeric.cols) {
  lapply(
    names(data)[names(data) %in% numeric.cols],
    function(col) {
      na.rows <- is.na(data[[col]])
      if(sum(na.rows) > 0) {
        data[, col := replace(data[[col]], which(na.rows), means[col]), with=F]
      }
    })
  data
}

numeric.champs <- names(champs)[input.feature.classes[names(champs)] != 'factor']
champ.means <- numeric.input.feature.means[numeric.champs]

orange.valid <- orange.valid[, names(champs), with=F]
orange.valid <- prep.numeric.data(orange.valid, champ.means, numeric.champs)

orange.test <- orange.test[, names(champs), with=F]
orange.test <- prep.numeric.data(orange.test, champ.means, numeric.champs)

########################################################################################
# Validate

low_prob <- 1e-6
high_prob <- 1 - low_prob
log_low_prob <- log(low_prob)
log_high_prob <- log(high_prob)
log_prob_thresholds <- seq(from=log_low_prob, to=log_high_prob, length.out=1000)
prob_thresholds <- exp(log_prob_thresholds)

champ.probs <- predict(champ.model, newdata = orange.valid, type="prob")
champ.perf <- bin_classif_eval(
  champ.probs$yes, upsell.valid, thresholds=prob_thresholds)


plot(x=1 - champ.perf$specificity,
     y=champ.perf$sensitivity,
     type = "l", col='darkgreen', lwd=3,
     xlim = c(0., 1.), ylim = c(0., 1.),
     main = "ROC Curve (Validation Data)",
     xlab = "1 - Specificity", ylab = "Sensitivity")
abline(a=0,b=1,lty=2,col=8)

# Not bad.  

summary(champ.perf)
# we can see that our min specificity is .3, so we don't have values on that area of the plot

########################################################################################
#We trained on our train set, which includes our validation set, because
#valid.indices were parts of our train set.  So we don't need to retrain on new data here. 

champ.probs.oos <- predict(champ.model, newdata = orange.test, type="prob")
champ.perf.oos <- bin_classif_eval(
  champ.probs.oos$yes, upsell.test, thresholds=prob_thresholds)


plot(x=1 - champ.perf.oos$specificity,
     y=champ.perf.oos$sensitivity,
     type = "l", col='darkgreen', lwd=3,
     xlim = c(0., 1.), ylim = c(0., 1.),
     main = "ROC Curves",
     xlab = "1 - Specificity", ylab = "Sensitivity")
abline(a=0,b=1,lty=2,col=8)
lines(x=1 - champ.perf$specificity,
      y=champ.perf$sensitivity,
      col='red', lwd=3)
legend('right', c('Champ RF Model (OOS)', 'Champ RF (valid)'), 
       lty=1, col=c('darkgreen', 'red'), lwd=3, cex=1.)

sensitivity.threshold <- .5

champ.perf.oos <- bin_classif_eval(champ.probs.oos$yes, upsell.test, thresholds = sensitivity.threshold)
champ.perf.oos

stopCluster(cl)   # shut down the parallel computing cluster
