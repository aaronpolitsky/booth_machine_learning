library(caret)
library(data.table)
library(doParallel)
library(dplyr)

# load modules from the common HelpR repo
# helpr_repo_raw_url <- 'https://raw.githubusercontent.com/ChicagoBoothML/HelpR/master'
# source(file.path(helpr_repo_raw_url, 'EvaluationMetrics.R'))
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

train.proportion <- .4
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

## Getting Rid of Input Features $x$'s with Too Many Missing Values

# examine the proportions of missing values per input feature column:

input.features.missing.proportions <-
  sapply(orange.train, function(col) sum(is.na(col))) / num.train.samples
  
hist(input.features.missing.proportions)

# Let's remove features which exceed 20% missing data.  
input.feature.names <-
  input.feature.names[input.features.missing.proportions <= .2]

num.input.features <- length(input.feature.names)

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

orange.train <- copy(orig.orange.train)
numeric.input.feature.means <-
  sapply(orange.train[ , numeric.input.feature.names, with=FALSE],
         function(col) mean(col, na.rm=TRUE))

asdf <- copy(orig.orange.train)
mus <- sapply(asdf[, numeric.input.feature.names, with=F],
              function(col) mean(col, na.rm=T))
system.time({
#  asdf <- as.data.frame(orig.orange.train)
  lapply(numeric.input.feature.names, 
         function(col) {
           na.rows <- is.na(asdf[[col]])
           if(sum(na.rows) > 0) {
            # mu <- mus[col]
#             asdf[, col := as.numeric(asdf[[col]]), with=F]
             #asdf[na.rows, col := mus[col], with=F]
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