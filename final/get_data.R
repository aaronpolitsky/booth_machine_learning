# read in data
col.names <- c("YY","MM","DD","hh","mm",
               "WDIR","WSPD","GST","WVHT","DPD","APD",
               "MWD","PRES","ATMP","WTMP","DEWP","VIS","TIDE")

data.path <- "data/"

asdf <- expand.grid(years, buoys)

library(plyr)
filenames <- alply(expand.grid(buoys, years), 1, function(row) 
  as.character(paste0(row$Var1, "h", row$Var2, ".txt.gz")))

data.list <- lapply(filenames, function(filename)
  read.table(gzfile(description = paste0(data.path, filename)), 
             col.names = col.names)
)
names(data.list) <- filenames
