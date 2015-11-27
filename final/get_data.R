# read in data
col.names <- c("YY","MM","DD","hh","mm",
               "WDIR","WSPD","GST","WVHT","DPD","APD",
               "MWD","PRES","ATMP","WTMP","DEWP","VIS","TIDE")
diablo.13 <- read.table("data/46215h2013.txt", col.names = col.names)
diablo.14 <- read.table("data/46215h2014.txt", col.names = col.names)
se.hawaii <- read.table("data/51004h2014.txt", col.names = col.names)
waimea.13 <- read.table("data/51201h2013.txt", col.names = col.names)
waimea.14 <- read.table("data/51201h2014.txt", col.names = col.names) 

alaska.13 <- read.table("data/46070h2013.txt", col.names = col.names) 
alaska.14 <- read.table("data/46070h2014.txt", col.names = col.names) 

