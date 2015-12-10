source('clean_data.R')

waimea.12 <- read.table("data/51201h2012.txt", col.names = col.names)
waimea.13 <- read.table("data/51201h2013.txt", col.names = col.names)
waimea.14 <- read.table("data/51201h2014.txt", col.names = col.names) 

ak70.12 <- read.table("data/46070h2012.txt", col.names = col.names)
ak70.13 <- read.table("data/46070h2013.txt", col.names = col.names) 
ak70.14 <- read.table("data/46070h2014.txt", col.names = col.names) 

ak72.12 <- read.table("data/46072h2012.txt", col.names = col.names)
ak72.13 <- read.table("data/46072h2013.txt", col.names = col.names)
ak72.14 <- read.table("data/46072h2014.txt", col.names = col.names)
# ak72 2014 data starts in June 

spectral.names <- c("YY", "MM", "DD", "hh", "mm", ".0200", ".0325", ".0375", ".0425", ".0475", ".0525", ".0575", ".0625", ".0675", ".0725", ".0775", ".0825", ".0875", ".0925", ".1000", ".1100", ".1200", ".1300", ".1400", ".1500", ".1600", ".1700", ".1800", ".1900", ".2000", ".2100", ".2200", ".2300", ".2400", ".2500", ".2600", ".2700", ".2800", ".2900", ".3000", ".3100", ".3200", ".3300", ".3400", ".3500", ".3650", ".3850", ".4050", ".4250", ".4450", ".4650", ".4850")
spec.ak.72.12 <- read.table("data/spectral46072h2012.txt", 
                            col.names = spectral.names)


merged.train.12 <- merge(lagged.wa.12[[1]]$go.surf, merged.ak72.12)
merged.train.12 <- na.locf(merged.train.12, maxgap = 12)

merged.test.14 <- merge(lagged.wa.14[[1]]$go.surf, merged.ak72.14)
merged.test.14 <- na.locf(merged.test.14)

set.seed(99) 
glm.fit <- glm(go.surf ~ ., merged.train.12, family=binomial())
pred <- predict.glm(object = glm.fit, type="response", newdata = merged.test.14)
pred <- (xts(pred, order.by = index(merged.test.14)))
plot(pred > .25)
plot(lagged.wa.14[[1]]$go.surf)
