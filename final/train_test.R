source('clean_data.R')

library(h20)

set.seed(99) 
library(h2o)


merged.test.13 <- merge(lagged.wa.13[[lags]]$go.surf, merged.ak.13)
merged.test.13 <- na.locf(merged.train.13)

merged.train.14 <- merge(lagged.wa[[lags]]$go.surf, merged.ak.14)
merged.train.14 <- na.locf(merged.train.14)

glm.fit <- glm(go.surf ~ ., merged.train.14, family=binomial())
pred <- predict.glm(object = glm.fit, newdata = merged.test.13)
