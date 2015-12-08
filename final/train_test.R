source('clean_data.R')

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
