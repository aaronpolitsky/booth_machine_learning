source('clean_data.R')

winter.nw.pacific.swells <- 
  (diablo$MWD >= 270) & 
  !is.na(diablo$MWD) & 
  (diablo$MM %in% c(10,11,12,1,2)) & 
  (diablo$WVHT > 1.5) & (diablo$WVHT < 3)

library(plotrix)
polar.plot(diablo$WVHT[winter.nw.pacific.swells], 
           polar.pos = diablo$MWD[winter.nw.pacific.swells], start = 90, 
           clockwise = T, 
           radial.lim=c(0,4))


hi.winter.swells <- 
  (waimea$MM %in% c(10,11,12,1,2)) & !is.na(waimea$MWD) & !is.na(waimea$WVHT)

polar.plot(waimea$WVHT[hi.winter.swells], 
           polar.pos = waimea$MWD[hi.winter.swells], start = 90, 
           clockwise = T)

# investigate the time regularity
ak70.12.time.diffs <- (ak70.12$datetime[2:nrow(ak70.12)]  - ak70.12$datetime[1:nrow(ak70.12)-1])
ak70.13.time.diffs <- (ak70.13$datetime[2:nrow(ak70.13)]  - ak70.13$datetime[1:nrow(ak70.13)-1])
ak70.14.time.diffs <- (ak70.14$datetime[2:nrow(ak70.14)]  - ak70.14$datetime[1:nrow(ak70.14)-1])

ak72.12.time.diffs  <- (ak72.12$datetime[2:nrow(ak72.12)] - ak72.12$datetime[1:nrow(ak72.12)-1])
ak72.13.time.diffs  <- (ak72.13$datetime[2:nrow(ak72.13)] - ak72.13$datetime[1:nrow(ak72.13)-1])
ak72.14.time.diffs  <- (ak72.14$datetime[2:nrow(ak72.14)] - ak72.14$datetime[1:nrow(ak72.14)-1])

wai.12.time.diffs <- (waimea.12$datetime[2:nrow(waimea.12)] - waimea.12$datetime[1:nrow(waimea.12)-1])
wai.13.time.diffs <- (waimea.13$datetime[2:nrow(waimea.13)] - waimea.13$datetime[1:nrow(waimea.13)-1])
wai.14.time.diffs <- (waimea.14$datetime[2:nrow(waimea.14)] - waimea.14$datetime[1:nrow(waimea.14)-1])

table(wai.12.time.diffs)
table(ak70.12.time.diffs)
table(ak72.12.time.diffs)

table(wai.13.time.diffs)
table(ak70.13.time.diffs)
table(ak72.13.time.diffs)

table(wai.14.time.diffs)
table(ak70.14.time.diffs)
table(ak72.14.time.diffs)

plot(xt.wa.12$WVHT)
plot(xt.ak70.12$WVHT)
plot(xt.ak72.12$WVHT)

plot(xt.wa.13$WVHT)
plot(xt.ak70.13$WVHT)
plot(xt.ak72.13$WVHT)

plot(xt.wa.14$WVHT)

