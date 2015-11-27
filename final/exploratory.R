source('clean_data.R')

winter.nw.pacific.swells <- 
  (diablo$MWD >= 270) & 
  !is.na(diablo$MWD) & 
  (diablo$MM %in% c(10,11,12,1,2)) & 
  (diablo$WVHT > 1.5) & (diablo$WVHT < 3)

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
wai.14.time.diffs <- (waimea.14$datetime[2:nrow(waimea.14)] - waimea.14$datetime[1:nrow(waimea.14)-1])
ak.14.time.diffs  <- (alaska.14$datetime[2:nrow(alaska.14)] - alaska.14$datetime[1:nrow(alaska.14)-1])

wai.13.time.diffs <- (waimea.13$datetime[2:nrow(waimea.13)] - waimea.13$datetime[1:nrow(waimea.13)-1])
ak.13.time.diffs  <- (alaska.13$datetime[2:nrow(alaska.13)] - alaska.13$datetime[1:nrow(alaska.13)-1])

count(wai.13.time.diffs)
count(ak.13.time.diffs)

plot(xt.wa.13$WVHT)
plot(xt.wa.14$WVHT)

