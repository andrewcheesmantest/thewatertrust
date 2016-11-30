
# TWT Time Series Investigation and Calibration
# 10/4/16

getwd()
library(lubridate)
library(ggplot2)

d <- read.csv("WP_Data_Reformatted.csv")
d2 <- data.frame(id = d$twt_id,
                 st = d$srvy_typ,
                 dt = as.Date(d$min_srvy_dt, format = "%Y-%m-%d"),
                 diar = d$ho_diar/d$hh_cntr) # diar pct
db <- d2[d2$st == "Baseline",]
dm <- d2[d2$st == "Monitoring",]

# Creating TWTID-level dataset (diff fields for B/M)
d3 <- merge(db, dm, "id", all = F)
d3 <- d3[,c(1, 3, 4, 7)] # removing duplicated fields
colnames(d3) <- c("id", "dt", "b", "m")
d3$c <- 1

# # Creating long-form (diff records for B/M)
# d4 <- merge(d2, d3[,c(1, 4)], by = "id")

# Time series work - mostly focuses on d3
# to do: check other normalizing functions
#        asses exact shift (currently appears as residuals)
#        deal with noise - maybe differemt aggregating scale

d3$df <- (d3$m - d3$b)
rm(d, d2, db, dm)

# Filling gaps
sq <- data.frame(dts = seq(min(d3$dt), max(d3$dt), by = "day"))
sq2 <- merge(sq, d3, by.x = "dts", by.y = "dt", all.x = T)
sq2[is.na(sq2$b),3] <- 0
sq2[is.na(sq2$m),4] <- 0
sq2[is.na(sq2$c),5] <- 0
sq2[is.na(sq2$df),6] <- 0
rm(sq)

# Aggregating TSs to different levels
sq2$wk <- (year(sq2$dt)*100) + week(sq2$dt)
sq2$mn <- (year(sq2$dt)*100) + month(sq2$dt)
sq2$qt <- (year(sq2$dt)*100) + quarter(sq2$dt)
sq2$yr <- (year(sq2$dt))

# wkly <- aggregate(df ~ wk, sq2, FUN = mean)
mnly <- aggregate(df ~ mn, sq2, FUN = mean)
# qtly <- aggregate(df ~ qt, sq2, FUN = mean)
# anly <- aggregate(df ~ yr, sq2, FUN = mean)


# Treating outliers (extreme negative values)
plot(mnly[,2])
mnly[mnly$df<=-0.04,2] <- mean(mnly$df)

# wts <- ts(wkly$df, frequency = 52)
mts <- ts(mnly[c(10:nrow(mnly)),2], 
          frequency = 12, 
          start = min(as.Date(paste0(substr(mnly[,1],0,4),"-",
                                     substr(mnly[,1],5,6),"-01"),
                              format = "%Y-%m-%d"))) # omitting first few observations which skew the TS (all are zeroes)
# qts <- ts(qtly$df, frequency = 4)
# ats <- ts(anly$df, frequency = 1)

# plot(wts)
plot(mts)
# plot(qts)
# plot(ats)

# plot(stl(wts, s.window = "periodic", robust = T))
mtsstl <- stl(mts, s.window = "periodic", robust = T)
mtsstl
plot(mtsstl)
# plot(stl(qts, s.window = "periodic", robust = T))
# plot(stl(ats, s.window = "periodic", robust = T))



write.csv(mtsstl$weights, "mtsstl.csv")

# lookong at breakdown rates

hist(d$brk_dwn/d$hh_cntr, 
     breaks = 50)

bd <- ggplot(d, aes(x = as.Date(min_srvy_dt, format = "%Y-%m-%d"),
                    y = (brk_dwn / hh_cntr),
                    col = srvy_typ))
bd <- bd + geom_jitter() + geom_smooth()
plot(bd)

# Plots

# Diar pct values over time (monitoring v baseline)
p <- ggplot(d2, aes(x = dt, y = diar, col = st))
p <- p + geom_jitter() + geom_smooth()
plot(p)

# Distribution of surveys by diar pct (monitoring v baseline)
h <- ggplot(d4, aes(diar, col = st)) +
  geom_freqpoly(bins = 300)
plot(h)

hist(d2$diar)











new_variable_count <- ifelse(old_variable == "Value1". 1. 0)











