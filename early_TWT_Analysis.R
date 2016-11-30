
# install.packages("ggplot2")
library(ggplot2)
library(dplyr)
library(lubridate)

id <- read.csv("WP_Data_Reformatted.csv")
id$min_srvy_dt <- as.Date(id$min_srvy_dt, format = "%Y-%m-%d")
id$max_srvy_dt <- as.Date(id$max_srvy_dt, format = "%Y-%m-%d")

# Health Outcome Vars, controlling for # Surveys

id$diar <- id$ho_diar / id$hh_cntr
id$typh <- id$ho_typh / id$hh_cntr
id$malr <- id$ho_malr / id$hh_cntr
id$dysn <- id$ho_dysn / id$hh_cntr
id$rspn <- id$ho_rspn / id$hh_cntr
id$skin <- id$ho_skin / id$hh_cntr
id$eyin <- id$ho_eyin / id$hh_cntr
id$worm <- id$ho_worm / id$hh_cntr
id$othr <- id$ho_othr / id$hh_cntr

bl <- id[id$srvy_typ == "Baseline",]
bl <- subset(bl, bm == 1)
mn <- id[id$srvy_typ == "Monitoring",]
mn <- subset(mn, bm == 1)

# Reformatting some vars to get a better-looking set of plots

blsb <- bl[,c(1, 3, 24, 30:38)]

bl_lng <- data.frame()
for (i in 3:12) {
  
  var <- data.frame(twt_id = blsb$twt_id,
                    dat = blsb$min_srvy_dt,
                    var = colnames(blsb)[i],
                    val = blsb[,i])
  bl_lng <- rbind(bl_lng, var)
  rm(var)
}

mnsb <- mn[,c(1, 3, 24, 30:38)]

mn_lng <- data.frame()
for (i in 3:12) {
  
  var <- data.frame(twt_id = blsb$twt_id,
                    dat = mnsb$min_srvy_dt,
                    var = colnames(mnsb)[i],
                    val = mnsb[,i])
  mn_lng <- rbind(mn_lng, var)
  rm(var)
}

lng <- rbind(data.frame(srvy = "Baseline", bl_lng),
             data.frame(srvy = "Monitoring", mn_lng))

# Producing Individual Graphs for Each Var

# Baseline
varunq <- as.character(unique(lng$var))

for (i in 2:10) {
  
  blpind <- ggplot(lng[lng$srvy == "Baseline" & lng$var == varunq[i],], aes(x = dat, y = val)) + 
    geom_point() + 
    geom_smooth() +
    ggtitle(paste0("Baseline - ", varunq[i])) + 
    scale_x_date(date_breaks = "6 months", date_labels = "%m/%y", 
                 limit = c(as.Date("2007-12-01", format = "%Y-%m-%d"), NA)) +
    scale_y_continuous(limits = c(0,1)) +
    theme(title = element_text(size = 10),
          axis.title = element_blank(),
          axis.text.x = element_text(size = 6, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 6))
  
  pnm <- paste0("plots/baseline_", varunq[i], ".png")
  ggsave(pnm, 
         blpind, 
         width = 6, 
         height = 4, 
         dpi = 1200)
  
}

# Baseline
varunq <- as.character(unique(lng$var))

for (i in 2:10) {
  
  blpind <- ggplot(lng[lng$srvy == "Monitoring" & lng$var == varunq[i],], aes(x = dat, y = val)) + 
    geom_point() + 
    geom_smooth() +
    ggtitle(paste0("Monitoring - ", varunq[i])) + 
    scale_x_date(date_breaks = "6 months", date_labels = "%m/%y", 
                 limit = c(as.Date("2007-12-01", format = "%Y-%m-%d"), NA)) +
    scale_y_continuous(limits = c(0,1)) +
    theme(title = element_text(size = 10),
          axis.title = element_blank(),
          axis.text.x = element_text(size = 6, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 6))
  
  pnm <- paste0("plots/monitoring_", varunq[i], ".png")
  ggsave(pnm, 
         blpind, 
         width = 6, 
         height = 4, 
         dpi = 1200)
  
}

# Overall Graphs

# Normalized health outcomes - baseline, facet
blp3 <- ggplot(lng[lng$srvy == "Baseline",], aes(x = dat, y = val)) + 
  geom_jitter() + 
  geom_smooth() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Baseline Health Outcomes Values") + 
  theme_bw()
plot(blp3)

# Normalized health outcomes - monitoring, facet
blp4 <- ggplot(lng[lng$srvy == "Monitoring",], aes(x = dat, y = val)) + 
  geom_jitter() + 
  geom_smooth() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Monitoring Health Outcomes Values") + 
  theme_bw()
plot(blp4)

# Normalized health outcomes - both survey types, colored
blp5 <- ggplot(lng, aes(x = dat, y = val, colour = srvy)) + 
  geom_jitter() + 
  # geom_smooth() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Health Outcomes Values") + 
  theme_bw()
plot(blp5)

####################################################################################################
# Checking Change in Diarrhea Incidence

diarb <- lng[lng$var=="diar" & lng$srvy=="Baseline", c(-4)]
diarm <- lng[lng$var=="diar" & lng$srvy=="Monitoring", c(-4)]

diarb$qntl <- ntile(diarb$val, 5)
diarm$qntl <- ntile(diarm$val, 5)

diar <- merge(diarb[,c(-1)], diarm[,c(-1)], 
              by = "twt_id",
              all.x = T, all.y = T)

colnames(diar) <- c("twt_id", "dat_b", "val_b", "qntl_b", "dat_m", "val_m", "qntl_m")

lng2 <- merge(lng, diar[,c(1, 4, 7, 8)], by = "twt_id", all.x = T, all.Y = F)
lng2$mvmt2 <- factor(ifelse(lng2$qntl_m > lng2$qntl_b, "Inc", ifelse(lng2$qntl_m == lng2$qntl_b, "Stt", "Dec")))

# Contextualizing with diarrhea movement - baseline only
# Does where sites started with diarrhea affect where they ended up?
# Observations: 
#   As expected, those whose rates decreased started at least somewhat high; inverse is true for those whose rates increased
#   More interesting, sites that remained static are scattered about

blp5 <- ggplot(lng2[lng2$srvy == "Baseline" & lng2$var == "diar",], 
               aes(x = dat, 
                   y = val, 
                   shape = mvmt2, 
                   colour = mvmt2, 
                   size = 2)) + 
  geom_jitter() + 
  # geom_smooth() +
  # facet_wrap(~ var, scales = "free") +
  ggtitle("Health Outcomes Values") + 
  theme_bw()
# png("monitoring.png")
plot(blp5)
# dev.off()

# Experimenting with TS
# First, aggregate at monthly level

dts <- data.frame(d_pct = lng[lng$var=="diar" & lng$srvy=="Baseline",5],
                  yr = year(lng[lng$var=="diar" & lng$srvy=="Baseline",3]),
                  mo = month(lng[lng$var=="diar" & lng$srvy=="Baseline",3]))
dts$dt <- as.Date(paste0(dts$yr, "-", dts$mo, "-", "01"), format = "%Y-%m-%d")

dtsag <- aggregate(d_pct ~ dt, dts, FUN = mean)

time <- data.frame(dt = seq.Date(from = min(dtsag[,1]), to = max(dtsag[,1]), by = "month"))
time2 <- merge(time, dtsag, by = "dt", all.x = T, all.y = T)
time2[is.na(time2)] <- 0

timeseries <- ts(time2[,2], 
                 start = as.Date("2008-03-01", format = "%Y-%m-%d"), 
                 frequency = 12)

plot(stl(timeseries, 
         s.window = "periodic", 
         robust = T))













































