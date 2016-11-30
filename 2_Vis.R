####################################################################################################
# Script to Generate ggplot Visualizations for TWT website
# Begin:      Oct 27, 2016   -   First draft; graphs for functionality and ecoli
# Finalize:   

####################################################################################################
# TODO:                                                                                            #
# - functionalize graphing call (maybe)                                                            #
# - fix management section                                                                         #
####################################################################################################

library(ggplot2)
library(scales)

sa <- read.csv("site_agg.csv", stringsAsFactors = F)

# Setting TWT color palette
twtp <- c("#1B4463", "#378BCA", "#C9A036", "#C93E36")

############################################
# Water Functionality

sa$fscr <- sa$fnct_wp / sa$fnct_wp_tt
sa$fscr_rcd <- ifelse(sa$fscr < 1, ifelse(sa$fscr > 0, "2. Partly Functional", "1. Not Functional"), "3. Fully Functional")

# Overall for now - future filters for new/both must also apply to aggregator for legend name calculation
fnctl <- aggregate(twt_id ~ fscr_rcd, sa, FUN = length)
fplot <- ggplot(sa[is.na(sa$fscr_rcd) == F,], aes(x = factor(1), fill = fscr_rcd)) +
  theme_bw() + 
  geom_bar(width = .5, colour = "black") + 
  coord_polar("y", direction = -1) +
  scale_fill_manual("Functionality Score", 
                    values = rev(twtp),
                    labels = c(paste0("Not Func'l (", percent(fnctl[fnctl$fscr_rcd == "1. Not Functional", 2]/sum(fnctl[, 2])),")"), 
                               paste0("Partly Func'l (", percent(fnctl[fnctl$fscr_rcd == "2. Partly Functional", 2]/sum(fnctl[, 2])),")"),
                               paste0("Fully Func'l (", percent(fnctl[fnctl$fscr_rcd == "3. Fully Functional", 2]/sum(fnctl[, 2])),")"))) +
  theme(plot.title = element_text(size = rel(2)),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        
        # legend.position = "bottom",
        legend.background = element_blank(),
        legend.margin = unit(.2, "cm"),
        legend.text = element_text(size = rel(1)),
        
        panel.border = element_blank()) +
  ggtitle("Well Functionality")

############################################
# E. Coli

sa$eca <- NA
sa[sa$ecoli_risk_l == 1, "eca"] <- "1. Low"
sa[sa$ecoli_risk_i == 1, "eca"] <- "2. Medium"
sa[sa$ecoli_risk_h == 1, "eca"] <- "3. High"
sa[sa$ecoli_risk_v == 1, "eca"] <- "4. V. High"

# Overall for now - future filters for new/both must also apply to aggregator for legend name calculation
ecoli <- aggregate(twt_id ~ eca, sa, FUN = length)
eplot <- ggplot(sa[is.na(sa$eca) == F,], aes(x = factor(1), fill = eca)) +
  theme_bw() + 
  geom_bar(width = .5, colour = "black") + 
  coord_polar("y", direction = -1) +
  scale_fill_manual("Exposure Level", 
                    values = twtp,
                    labels = c(paste0("Low (", percent(ecoli[ecoli$eca == "1. Low", 2]/sum(ecoli[, 2])),")"), 
                               paste0("Medium (", percent(ecoli[ecoli$eca == "2. Medium", 2]/sum(ecoli[, 2])),")"),
                               paste0("High (", percent(ecoli[ecoli$eca == "3. High", 2]/sum(ecoli[, 2])),")"),
                               paste0("V. High (", percent(ecoli[ecoli$eca == "4. V. High", 2]/sum(ecoli[, 2])),")"))) +
  theme(plot.title = element_text(size = rel(2)),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        
        # legend.position = "bottom",
        legend.background = element_blank(),
        legend.margin = unit(.2, "cm"),
        legend.text = element_text(size = rel(1)),
        
        panel.border = element_blank()) +
  ggtitle("Exposure to E. Coli")

############################################
# Health

# FOR JUST BOTH + NEW SITES
sa_sbst <- sa[sa$newboth == 11,]

# Reorganizing data (to long) for ggplot facets
hhct <- aggregate(hhs ~ twt_id + srvy_tp, sa_sbst, FUN = sum)
dout <- data.frame()
ho_nms <- c("Diarrhea", "Typhoid", "Malaria", "Dysentery", "Respiratory Problems", "Skin Problems", "Eye Infection", "Intestinal Worms")

for (j in 14:21) {
  # NOTE: this may break if the distribution of nulls changes among the HO vars. Fix.
  otcm <- aggregate(sa_sbst[,j] ~ twt_id + srvy_tp, sa_sbst, FUN = sum)
  otcm <- merge(otcm, hhct, by = c("twt_id", "srvy_tp"))
  otcm$rt <- otcm[,3] / otcm[,4]
  otcd <- data.frame(var = ho_nms[j-13], # substr(names(sa_sbst)[j],4,7),
                     otcm[,c("twt_id", "srvy_tp", "rt")])
  dout <- rbind(dout, otcd)
}

dout2 <- aggregate(rt ~ var + srvy_tp, dout, FUN = mean)

hplot <- ggplot(dout2, aes(x = srvy_tp, y = rt, fill = srvy_tp)) +
  geom_bar(stat = "identity") + 
  geom_text(data=dout2 ,aes(x = srvy_tp, label = percent(rt)), vjust = -.5) +
  facet_grid(~var, switch = "x") +
  theme_bw() +
  scale_fill_manual("Survey Type", 
                    values = twtp[2:1],
                    labels = c("Baseline", "Monitoring")) +
  theme(plot.title = element_text(size = rel(2)),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.margin = unit(.2, "cm"),
        legend.text = element_text(size = rel(1)),
        
        panel.border = element_blank()) +
  ggtitle("Health Outcomes")


############################################
# Outputting 

png(file = "functionality.png")
fplot
dev.off()

png(file = "ecoli.png")
eplot
dev.off()

png(file = "health.png", width = 2000)
hplot
dev.off()

############################################
# Management (Lat Cov, Mgmt Sust, Fin Sust)
# TODO: - pull management sustainability from base datasets into site_agg
#       - what to do with financial sustainability? not using midterm surveys ATM
#       - other vars to pull in?

sa$sanfac <- sa$sf_ImpLatr + sa$sf_Pit + sa$sf_Latr
sa$finsst <- sa$strd_fees_sum/3630 # using exchange rate from 11/30/2016, google
# sa$mgtsst (DEFINE)

# FOR JUST BOTH + NEW SITES
sa_sbst <- sa[sa$newboth == 11,]

# Reorganizing data (to long) for ggplot facets; non-loop approach given only two vars and differing calculations in them
hhct <- aggregate(hhs ~ twt_id + srvy_tp, sa_sbst, FUN = sum)
sf <- merge(aggregate(sanfac ~ twt_id + srvy_tp, sa_sbst, FUN = sum), 
            hhct, 
            by = c("twt_id", "srvy_tp"))
sf$rt <- sf[,3] / sf[,4]
fs <- rbind(aggregate(finsst ~ twt_id + srvy_tp, sa_sbst, FUN = sum),
            data.frame(twt_id = aggregate(finsst ~ twt_id + srvy_tp, sa_sbst, FUN = sum)[,1],
                       srvy_tp = "b", 
                       finsst = 0))

mgmt1 <- rbind(data.frame(var = "Latrine Coverage", 
                         sf[,c("twt_id", "srvy_tp", "rt")]))

mgmt1 <- rbind(data.frame(var = "Latrine Coverage", 
                          fs[,c("twt_id", "srvy_tp", "rt")]))

mgmt2 <- aggregate(rt ~ var + srvy_tp, mgmt, FUN = mean)

ggplot(mgmt2, aes(x = srvy_tp, y = rt, fill = srvy_tp)) +
  geom_bar(stat = "identity") + 
  geom_text(data=mgmt2 ,aes(x = srvy_tp, label = percent(rt)), vjust = -.5) +
  facet_grid(~var, switch = "x") +
  theme_bw() +
  scale_fill_manual("Survey Type", 
                    values = twtp[2:1],
                    labels = c("Baseline", "Monitoring")) +
  theme(plot.title = element_text(size = rel(2)),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.margin = unit(.2, "cm"),
        legend.text = element_text(size = rel(1)),
        
        panel.border = element_blank()) +
  ggtitle("Health Outcomes")


























