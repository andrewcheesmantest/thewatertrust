####################################################################################################
# Produces a comprehensive summary from site_agg.csv 
# Begin:    Dec 9, 2016   - Create a process that rolls everything up to some basic levels for aggregating into a KPI-style dashboard
# Finalize: Dec 9, 2016   - Finished. Pretty hacky.

library(scales)

sa <- read.csv("site_agg.csv", stringsAsFactors = F, colClasses = c(NA, NA, NA, NA, NA, "character", NA, NA, NA, NA, NA, NA, 
                                                                    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                                                                    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                                                                    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                                                                    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                                                                    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))

lkup <- data.frame(vlu = c("00", "01", "11", "All"))
lkup$vlu <- as.character(lkup$vlu)

for (k in 1:4) {
  
  if(lkup[k,] == "All") { 
    sa2 <- sa 
  } else { 
    sa2 <- sa[sa$newboth == lkup[k,] ,] 
  }
  
  dummy <- data.frame()
  for (j in c(7:90)) {
    
    if(is.na(sum(sa[,j], na.rm = T)) == T | sum(sa[,j], na.rm = T) == 0 ) { 
      agg1 <- data.frame(colnames(sa2)[j], "NA", "NA") 
    } else {
      agg1 <- data.frame(colnames(sa2)[j],
                         data.frame(t(aggregate(sa2[,j] ~ srvy_tp, sa2, FUN = sum, na.rm = T)))[2,]) 
    }
    
    if(ncol(agg1) == 3) { 
      agg <- agg1 
    } else { 
      agg <- data.frame(agg1[1,1], "", agg1[1,2]) 
    }
    
    colnames(agg) <- c("vnm", "Baseline", "Monitoring")
    rownames(agg) <- j-6
    dummy <- rbind(dummy, agg)
    
  }
  
  assign(paste0("dummy_",lkup[k,]), dummy)
  
}

compout <- cbind(dummy_00, 
                 dummy_01[,c(2:3)], 
                 dummy_11[,c(2:3)],
                 dummy_All[,c(2:3)])

colnames(compout) <- c("Var", 
                       "00 - B", "00 - M", 
                       "01 - B", "01 - M", 
                       "11 - B", "11 - M",
                       "All - B", "All - M")

write.csv(compout, "comprehensive_summary.csv", row.names = F)

####################################################################################################
# Part 2: Nicer-looking version of comprehensive summary
# Compiles a smaller list of easy-to-consume KPIs

comp <- read.csv("comprehensive_summary.csv")

colnames(comp) <- c("Var", 
                    "00 - B", "00 - M", 
                    "01 - B", "01 - M", 
                    "11 - B", "11 - M",
                    "All - B", "All - M")

strip <- comp[,c(2:9)]

kpis1 <- rbind(strip[c(1:3),])

for (i in 1:8) {kpis1[,i] <- as.character(kpis1[,i])}

# Water appearance, smell, etc.
kpis2 <- rbind(strip[4,] / strip[2,],
               strip[5,] / strip[2,],
               strip[6,] / strip[2,],
               strip[7,] / strip[2,])

for (i in 1:8) {kpis2[,i] <- percent(kpis2[,i])}

# Health outcomes
kpis3 <- rbind(strip[8,] / strip[2,],
               strip[9,] / strip[2,],
               strip[10,] / strip[2,],
               strip[11,] / strip[2,],
               strip[12,] / strip[2,],
               strip[13,] / strip[2,],
               strip[14,] / strip[2,],
               strip[15,] / strip[2,])

for (i in 1:8) {kpis3[,i] <- percent(kpis3[,i])}

# Fetch Time, reordered to make sense
kpis4 <- rbind(strip[19,] / strip[2,],
               strip[20,] / strip[2,],
               strip[22,] / strip[2,],
               strip[21,] / strip[2,],
               strip[23,] / strip[2,])

for (i in 1:8) {kpis4[,i] <- percent(kpis4[,i])}

# Sanitation Facilities
kpis5 <- rbind(strip[25,] / strip[2,],
               strip[26,] / strip[2,],
               strip[27,] / strip[2,],
               strip[28,] / strip[2,],
               strip[29,] / strip[2,])

for (i in 1:8) {kpis5[,i] <- percent(kpis5[,i])}

# Stored Fees
kpis6 <- rbind(round(strip[49,]/1000000, 2))

for (i in 1:8) {kpis6[,i] <- as.character(kpis6[,i])}

# Functionality - from water point and water quality surveys, separate
kpis7 <- rbind(strip[46,] / strip[48,],
               strip[51,] / strip[53,])

for (i in 1:8) {kpis7[,i] <- percent(kpis7[,i])}

# Water testing results
kpis8 <- rbind(round(strip[54,] / strip[56,], 2),
               round(strip[57,] / strip[59,], 2),
               round(strip[60,] / strip[62,], 2),
               round(strip[75,] / strip[77,], 2),
               round(strip[78,] / strip[80,], 2))

for (i in 1:8) {kpis8[,i] <- as.character(kpis8[,i])}

# EColi risk buckets
kpis9 <- rbind(strip[81,] / strip[1,],
               strip[82,] / strip[1,],
               strip[83,] / strip[1,],
               strip[84,] / strip[1,])

for (i in 1:8) {kpis9[,i] <- percent(kpis9[,i])}

kpis <- rbind(kpis1,
              kpis2,
              kpis3,
              kpis4,
              kpis5,
              kpis6,
              kpis7,
              kpis8,
              kpis9)

kpis_out <- cbind(c("Sites",
                    "Households",
                    "People",
                    "WP - Taste Problem",
                    "WP - Smell",
                    "WP - Color",
                    "WP - Cloudy",
                    "HO - Diarrhea",
                    "HO - Typhoid",
                    "HO - Malaria",
                    "HO - Dysentery",
                    "HO - Respiratory Issues",
                    "HO - Skin Rash",
                    "HO - Eye Infection",
                    "HO - Worms",
                    "FT - LT 30M",
                    "FT - 30-60M",
                    "FT - 60-120M",
                    "FT - 120-180M",
                    "FT - GT 180M",
                    "SF - Bush",
                    "SF - Improvised Latrine",
                    "SF - Pit",
                    "SF - Latrine",
                    "SF - No, Neighbor's",
                    "MG - Stored Fees, MM USh",
                    "% Functionality, Site",
                    "% Functionality, Water Quality",
                    "WT - Electricity",
                    "WT - Temperature",
                    "WT - Turbidity",
                    "WT - E. Coli",
                    "WT - F. Coli",
                    "E.Coli Risk - Low",
                    "E.Coli Risk - Intermediate",
                    "E.Coli Risk - High",
                    "E.Coli Risk - Very High"),
                  kpis)

colnames(kpis_out)[1] <- "Measure"

write.csv(kpis_out, "Short_Summary.csv")

rm(list = ls())

####################################################################################################
# Part 3: Metrics aggregated at site level before being rolled into other filters
# Essentially, follows form of "site metric sum / site HH count"
# site_agg is dependency
# disregards metrics that counted NAs (anything ending in "_na")

library(scales)

sa <- read.csv("site_agg.csv", stringsAsFactors = F, colClasses = c(NA, NA, NA, NA, NA, "character", NA, NA, NA, NA, NA, NA, 
                                                                    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                                                                    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                                                                    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                                                                    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                                                                    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))

sa2 <- sa[,c(10:21, 24:35)]
sa3 <- sa[,c(1:9)]
for (i in 1:24) {
sa3[,(i+9)] <- sa2[,i]/sa3[,8]
colnames(sa3)[i+9] <- colnames(sa2)[i]
}

sa3$func_wp <- sa[,52]/sa[,54]
sa3$func_wq <- sa[,57]/sa[,59]
sa3$strd_fees <- sa[,55]
sa3$ecoli_risk_l <- sa[,87]
sa3$ecoli_risk_i <- sa[,88]
sa3$ecoli_risk_h <- sa[,89]
sa3$ecoli_risk_v <- sa[,90]

for (i in 34:40) {
sa3[is.na(sa3[, i]) == T, i] <- 0
}

write.csv(sa3, "site_agg_2.csv", row.names = F)

rm(list = ls())

####################################################################################################
# Part 4: Rolls site_agg_2 into a summary for easy viewing

sa <- read.csv("site_agg_2.csv", stringsAsFactors = F, colClasses = c(NA, NA, NA, NA, NA, "character", NA, NA, NA, NA, 
                                                                      NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                                                                      NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                                                                      NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))

# Splitting by Baseline/Monitoring and newboth

hldr <- data.frame(srvy_tp = rep(c("b", "m"), 4),
                   newboth = c("00", "00", "01", "01", "10", "10", "11", "11"))

for (i in 7:9) {
  hldr <- merge(hldr, 
                aggregate(sa[,i] ~ srvy_tp + newboth, sa, FUN = sum),
                by = c("srvy_tp", "newboth"),
                all.x = T)
  colnames(hldr)[i-4] <- colnames(sa)[i]
}

for (i in 10:40) {
  hldr <- merge(hldr, 
                aggregate(sa[,i] ~ srvy_tp + newboth, sa, FUN = mean),
                by = c("srvy_tp", "newboth"),
                all.x = T)
  colnames(hldr)[i-4] <- colnames(sa)[i]
  }

# Overall

hldr2 <- data.frame(srvy_tp = c("b", "m"),
                    newboth = c("All", "All"))

for (i in 7:9) {
  hldr2 <- merge(hldr2, 
                 aggregate(sa[,i] ~ srvy_tp, sa, FUN = sum),
                 by = c("srvy_tp"),
                 all.x = T)
  colnames(hldr2)[i-4] <- colnames(sa)[i]
}

for (i in 10:40) {
  hldr2 <- merge(hldr2, 
                aggregate(sa[,i] ~ srvy_tp, sa, FUN = mean),
                by = c("srvy_tp"),
                all.x = T)
  colnames(hldr2)[i-4] <- colnames(sa)[i]
}

hldr3 <- rbind(hldr, 
               hldr2)

hldr4 <- hldr3[c(1, 5, 2, 6, 3, 7, 4, 8, 9, 10),]

for (k in 6:36) {
  
  if (colnames(hldr4)[k] == "strd_fees") {
    hldr4[,k] <- formatC(hldr4[,k], format="d", big.mark=',')
  } else {
    hldr4[,k] <- percent(hldr4[,k])
  }
  
}

hldr5 <- data.frame(t(hldr4))
hldr6 <- data.frame(row.names(hldr5),
                    hldr5)

colnames(hldr6) <- c("Measure", 
                     "00 - B", "00 - M",
                     "01 - B", "01 - M",
                     "10 - B", "10 - M",
                     "11 - B", "11 - M",
                     "All - B", "All - M")

hldr6 <- hldr6[c(-1, -2),]
row.names(hldr6) <- NULL

hldr6 <- hldr6[c(1:18,20,19, 21:34),]
hldr6 <- hldr6[c(-16, -22),]

hldr6$Measure <- c("Sites",
                   "Households",
                   "People",
                   "WP - Taste",
                   "WP - Smell",
                   "WP - Color",
                   "WP - Cloudy",
                   "HO - Diarrhea",
                   "HO - Typhoid",
                   "HO - Malaria",
                   "HO - Dysentery",
                   "HO - Respiratory Issues",
                   "HO - Skin Rash",
                   "HO - Eye Infection",
                   "HO - Worms",
                   "FT - LT 30M",
                   "FT - 30-60M",
                   "FT - 60-120M",
                   "FT - 120-180M",
                   "FT - GT 180M",
                   "SF - Bush",
                   "SF - Improvised Latrine",
                   "SF - Pit",
                   "SF - Latrine",
                   "SF - No, Neighbor's",
                   "% Functionality, Site",
                   "% Functionality, Water Quality",
                   "MG - Stored Fees, USh",
                   "E.Coli Risk - Low",
                   "E.Coli Risk - Intermediate",
                   "E.Coli Risk - High",
                   "E.Coli Risk - Very High")

write.csv(hldr6, "Short_Summary_2.csv", row.names = F)





































