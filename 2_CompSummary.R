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

kpis2 <- rbind(# Water appearance, smell, etc.
  strip[4,] / strip[2,],
  strip[5,] / strip[2,],
  strip[6,] / strip[2,],
  strip[7,] / strip[2,])

for (i in 1:8) {kpis2[,i] <- percent(kpis2[,i])}

kpis3 <- rbind(# Health outcomes
  strip[8,] / strip[2,],
  strip[9,] / strip[2,],
  strip[10,] / strip[2,],
  strip[11,] / strip[2,],
  strip[12,] / strip[2,],
  strip[13,] / strip[2,],
  strip[14,] / strip[2,],
  strip[15,] / strip[2,])

for (i in 1:8) {kpis3[,i] <- percent(kpis3[,i])}

kpis4 <- rbind(# Fetch Time, reordered to make sense
  strip[19,] / strip[2,],
  strip[20,] / strip[2,],
  strip[22,] / strip[2,],
  strip[21,] / strip[2,],
  strip[23,] / strip[2,])

for (i in 1:8) {kpis4[,i] <- percent(kpis4[,i])}

kpis5 <- rbind(# Sanitation Facilities
  strip[25,] / strip[2,],
  strip[26,] / strip[2,],
  strip[27,] / strip[2,],
  strip[28,] / strip[2,],
  strip[29,] / strip[2,])

for (i in 1:8) {kpis5[,i] <- percent(kpis5[,i])}

kpis6 <- rbind(# Stored Fees
  round(strip[49,]/1000000, 2))

for (i in 1:8) {kpis6[,i] <- as.character(kpis6[,i])}

kpis7 <- rbind(# Functionality - from water point and water quality surveys, separate
  strip[46,] / strip[48,],
  strip[51,] / strip[53,])

for (i in 1:8) {kpis7[,i] <- percent(kpis7[,i])}

kpis8 <- rbind(# Water testing results
  round(strip[54,] / strip[56,], 2),
  round(strip[57,] / strip[59,], 2),
  round(strip[60,] / strip[62,], 2),
  round(strip[75,] / strip[77,], 2),
  round(strip[78,] / strip[80,], 2))

for (i in 1:8) {kpis8[,i] <- as.character(kpis8[,i])}

kpis9 <- rbind(# EColi risk buckets
  strip[81,] / strip[1,],
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


















