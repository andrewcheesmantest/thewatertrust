####################################################################################################
# TWT Data Formatting Script
# Sep 18, 2016
# 
# Prerequisites: HH Survey, WQ Survey (Change file names (line 24-25) as necessary)
# 
# Output: An aggregate dataset of key indicators and outcomes at the TWT_ID + Survey Type level
#         All aggregates except for ecoli and fcoli are *sums*. They must be divided by 
#         the household_counter var "hh_cntr" in order to be used as averages/rates
# 
####################################################################################################
# Setup
# 
# 1. Copy this R code to some folder on your hard disk
# 2. Copy prerequisite files into same folder
# 3. Paste folder location into "setwd" call and run
#    Note: R doesn't like the '\' character; change to '/' if copy/pasting from windows explorer

setwd("C:/Users/Andrew95733/Desktop/TWT/v2")
getwd() # confirm that setwd worked

# 4. Paste filenames in quotes

hh_fnm <- "HHSurveyJune2016.csv"
wq_fnm <- "WaterQualitySurveyJune2016.csv"

# # Run to install required packages - only necessary the first time (uncomment to run)
# install.packages("stringr")
# install.packages("sqldf")

# Run to load required packages - necessary every time R is started fresh
library(stringr, quietly = T)
library(sqldf, quietly = T)

####################################################################################################
# Creating ID-level survey results from HH-level data
# Removing extraneous fields and standardizing names
# Health outcomes preceded by "ho"
# WILL BREAK IF FIELD ARRANGEMENT CHANGES

hh <- read.csv(hh_fnm)

hh1 <- hh[,c(2, 3, 4, 6, 8, 14, 15, 16, 17, 22, 25, 32, 48, 54, 55, 56, 57, 58, 59, 60, 61, 62)]
colnames(hh1) <- c("srvy_dt", "twt_id", "srvy_typ", "hh_sz", "twt_use",
                   "taste", "smell", "color", "cloudy",
                   "ftch_tm", "brk_dwn", "san_fac", "ftch_ctnr",
                   "ho_diar", "ho_typh", "ho_malr", "ho_dysn", "ho_rspn",
                   "ho_skin", "ho_eyin", "ho_worm", "ho_othr")
hh1$hh_cntr <- "1"

# Removing Observations with Null TWT_IDs
hh1 <- hh1[is.na(hh1$twt_id)==F,]

# Formatting Survey Date as Dates
dtsplt <- data.frame(t(data.frame(strsplit(as.character(hh1$srvy_dt), ' '))))
hh1$srvy_dt <- as.Date(dtsplt[,1], format = "%m/%d/%y")
rm(dtsplt)

# Recoding Fetch Container (Dirty = 1)
hh1$ftch_ctnr <- ifelse(hh1$ftch_ctnr == "Dirty", 1, 0)

# Removing Midterm Observations
hh1 <- hh1[hh1$srvy_typ!="Midterm",]

# Aggregating to TWTID+Survey Level and recoding var nulls to zeroes
id <- sqldf("select twt_id, srvy_typ,  
             min(srvy_dt) as min_srvy_dt,
             max(srvy_dt) as max_srvy_dt,
             sum(hh_sz) as hh_sz,
             sum(twt_use) as twt_use,
             sum(taste) as taste,
             sum(smell) as smell,
             sum(color) as color,
             sum(cloudy) as cloudy,
             sum(ftch_tm) as ftch_tm,
             sum(brk_dwn) as brk_dwn,
             sum(san_fac) as san_fac,
             sum(ftch_ctnr)  as ftch_ctnr,
             sum(ho_diar) as ho_diar,
             sum(ho_typh) as ho_typh,
             sum(ho_malr) as ho_malr,
             sum(ho_dysn) as ho_dysn,
             sum(ho_rspn) as ho_rspn,
             sum(ho_skin) as ho_skin,
             sum(ho_eyin) as ho_eyin,
             sum(ho_worm) as ho_worm,
             sum(ho_othr) as ho_othr,
             sum(hh_cntr) as hh_cntr
             
             from hh1
             
             group by 1, 2")

id$srvy_typ <- factor(id$srvy_typ) # re-assign factor labels (i.e. remove "midterm")
id$min_srvy_dt <- as.Date(id$min_srvy_dt, origin = "1970-01-01") # Reformat dates
id$max_srvy_dt <- as.Date(id$max_srvy_dt, origin = "1970-01-01") # Reformat dates
id[is.na(id)] <- 0

rm(hh, hh1)

####################################################################################################
# Water Quality Data

wq <- read.csv(wq_fnm)

# Assumes units only have one value/type; retains date for potential future use
wq1 <- wq[,c(2, 3, 65, 67)]
colnames(wq1) <- c("srvy_dt","twt_id", "ecoli", "fcoli")

wq1 <- wq1[is.na(wq1$twt_id)==F,]
wq1[is.na(wq1)] <- 0

# Assumes all observations are monitoring, and averages across IDs as above 
wq2 <- sqldf("select twt_id, 'Monitoring' as srvy_typ, 
              avg(ecoli) as ecoli,
              avg(fcoli) as fcoli
              from wq1
              group by 1, 2")

rm(wq, wq1)

####################################################################################################
# Joining to ID-level

id2 <- merge(id, wq2, by = c("twt_id", "srvy_typ"), all.x = T, all.y = F)
id2[is.na(id2)] <- 0

rm(wq2, id)

####################################################################################################
# Writing to disk

write.csv(id2, "WP_Data_Reformatted.csv", row.names = F)

# # Checking date range for monitoring surveys
# # Potential todo: weight results towards earlier or later observations
# 
# test <- id2[id2$srvy_typ=="Monitoring",]
# test$diff <- test$max_srvy_dt - test$min_srvy_dt
