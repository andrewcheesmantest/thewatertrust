####################################################################################################
# Script to Process TWT Data from mWater
# Begin:      Oct 6, 2016   -   First draft
# Update:     Nov 5, 2016   -   Taking logic, adding buckets
# Finalize:   Nov 12, 2016  -   Finalizing buckets and logic; making script more stable
# Update:     Nov 16, 2016  -   Added ecoli buckets; made final dataset preparation more stable

# TODO:     Dec 9, 2016   -   TODO: create comprehensive summary on top of site_agg - simple aggregate metrics across B/M for each var DONE
#                         -   TODO: maybe standardize format so that graphing is easy 
#                         -   TODO: fix differences btw HH/WP-level information (functional HH v functional WP) DONE
#                         -   TODO: include new site-survey level denominator for summary 
#                         -   TODO: bucket functionality data (functional, partially functional, not functional)

library(sqldf)
library(plyr)

hh <- read.csv("hh_survey_20161214.csv", stringsAsFactors = F)
wp <- read.csv("site_quality_20161214.csv", stringsAsFactors = F)
wq <- read.csv("water_quality_20161214.csv", stringsAsFactors = F)

####################################################################################################
# Hand-recoding mWater's shitty field names; see "names.xlsx" for a full mapping

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ----------> THIS WILL BREAK IF FIELD ORDER CHANGES AT ALL <---------- #
# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ #

colnames(hh) <- c('depl', 'enum', 'stts', 'resp_cd', 'drft_dt', 'subm_dt', 'twt_id', 'srvy_tp', 'perm', 
                  'hh_nm', 'hh_sz', 'hh_sz_dk', 
                  'twt_wp_use', 'twt_wp_use_dk', 'alt_wp_src_1', 'alt_wp_src_2', 'alt_wp_suff_wtr', 'alt_wp_suff_wtr_dk', 
                  'wp_prblm_taste', 'wp_prblm_smell', 'wp_prblm_color', 'wp_prblm_cloudy', 'alt_wp_prblms_1', 'alt_wp_prblms_2', 'alt_wp_prblms_3', 
                  'ftch_prsn', 'ftch_tm', 'ftch_insuff', 'ftch_insuff_dk', 'ftch_brkdwn', 'ftch_brkdwn_dk', 
                  'pay_fee', 'pay_fee_dk', 'pay_fee_amt', 'pay_fee_unts', 'pay_fee_amt_dk', 
                  'san_fac', 'san_fac_na', 'latr_walls', 'latr_door', 'latr_roof', 'latr_na', 
                  'hyg_fac_hand_wash_1', 'hyg_fac_bath_shltr', 'hyg_fac_rubbsh_pt', 'hyg_fac_ktchn', 'hyg_fac_dryng_rck', 
                  'hyg_fac_dryng_ln', 'hyg_fac_anml_hs', 'hyg_fac_na', 'hyg_fac_hand_wash_2', 'hyg_fac_hand_wash_na', 
                  'ftch_cntnr', 'ftch_cntnr_strd', 'ftch_cntnr_cvrd', 'ftch_cntnr_abvgrd', 'ftch_cntnr_scoop', 'ftch_cntnr_na', 
                  'ho_diar', 'ho_typh', 'ho_malr', 'ho_dysn', 'ho_resp', 'ho_skin', 'ho_eyin', 'ho_wrms', 'ho_othr_1', 'ho_othr_2', 'ho_dk', 
                  'satsfd', 'unsat_rsn', 'loc_lat', 'loc_lon', 'loc_acc', 'loc_alt', 'hh_comment', 'hh_comment_na')

colnames(wp) <- c('depl', 'enum', 'stts', 'resp_cd', 'drft_dt', 'subm_dt', 'twt_id', 'srvy_tp', 'perm', 'perm_no_rsn', 
                  'rspndnt_commchr', 'rspndnt_mmbrs', 'rspndnt_entrprn', 'rspndnt_lic1', 'rspndnt_usrs', 
                  'functl', 'functl_part', 'rqst_mchnc', 'rqst_mchnc_cmmnt', 
                  'wuc_comm_actv', 'wuc_comm_actv_dk', 'wuc_comm_rcrds', 'wuc_comm_rcrd_updtd', 'wuc_comm_rcrd_hh_nm', 'wuc_comm_rcrd_hh_nm_dk', 'wuc_comm_rst_tm', 
                  'wuc_comm_mmbrs_male', 'wuc_comm_mmbrs_male_dk', 'wuc_comm_mmbrs_female', 'wuc_comm_mmbrs_female_dk', 'wuc_comm_ldr_female', 
                  'wuc_comm_fees', 'wuc_comm_fees_dk', 'wuc_comm_fees_amt', 'wuc_comm_fees_unts', 'wuc_comm_fees_amt_dk', 'wuc_comm_fees_hh_nm', 'wuc_comm_fees_hh_nm_dk', 
                  'wuc_comm_fees_strd_yn', 'wuc_comm_fees_strd_yn_dk', 'wuc_comm_fees_strd_amt', 'wuc_comm_fees_strd_unts', 'wuc_comm_fees_strd_amt_dk', 
                  'hnd_pmp_mchnc', 'hnd_pmp_mchnc_cmmnt', 'hnd_pmp_mchnc_dk', 'hnd_pmp_mchnc_pd', 'hnd_pmp_mchnc_pd_dk', 'hnd_pmp_mchnc_amt', 'hnd_pmp_mchnc_unts', 'hnd_pmp_mchnc_amt_dk', 
                  'wp_brkdwn_lyr', 'wp_brkdwn_lyr_cs', 'wp_brkdwn_lyr_rspn', 'wp_brkdwn_lyr_r_mtg', 'wp_brkdwn_lyr_r_mchnc', 'wp_brkdwn_lyr_r_twt', 'wp_brkdwn_lyr_r_dwes', 'wp_brkdwn_lyr_othr_1', 'wp_brkdwn_lyr_othr_2', 
                  'wp_brkdwn_rpr_md', 'wp_brkdwn_rpr_tm', 'wp_brkdwn_rpr_tm_dk', 'wp_brkdwn_rpr_cst_amt', 'wp_brkdwn_rpr_cst_unts', 
                  'wp_brkdwn_spr_prts_cst_amt', 'wp_brkdwn_spr_prts_cst_unts', 'wp_brkdwn_spr_prts_cst_na', 'wp_brkdwn_spr_prts_cst_dk', 'wp_brkdwn_spr_prts_prcrmnt', 'wp_brkdwn_spr_prts_prcrmnt_cmmnt', 'wp_brkdwn_spr_prts_prcrmnt_dk', 'wp_cmmnts')

colnames(wq) <- c('depl', 'enum', 'stts', 'resp_cd', 'drft_dt', 'subm_dt', 'twt_id', 'functl', 'functl_part', 'functl_dk', 'smpl_src', 
                  'tst_prfrm_ph', 'tst_prfrm_trbdt', 'tst_prfrm_ecoli', 'tst_prfrm_temp', 'tst_prfrm_sntr', 'wp_typ', 
                  'well_latr_prox', 'well_latr_uphll', 'well_plltn_prox', 'well_drn_pndng', 'well_drn_brkn', 'well_fnc_brk', 'well_aprn_brk', 
                  'well_aprn_cllct', 'well_flr_crcks', 'well_hnd_pmp_lse', 'well_cvr_unsan', 'well_cvr_unsan_na', 'well_san_insp_cmmnt', 
                  'sprng_unprtctd', 'sprng_unprtctd_na', 'sprng_msnry_brk', 'sprng_bckfll_brk', 'sprng_flood', 'sprng_fnc_brk', 'sprng_anmls', 
                  'sprng_ltrn_uphll', 'sprng_srfc_cllct', 'sprng_dtch_brk', 'sprng_plltn_prox', 'sprng_san_insp_cmmnt', 
                  'brhl_latrn_prox', 'brhl_latrn_uphll', 'brhl_plltn_prox', 'brhl_drn_brk_1', 'brhl_drn_brk_2', 'brhl_fnc_brk', 'brhl_aprn_brk', 
                  'brhl_aprn_cllct', 'brhl_flr_crck', 'brhl_hnd_pmp_lse', 'brhl_san_insp_cmmnt', 
                  'tst_rslt_ph', 'tst_rslt_elec_amt', 'tst_rslt_elec_unts', 'tst_rslt_temp_amt', 'tst_rslt_temp_unts', 'tst_rslt_trbd_amt', 'tst_rslt_trbd_unts', 
                  'tst_rslt_nitrite_amt', 'tst_rslt_nitrite_unts', 'tst_rslt_nitrate_amt', 'tst_rslt_nitrate_unts', 'tst_rslt_fl_amt', 'tst_rslt_fl_unts', 
                  'tst_rslt_arsnc_amt', 'tst_rslt_arsnc_unts', 'tst_rslt_ecoli_amt', 'tst_rslt_ecoli_unts', 'tst_rslt_fcoli_amt', 'tst_rslt_fcoli_unts')


####################################################################################################
# Aggregating the Water Quality Survey
# Assumes that all surveys are monitoring; averages values across surveys but takes the max submit date

for (i in c(54, 55, 57, 59, 61, 63, 65, 67, 69, 71)) {
  wq[,i] <- as.numeric(wq[,i])
}

wq$subm_dt <- as.Date(substr(wq$subm_dt,0,10), format = "%Y-%m-%d")
wq$functl_rcd <- ifelse(substr(wq$functl, 7, 8) == "fu", 1, ifelse(substr(wq$functl, 7, 8) == "pa", 0.5, 0))

wq1 <- sqldf("select 
             
             twt_id as twt_id_wq, 
             'm' as srvy_tp_wq,
             max(subm_dt) as subm_dt_wq,
             sum(functl_rcd) as fnct_wq,
             sum(case when functl_rcd is null then 1 else 0 end) as fnct_wq_na,
             count(functl_rcd) as fnct_wq_tt,
             sum(tst_rslt_elec_amt) as tst_rslt_elec_amt,
             sum(case when tst_rslt_elec_amt is null then 1 else 0 end) as tst_rslt_elec_amt_na,
             count(tst_rslt_elec_amt) as tst_rslt_elec_amt_tt,
             sum(tst_rslt_temp_amt) as tst_rslt_temp_amt,
             sum(case when tst_rslt_temp_amt is null then 1 else 0 end) as tst_rslt_temp_amt_na,
             count(tst_rslt_temp_amt) as tst_rslt_temp_amt_tt,
             sum(tst_rslt_trbd_amt) as tst_rslt_trbd_amt,
             sum(case when tst_rslt_trbd_amt is null then 1 else 0 end) as tst_rslt_trbd_amt_na,
             count(tst_rslt_trbd_amt) as tst_rslt_trbd_amt_tt,
             sum(tst_rslt_nitrite_amt) as tst_rslt_nitrite_amt,
             sum(case when tst_rslt_nitrite_amt is null then 1 else 0 end) as tst_rslt_nitrite_amt_na,
             count(tst_rslt_nitrite_amt) as tst_rslt_nitrite_amt_tt,
             sum(tst_rslt_nitrate_amt) as tst_rslt_nitrate_amt,
             sum(case when tst_rslt_nitrate_amt is null then 1 else 0 end) as tst_rslt_nitrate_amt_na,
             count(tst_rslt_nitrate_amt) as tst_rslt_nitrate_amt_tt,
             sum(tst_rslt_fl_amt) as tst_rslt_fl_amt,
             sum(case when tst_rslt_fl_amt is null then 1 else 0 end) as tst_rslt_fl_amt_na,
             count(tst_rslt_fl_amt) as tst_rslt_fl_amt_tt,
             sum(tst_rslt_arsnc_amt) as tst_rslt_arsnc_amt,
             sum(case when tst_rslt_arsnc_amt is null then 1 else 0 end) as tst_rslt_arsnc_amt_na,
             count(tst_rslt_arsnc_amt) as tst_rslt_arsnc_amt_tt,
             sum(tst_rslt_ecoli_amt) as tst_rslt_ecoli_amt,
             sum(case when tst_rslt_ecoli_amt is null then 1 else 0 end) as tst_rslt_ecoli_amt_na,
             count(tst_rslt_ecoli_amt) as tst_rslt_ecoli_amt_tt,
             sum(tst_rslt_fcoli_amt) as tst_rslt_fcoli_amt,
             sum(case when tst_rslt_fcoli_amt is null then 1 else 0 end) as tst_rslt_fcoli_amt_na,
             count(tst_rslt_fcoli_amt) as tst_rslt_fcoli_amt_tt
             
             from wq 
             
             where twt_id is not null
             
             group by 1, 2
             
             ")

wq1$ecoli_risk_l <- ifelse(wq1$tst_rslt_ecoli_amt <= 1, 1, 0)
wq1$ecoli_risk_i <- ifelse(wq1$tst_rslt_ecoli_amt > 1 & wq1$tst_rslt_ecoli_amt <= 10, 1, 0)
wq1$ecoli_risk_h <- ifelse(wq1$tst_rslt_ecoli_amt > 10 & wq1$tst_rslt_ecoli_amt <= 100, 1, 0)
wq1$ecoli_risk_v <- ifelse(wq1$tst_rslt_ecoli_amt > 100, 1, 0)

rm(wq)

####################################################################################################
# Aggregating the Water Point Survey
# Assumes that all surveys are monitoring; averages values across surveys but takes the max submit date
# Omits "Post" surveys

wp$subm_dt <- as.Date(substr(wp$subm_dt,0,10), format = "%Y-%m-%d")
wp$functl_rcd <- ifelse(substr(wp$functl, 7, 8) == "fu", 1, ifelse(substr(wp$functl, 7, 8) == "pa", 0.5, 0))

wp1 <- sqldf("select 
             
             twt_id as twt_id_wp, 
             'm' as srvy_tp_wp,
             max(subm_dt) as subm_dt_wp,
             sum(functl_rcd) as fnct_wp,
             sum(case when functl_rcd is null then 1 else 0 end) as fnct_wp_na,
             count(functl_rcd) as fnct_wp_tt,
             sum(wuc_comm_fees_strd_amt) as strd_fees_sum
             
             from wp
             
             where twt_id is not null
             and srvy_tp not in ('Post')
             
             group by 1, 2
             
             ")

wp1[is.na(wp1$strd_fees_sum) == T, 7] <- 0
wp1$strd_fees_none <- ifelse(wp1$strd_fees_sum == 0, 1, 0)

rm(wp)

####################################################################################################
# Aggregating the Household Survey
# Takes max dates for both survey types
# Omits Post surveys

hh$subm_dt <- as.Date(substr(hh$subm_dt,0,10), format = "%Y-%m-%d")
hh$srvy_tp2 <- ifelse(substr(hh$srvy_tp,0,1) == "B", "b", ifelse(substr(hh$srvy_tp,0,1) == "M", "m", "p"))
hh <- hh[hh$srvy_tp2!="p",]

# Recoding key binaries - only run once (to avoid dupes!) Maybe I'll fix later
for (i in c(19, 20, 21, 22, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69)) {
  hh$dummy <- ifelse(hh[,i] == "True", 1, ifelse(hh[,i] == "False", 0, NA))
  colnames(hh)[length(names(hh))] <- paste0(colnames(hh)[i], "_rcd")
  }

# Creating Dummies for some other key vars
# Must identify these variables in "names" and manually recode factor levels

hh$ftch_tm <- mapvalues(hh$ftch_tm, 
                        from = c("Less than 30 minutes", "30 - 60 minutes", "More than 2 hours",
                                 "More than 1 hour", "More than 3 hours", ""), 
                        to = c("ft_lt_30M", "ft_30_60M", "ft_gt_120M", "ft_gt_60M", "ft_gt_180M", ""))

hh$san_fac <- mapvalues(hh$san_fac, 
                        from = c("No facility - use the bush or field",
                                 "Improved latrine (with slab)",
                                 "Open pit",
                                 "Latrine",
                                 "No facility - use a neighbor's",
                                 ""), 
                        to = c("sf_NoBush", 
                               "sf_ImpLatr", 
                               "sf_Pit", 
                               "sf_Latr", 
                               "sf_NoNghbr", 
                               ""))

names <- c("hh$ftch_tm", "hh$san_fac")

for (k in 1:length(names)) {

hh_d <- data.frame(eval(parse(text = names[k])))

for (i in 1:length(unique(hh_d[,1]))) {
  hh_d[,i+1] <- ifelse(hh_d[,1] == unique(hh_d[,1])[i], 1, 0)
  colnames(hh_d)[i+1] <- as.character(unique(hh_d[,1])[i])
  colnames(hh_d)[i+1] <- ifelse(colnames(hh_d)[i+1] == "", 
                                paste0(substr(names[k], 4, 50),"_NA"), 
                                colnames(hh_d)[i+1])
  }

hh <- cbind(hh, hh_d[,c(2:length(colnames(hh_d)))])
colnames(hh_d[,7])
}

hh1_amt  <-  aggregate(hh[,c(79:105)], 
                       by = list(hh$twt_id, hh$srvy_tp2), 
                       FUN = sum, 
                       na.rm = TRUE)
colnames(hh1_amt)[1:2] <- c("twt_id", "srvy_tp")

hh$wp_prblm_taste_rcd_na <- ifelse(is.na(hh$wp_prblm_taste_rcd) == "TRUE", 1, 0)
hh$wp_prblm_smell_rcd_na <- ifelse(is.na(hh$wp_prblm_smell_rcd) == "TRUE", 1, 0)
hh$wp_prblm_color_rcd_na <- ifelse(is.na(hh$wp_prblm_color_rcd) == "TRUE", 1, 0)
hh$wp_prblm_cloudy_rcd_na <- ifelse(is.na(hh$wp_prblm_cloudy_rcd) == "TRUE", 1, 0)
hh$ho_diar_rcd_na <- ifelse(is.na(hh$ho_diar_rcd) == "TRUE", 1, 0)
hh$ho_typh_rcd_na <- ifelse(is.na(hh$ho_typh_rcd) == "TRUE", 1, 0)
hh$ho_malr_rcd_na <- ifelse(is.na(hh$ho_malr_rcd) == "TRUE", 1, 0)
hh$ho_dysn_rcd_na <- ifelse(is.na(hh$ho_dysn_rcd) == "TRUE", 1, 0)
hh$ho_resp_rcd_na <- ifelse(is.na(hh$ho_resp_rcd) == "TRUE", 1, 0)
hh$ho_skin_rcd_na <- ifelse(is.na(hh$ho_skin_rcd) == "TRUE", 1, 0)
hh$ho_eyin_rcd_na <- ifelse(is.na(hh$ho_eyin_rcd) == "TRUE", 1, 0)
hh$ho_wrms_rcd_na <- ifelse(is.na(hh$ho_wrms_rcd) == "TRUE", 1, 0)
hh$ho_othr_1_rcd_na <- ifelse(is.na(hh$ho_othr_1_rcd) == "TRUE", 1, 0)
hh$ho_othr_2_rcd_na <- ifelse(is.na(hh$ho_othr_2_rcd) == "TRUE", 1, 0)
hh$ho_dk_rcd_na <- ifelse(is.na(hh$ho_dk_rcd) == "TRUE", 1, 0)

hh1_na <- aggregate(hh[,c(106:120)], 
                    by = list(hh$twt_id, hh$srvy_tp2), 
                    FUN = sum, 
                    na.rm = TRUE)
colnames(hh1_na)[1:2] <- c("twt_id", "srvy_tp")

hh1_hhs <- aggregate(hh[,c(10)], 
                    by = list(hh$twt_id, hh$srvy_tp2), 
                    FUN = length)
colnames(hh1_hhs)[1:3] <- c("twt_id", "srvy_tp", "hhs")

hh1_ppl <- aggregate(hh[,c(11)], 
                     by = list(hh$twt_id, hh$srvy_tp2), 
                     FUN = sum, 
                     na.rm = TRUE)
colnames(hh1_ppl)[1:3] <- c("twt_id", "srvy_tp", "ppl")

hh1_dt <- aggregate(hh[,c(6)],
                    by = list(hh$twt_id, hh$srvy_tp2), 
                    FUN = max, 
                    na.rm = TRUE)
colnames(hh1_dt)[1:3] <- c("twt_id", "srvy_tp", "subm_dt")

# Rolling Merge

hh1 <- merge(hh1_hhs, 
             hh1_ppl, 
             by = c("twt_id", "srvy_tp"))

hh1 <- merge(hh1, 
             hh1_dt, 
             by = c("twt_id", "srvy_tp"))

hh1 <- merge(hh1, 
             hh1_amt, 
             by = c("twt_id", "srvy_tp"))

hh1 <- merge(hh1, 
             hh1_na, 
             by = c("twt_id", "srvy_tp"))

rm(hh, i, hh1_amt, hh1_na, hh1_hhs, hh1_ppl, hh1_dt, hh_d)

####################################################################################################
# Determining which sites have both surveys and which have baseline since Jan 1 2015

b <- data.frame(hh1[hh1$srvy_tp == "b", c(1, 5)], b = 1)
m <- data.frame(hh1[hh1$srvy_tp == "m", c(1, 5)], m = 1)
colnames(b) <- c("twt_id_bm", "subm_dt_b", "b")
colnames(m) <- c("twt_id_bm", "subm_dt_m", "m")

bm <- merge(b, m, by = "twt_id_bm", all = T)
bm$both <- ifelse(bm$b + bm$m == 2, 1, 0)
bm$new <- ifelse(bm$subm_dt_b >= as.Date("2015-01-01", format = "%Y-%m-%d"), 1, 0)

bm[is.na(bm$both),6] <- 0
bm[is.na(bm$new),7] <- 0

bm$newboth <- paste0(as.numeric(bm$new), as.numeric(bm$both))

rm(b, m)

####################################################################################################
# Merging togther
# site_agg is final product

site_agg <- sqldf("
                  select 
                  
                  a.*, b.*, c.*, d.both, d.new, d.newboth
                  
                  from hh1 a
                  left join wp1 b on a.twt_id = b.twt_id_wp and a.srvy_tp = b.srvy_tp_wp
                  left join wq1 c on a.twt_id = c.twt_id_wq and a.srvy_tp = c.srvy_tp_wq
                  left join bm d on a.twt_id = d.twt_id_bm
                  
                  ")

site_agg <- subset(site_agg, select=-c(twt_id_wq, twt_id_wp, 
                                       srvy_tp_wq, srvy_tp_wp, 
                                       subm_dt_wq, subm_dt_wp))

site_agg$st_ct <- as.numeric(1)
site_agg <- cbind(site_agg[, c("twt_id", "srvy_tp", "subm_dt", "both", "new", "newboth", "st_ct")],
                  site_agg[, !names(site_agg) %in% c("twt_id", "srvy_tp", "subm_dt", "both", "new", "newboth", "st_ct")])

# Recoding some vars (dates, factors, etc.)

site_agg$subm_dt <- as.Date(site_agg$subm_dt, origin = "1970-01-01")

for (i in c(which(names(site_agg) %in% c("twt_id", "srvy_tp", "both", "new", "newboth")))) {
  site_agg[,i] <- factor(site_agg[,i])
}

for (i in 7:length(names(site_agg))) {
  site_agg[,i] <- as.numeric(site_agg[,i])
}

rm(hh1, wp1, wq1, i, bm, k, names)

write.csv(site_agg, "site_agg.csv", row.names = F)

