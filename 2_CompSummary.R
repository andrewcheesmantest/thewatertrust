####################################################################################################
# Produces a comprehensive summary from site_agg.csv 
# Begin:    Dec 9, 2016
# TODO:     Dec 9, 2016   -   TODO: create comprehensive summary on top of site_agg - simple aggregate metrics across B/M for each var
#                         -   TODO: maybe standardize format so that graphing is easy

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

































