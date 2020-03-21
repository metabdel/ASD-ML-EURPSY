library(data.table)
library(magrittr)
library(tidyr)
library(stringr)
library(plyr)
library(dplyr)
setwd("D:\\Autism/")
levels.mat = fread("11.19.18/level1-4.mat.drug.freq.csv", sep = "," , header = T)
levels.pat = fread("11.19.18/level1-4.pat.drug.freq.csv", sep = "," , header = T)
israeli = fread("11.19.18/israeli_data_utf8.relavent.col.csv", sep = ",", header = F)
names(israeli) =c("CID","FID","MID","child_sex", "ses", "mdob","pdob", "mage_concept", "page_concept", "diff", "asd", "cdoby")
israeli$mdoby = str_sub(israeli$mdob, start = 1 , end = 4)
israeli$pdoby = str_sub(israeli$pdob, start = 1 , end = 4)
israeli= israeli[! is.na(israeli$CID),]
israeli$missing_p = 0 
israeli$missing_m = 0 
israeli$diff = NULL
##take average age of parents 
israeli[is.na(israeli$FID), "missing_p"] = 1 
israeli[is.na(israeli$MID), "missing_m"] = 1 
avg_mat_dob = israeli[!is.na(israeli$mdob), ]$mdoby %>% as.numeric() %>% mean
avg_pat_dob = israeli[!is.na(israeli$pdob), ]$pdoby %>% as.numeric() %>% mean
israeli[is.na(israeli$mdob), ]$mdob =  avg_mat_dob 
israeli[is.na(israeli$pdob),  ]$pdoby =  avg_pat_dob 
israeli$pagediff = abs(as.numeric(israeli$pdoby) - as.numeric(israeli$mdoby))
israeli$m_current_age = 2008 - as.numeric(israeli$mdoby)
israeli$p_current_age = 2008 - as.numeric(israeli$pdoby)
###remove old people 
israeli = israeli[which(israeli$mdoby > "1950" ),]
###remove duplicated records
israeli  = israeli[! duplicated(israeli),] 
###check asd status of parents 
israeli$par_id = paste(israeli$FID, israeli$MID , sep =  "-") 
avg_ses = israeli[! is.na(israeli$ses), ]$ses %>%  as.numeric() %>%  mean %>%  round 
israeli[is.na(israeli$ses), ]$ses = avg_ses
is.sp = split(israeli, f= israeli$par_id)
check_asd_status = function(x)
{
  print(unique(x$par_id))
  rw = x[,c(2,3,5,14,15,16,17,18)] %>%  unique 
  rw$asd_status = 0 
  if ("ASD" %in% x$asd) {
    rw$asd_status = 1
  }
  return(rw)
}
par_demo_info = lapply(is.sp, function(x) { check_asd_status(x)} )  %>% rbind.fill() %>% as.data.table()
###merge with maternal data 
par_combined = merge(par_demo_info, levels.mat, by = "MID")
##for missing dads put median values of existing data 
par_combined.sp = split(par_combined, f = par_combined$missing_p)
par_combined.missing.p = par_combined.sp$`1` 
par_combined.complete.p = par_combined.sp$`0` 
par_combined.complete.p.2 = merge(par_combined.complete.p, levels.pat, by = "FID")
par_combined.missing.p$FID = rep("missing", nrow(par_combined.missing.p))
average_pat_values = apply(levels.pat, 2, function(x) {median(x)} ) %>%  as.vector() %>%  as.data.frame() %>%  t() %>%  as.data.table()
names(average_pat_values) = names(levels.pat)
average_pat_values$FID = "missing"
par_combined.missing.p.2 = merge(par_combined.missing.p, average_pat_values , by = "FID")
par_combined_all = rbind(par_combined.complete.p.2, par_combined.missing.p.2)
write.table(par_combined_all, file ="11.19.18/parent_demo_asd_med_combined.csv", 
            sep = ",", col.names = T, row.names = F, quote = F, eol = "\n", fileEncoding = "UTF8")








