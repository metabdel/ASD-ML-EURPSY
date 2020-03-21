library(data.table)
library(magrittr)
library(tidyr)
library(stringr)
library(plyr)
library(dplyr)
library(caret)
library(DMwR)
setwd("D:\\Autism/")
setwd("/media/rrahman/Seagate Backup Plus Drive/Autism/")

all_data = fread(file ="11.19.18/parent_demo_asd_med_combined.csv", sep ="," , header = T) 
feats = c("FID", "MID", "ses", "missing_p", "missing_m" , "pagediff" , 
          "m_current_age",  "p_current_age"  , "count_unique_drugs.x", "count_unique_drugs.y" ,"asd_status" )
all_data_demo = all_data[, ..feats]
all_data_demo$count_drugs_scaled.x =  all_data_demo$count_unique_drugs.x / all_data_demo$m_current_age
all_data_demo$count_drugs_scaled.y =  all_data_demo$count_unique_drugs.y / all_data_demo$p_current_age
all_data_demo$m_current_age = NULL
all_data_demo$p_current_age = NULL
atc_cols = which(!names(all_data) %in% feats)
all_data_atc = all_data[, ..atc_cols]
cols = colnames(all_data_atc)
#col.feat = which(cols %in% feats) 
cols.level1  = c()
cols.level2  = c()
cols.level3  = c()
cols.level4  = c()
cols.level5  = c()
for (i in 1:length(cols))
{
    if ( i %in%  which(cols %in% feats))
    {
        next 
    }
    charlen = nchar(cols[i])
    if (charlen == 3 )
    {
        cols.level1 = c(cols.level1,i)
    }
    else if ( charlen == 5)
    {
        cols.level2  = c(cols.level2,i)
    }
    else if ( charlen == 6)
    {
        cols.level3 = c(cols.level3,i)
    }
    else if ( charlen == 7)
    {
        cols.level4 = c(cols.level4,i)
    }
    else if ( charlen > 7)
    {
        cols.level5 = c(cols.level5,i)
    }
}

all_data_atc_sel = all_data_atc[, ..cols.level2]
all_data_atc_sel$B05A.y = NULL
all_data_atc_sel = cbind(all_data_demo, all_data_atc_sel)
midfid = all_data_atc_sel[,c(1,2)]
cat_feat = all_data_atc_sel[,c(3,4,5,7,8,9)]
all_data_atc_sel[,c(1,2,3,4,5,7,8,9)] = NULL
all_data_atc_sel.scaled = scale(all_data_atc_sel) %>%  as.data.frame()
all_data_atc_sel.scaled = SoftMax(all_data_atc_sel) %>%  as.data.frame()
all_data_atc_scaled_com = cbind(cat_feat , all_data_atc_sel.scaled )
all_data_atc_scaled_com$count_unique_drugs.x = NULL
all_data_atc_scaled_com$count_unique_drugs.y = NULL

write.table(all_data_atc_scaled_com, f = "all_data_selected_atc.level2.softmax.csv", 
            sep = ",", col.names = T, quote = F, eol = "\n", row.names = F, fileEncoding = "UTF8")

#all_dat_cor = cor(all_data_atc_sel.scaled)
#ncol(all_data_atc_sel.scaled)
#hc = findCorrelation(all_dat_cor, cutoff=0.9)
#hc = sort(hc)
#all_data_atc_sel.scaled = all_data_atc_sel.scaled[,-c(hc)]
#ncol(all_data_atc_sel.scaled)