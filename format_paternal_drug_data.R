library(data.table)
library(magrittr)
library(tidyr)
library(stringr)
library(plyr)
library(ggplot2)

setwd("/mnt/d/Autism/")
source("Autism_project_R/uniqify_atc_codes.R")
###paternal 
asd.p = fread("research/Table7.csv.3", header = F, encoding = "UTF-8", 
              na.strings = c(".U", "NULL"))              
un.p = fread("control/Table7.csv.2", header = F, encoding =  "UTF-8",
             na.strings = c(".U", "NULL"))
data.pat = rbind(un.p, asd.p)
colnames(data.pat) <- c("As", "FID", "DATEMP", "DATERP", 
                              "DRUG_NAME", "PILLS", "BOXES", "ATC5_Code",
                              "DDDcode", "PillsPaid")
data.pat.no.na = data.pat[! is.na(data.pat$ATC5_Code) ,]

###maternal 
asd.m = fread("research/Table6.csv.3", header = F, encoding = "UTF-8", 
              na.strings = c(".U", "NULL"))
un.m = fread("control/Table6.csv.2", header = F, encoding =  "UTF-8",
             na.strings = c(".U", "NULL"))
data.mat = rbind(un.m, asd.m)
colnames(data.mat) <- c("As", "MID", "DATEMP", "DATERP", 
                        "DRUG_NAME", "PILLS", "BOXES", "ATC5_Code",
                        "DDDcode", "PillsPaid")
data.mat.no.na = data.mat[! is.na(data.mat$ATC5_Code) ,]


###demographic information 
asd.demo = fread("research/Table2.csv", header = F, encoding = "UTF-8", 
              na.strings = c(".U", "NULL"))
un.demo = fread("control/Table2.csv", header = F, encoding =  "UTF-8",
             na.strings = c(".U", "NULL"))
asd.demo.c = asd.demo[V3=="CID", c(2,3,4,5)]
asd.demo.c$ASD = rep(1,nrow(asd.demo.c))
un.demo.c <- un.demo[V3 == "CID", c(2, 3, 4, 5)]
un.demo.c$ASD = rep(0,nrow(un.demo.c))

data.demo = rbind(un.demo.c, asd.demo.c)

colnames(data.demo) <- c("CID", "IDTYPE", "CDOB", 
                        "SIBPAIRID", "ASD" )
data.demo$IDTYPE = NULL
data.demo$SIBPAIRID = NULL

###parent child relationships
asd.rel = fread("research/Table3.csv", header = F, encoding = "UTF-8", 
              na.strings = c(".U", "NULL"))
un.rel = fread("control/Table3.csv", header = F, encoding =  "UTF-8",
             na.strings = c(".U", "NULL"))
data.rel = rbind(un.rel, asd.rel)

colnames(data.rel) <- c("As", "MID", "FID", "SID", 
                        "CID", "SIBPAIRID", "CITYL")                        

###merge data 
data.mg = merge(data.rel, data.demo, by = "CID")
##remove duplicated entries 
data.mg.single = data.mg[ ! duplicated(data.mg[,c(1,3,4)]),c(1,3,4,6,8,9) ] 

# ##remove missing parental infomration 
# data.mg.single.complete.cases =data.mg.single[complete.cases(data.mg.single), ]
data.mg.single.complete.cases = data.mg.single[! is.na(data.mg.single$MID), ]

##get siblings with ASD 
asd.siblings =  data.mg.single.complete.cases[ASD == 1]$SIBPAIRID

###data with parental information 
info.asd = fread("research/Table1.csv", header = F, encoding = "UTF-8", 
              na.strings = c(".U", "NULL"))
info.non = fread("control/Table1.csv", header = F, encoding = "UTF-8", 
              na.strings = c(".U", "NULL"))
info.combined = rbind(info.asd, info.non)

names(info.combined) = c("INDEX","ID_CODE", "TYPE", "SIBPAIRD" ,"Sex" , "BIRTH_DATY" ,"BIRTH_DATM",
                "BIRTH_DATD", "DEATH_DATY" , "DEATH_DATM" , "DEATH_DATD" , "SES", "CITYL")
info.combined = info.combined[! duplicated(info.combined) ] 
info.combined$DOB = paste(info.combined$BIRTH_DATY,info.combined$BIRTH_DATM , info.combined$BIRTH_DATD , sep ="-" ) %>% as.Date
info.mid = info.combined[TYPE == "MID"]
info.fid = info.combined[TYPE == "FID"]
data.mg.single.complete.cases  = merge(data.mg.single.complete.cases , info.mid[,c(2,14)], by.x = "MID", by.y = "ID_CODE")
data.mg.single.complete.cases  = merge(data.mg.single.complete.cases , info.fid[,c(2,14)], by.x = "FID", by.y = "ID_CODE")
data.mg.single.complete.cases = data.mg.single.complete.cases[! duplicated(data.mg.single.complete.cases[,c("FID", "MID", "DOB.x", "DOB.y")])] 
diff.days = abs(data.mg.single.complete.cases$DOB.x -  data.mg.single.complete.cases$DOB.y)
diff.years = as.numeric(diff.days / 365  )
data.mg.single.complete.cases$age_diff = diff.years 

###age_diff dist 
ggplot(data.mg.single.complete.cases, aes( 
  x = factor(ASD),  y = age_diff 
)) + geom_boxplot() + 
theme_bw( )
ggplotly(p)
wilcox.test(data.mg.single.complete.cases[ASD==1]$age_diff, data.mg.single.complete.cases[ASD==0]$age_diff)

##separate ASD/NON cases 
data.mg.asd.children = data.mg.single.complete.cases[ASD == 1, ]
data.mg.non.children = data.mg.single.complete.cases[ASD == 0, ]

##get drugs from parent histories before child's birth 

store_atc_level_data = function(df, type = "MID", level = "level2")
{
  atc_ = df$ATC5_Code %>% unique 
  id = df[,..type] %>% unique %>% as.numeric 
  ###catches all atc codes that have mutiple listings 
  atcfull = atc.linked[atc_] %>%  unlist %>%  as.vector() %>%  unique 
  atclevels = data.frame(V1 =c(), N =c(), freq = c(), type =c(), id = c())
  if( is.null(atcfull[[1]]))
  {
    return(atclevels)
  }
  level1 = as.data.table(table(str_sub(atcfull, start = 1 , end = 1))) 
  level2 = as.data.table(table(str_sub(atcfull, start = 1 , end = 3)))
  level3 = as.data.table(table(str_sub(atcfull, start = 1 , end = 4)))
  level4 = as.data.table(table(str_sub(atcfull, start = 1 , end = 5)))
  level1$freq = level1$N / sum(level1$N)
  level2$freq = level2$N / sum(level2$N)
  level3$freq = level3$N / sum(level3$N)
  level4$freq = level4$N / sum(level4$N)
  level1$type = rep("level1", nrow(level1))
  level2$type = rep("level2", nrow(level2))
  level3$type = rep("level3", nrow(level3))
  level4$type = rep("level4", nrow(level4))
  atclevels = rbind(level1, level2, level3, level4)
  atclevels$id = rep(id, nrow(atclevels))
  if (level == "all")
  {
    atclevels = atclevels
  }  else  {
    atclevels = atclevels[type == level, ]
  }
  return(atclevels)
}

get_drugs = function(df, type = "MID", merged , before=FALSE, atclevel = "level2")
{
  df.sp = split(df, f = df[,..type])  
  atc.total = data.frame()
  for ( id.df in df.sp )
  {
    ###fix date 
    #print(paste("working on", type, as.numeric(unique(id.df[,..type])), sep = " "))
    years = sapply(id.df$DATEMP, function(x) { regmatches(x, gregexpr(".{4}", x))[[1]][1]} ) 
    mon = sapply(id.df$DATEMP, function(x) { regmatches(x, gregexpr(".{2}", x))[[1]][3]} ) 
    day = sapply(id.df$DATEMP, function(x) { regmatches(x, gregexpr(".{2}", x))[[1]][4]} ) 
    id.df$DATEMP = as.Date(paste(years, mon, day,sep= "-"))
    id = data.frame()
    if ( before == TRUE )
    {
      cdob = ""
      if ( type == "MID")
      {
        
        cdob = merged[MID == as.numeric(unique(id.df[,..type])),CDOB]   %>% as.Date
      } else {

        cdob = merged[FID == as.numeric(unique(id.df[,..type])),CDOB]   %>% as.Date
      }
      id = id.df[DATEMP <=  cdob, ]
      if ( empty(id))
      {
        next 
      }
    } else {
      id = id.df 
    }
    atc = data.frame()
    if ( type == "MID")
    {
      atc = store_atc_level_data(id, level = atclevel  )
    } else {
      atc = store_atc_level_data(id, type = "FID", level = atclevel)
    }
    atc.total = rbind(atc, atc.total) %>% as.data.table
  }
  return(atc.total)
}

get_atc_from_data = function(pat, mat, merged, before_birth = FALSE, atclevel = "level2" )
{
  ###internal check to remove siblings (take older sibling)
  sibpid = merged[duplicated(merged[,c(2,3)]),]$SIBPAIRID
  merged.remove.dup.sib = merged[! SIBPAIRID %in% sibpid]
  for ( id in sibpid )
  {
    dat = merged[SIBPAIRID == id ]
    dat = dat[CDOB == max(as.Date(dat$CDOB)),] 
    merged.remove.dup.sib = rbind(merged.remove.dup.sib, dat) %>% as.data.table
  }
  mat = mat[MID %in% merged.remove.dup.sib$MID]
  pat = pat[FID %in% merged.remove.dup.sib$FID]
  mat.atc.count  = get_drugs(mat, type = "MID", merged , before = before_birth, atclevel  )
  pat.atc.count  = get_drugs(pat, type = "FID", merged , before = before_birth , atclevel )
  data = list()
  data$mat = mat.atc.count
  data$pat = pat.atc.count 
  return(data)
}

###collect presense of atleast 1 occurance of each drug per supplied ID (for fischer's exact)
###for both mom and dad 
count_unique_atc_sample = function(df) 
{
  df.sp = split(df, by = 'id')
  atc = c()
  for (i in df.sp)
  {
    atc = c(atc, unique(i$V1 )) 
  }
  atc.df = table(atc) %>% as.data.table
  atc.df$total = rep(length(df.sp), nrow(atc.df))
  atc.df$un = atc.df$total - atc.df$N 
  return(atc.df)
}


####get all med history from parents of ASD children 
asd.parent.hist.all <- get_atc_from_data(
  pat = data.pat.no.na, mat = data.mat.no.na,
  merged = data.mg.asd.children, before_birth = FALSE,
  atclevel = "level2"
)

asd.parent.hist.before <- get_atc_from_data(
  pat = data.pat.no.na, mat = data.mat.no.na,
  merged = data.mg.asd.children, before_birth = TRUE,
  atclevel = "level2"
)


####get non asd med history (bootstrap 10x, ~1000 samples )
non.parent.hist.all.bootstrap.mat = list()
non.parent.hist.all.bootstrap.pat = list()

for (n in 1:10) 
{
  print(paste("bootstrap number " , n, sep = ""))
  data.mg.non.children.no.asd.sib = data.mg.non.children[! SIBPAIRID %in%  asd.siblings ]
  datasample.merged = dplyr::sample_n(data.mg.non.children.no.asd.sib, 15000) %>% as.data.table
  non.parent.hist.all = get_atc_from_data(pat = data.pat.no.na , mat = data.mat.no.na , 
                                          merged = datasample.merged , before_birth = FALSE, 
                                          atclevel = "level2")
  non.parent.hist.all.bootstrap.mat[[n]] = non.parent.hist.all$mat
  non.parent.hist.all.bootstrap.pat[[n]] = non.parent.hist.all$pat
}

saveRDS( non.parent.hist.all.bootstrap.mat, "non.parent.hist.all.bootstrap.mat.rdata")
saveRDS(non.parent.hist.all.bootstrap.pat, "non.parent.hist.all.bootstrap.pat.rdata" )

#non.parent.hist.all = get_atc_from_data(pat = data.pat.no.na , mat = data.mat.no.na , 
#                                          merged = data.mg.non.children.no.asd.sib , before_birth = FALSE, 
#                                          atclevel = "level2")

###get unique atc counts 
mat.cnt.atc.asd =  count_unique_atc_sample(asd.parent.hist.all$mat)
mat.cnt.atc.non =  count_unique_atc_sample(non.parent.hist.all$mat)
pat.cnt.atc.asd =  count_unique_atc_sample(asd.parent.hist.all$pat)
pat.cnt.atc.non =  count_unique_atc_sample(non.parent.hist.all$pat)


###compute 1 tailed chi^2/fishers for all drugs 
calc_enrichment = function(test, background, alt = "greater")
{
 common_atc = test$atc[which(test$atc %in% background$atc) ] 
 return_df = data.table()
 for ( i in common_atc)
  {
    t = test[atc == i ]
    perc_use_test = t$N/t$total 
    bg = background[atc == i ]
    perc_use_bg = bg$N/bg$total 
    mat = cbind(c(t$N, bg$N), c(t$un, bg$un)) 
    chisq = chisq.test(mat)
    fish = fisher.test(mat)
    fish.2 = fisher.test(mat, alternative = "greater")
    line = c( i, t$N, bg$N , t$un, bg$un, 
              chisq$p.value , fish$p.value, fish.2$p.value,  
              fish$estimate,  fish$conf.int[1][1] , fish$conf.int[2][1] , 
              perc_use_test , perc_use_bg ) %>% as.data.table %>% t %>% as.data.table
    names(line) = c("atc", "test_cnt", "bg_cnt", "test_un", "bg_un",
                    "x^2_p", "fishers_p", "fishers_p_alt_>", "odds_ratio", 
                    "lower_conf", "upper_conf", "percent_use_test" , "percent_use_background")
    return_df = rbind(return_df, line) %>% as.data.table
  }
  return(return_df)
} 

paternal_enrichment = calc_enrichment(pat.cnt.atc.asd, pat.cnt.atc.non)
maternal_enrichment = calc_enrichment(mat.cnt.atc.asd, mat.cnt.atc.non)
write.table(paternal_enrichment, "paternal_enrichment.full.csv", sep = ",", col.names = T,
          row.names = F, quote = F )

write.table(maternal_enrichment, "maternal_enrichment.full.csv", sep = ",", col.names = T,
          row.names = F, quote = F )


pat_atc = c("N06", "A06", "A11", "G04", "G03", "B01", "B05", "B02", "D09", "A12", "D04")
mat_atc = c("L02", "V03", "P02", "A03", "A08")
paternal_enrichment.sel = paternal_enrichment[atc %in% pat_atc]
maternal_enrichment.sel = maternal_enrichment[atc %in% mat_atc]

###write enrichment tables 
write.table(paternal_enrichment.sel, "paternal_enrichment.csv", sep = ",", col.names = T,
          row.names = F, quote = F )

write.table(maternal_enrichment.sel, "maternal_enrichment.csv", sep = ",", col.names = T,
          row.names = F, quote = F )


d[order(odds_ratio)]
###average all bootstraps anbd combine into a single df 
library(plyr)
load("non.parent.hist.all.bootstrap.pat.rdata")
load("non.parent.hist.all.bootstrap.mat.rdata")

non.parent.hist.all.bootstrap.mat.avg = lapply(non.parent.hist.all.bootstrap.mat, function (x) {
  df = aggregate(x[, 2:3], list(x$V1), mean)
  names(df) = c("V1", "N" , "freq")
  df
} )  
non.parent.hist.all.bootstrap.pat.avg = lapply(non.parent.hist.all.bootstrap.pat, function (x) {
  df =aggregate(x[, 2:3], list(x$V1), mean)
  names(df) = c("V1", "N" , "freq")
  df
} )  

non.parent.hist.all.bootstrap.mat.avg.combined = ldply(non.parent.hist.all.bootstrap.mat.avg) 
non.parent.hist.all.bootstrap.pat.avg.combined = ldply(non.parent.hist.all.bootstrap.pat.avg) 

asd.parent.hist.all$mat$class = rep("ASD", nrow(asd.parent.hist.all$mat))
asd.parent.hist.all$pat$class = rep("ASD", nrow(asd.parent.hist.all$pat))
non.parent.hist.all.bootstrap.mat.avg.combined$class = rep("NON", nrow(non.parent.hist.all.bootstrap.mat.avg.combined))
non.parent.hist.all.bootstrap.pat.avg.combined$class = rep("NON", nrow(non.parent.hist.all.bootstrap.pat.avg.combined))

combined.m = rbind(asd.parent.hist.all$mat[,c(1:3,6)] ,non.parent.hist.all.bootstrap.mat.avg.combined)
combined.p = rbind(asd.parent.hist.all$pat[,c(1:3,6)] ,non.parent.hist.all.bootstrap.pat.avg.combined )
library(ggplot2)

ggplot(subset(combined.p, V1 %in% data, aes( factor(class), freq)) + geom_boxplot() + facet_wrap(~V1,  scales = "free" )

ggplot(combined.p, aes( factor(class), freq)) + 
  geom_boxplot() + facet_wrap(~V1,  scales = "free" )
d###count atc at distinct levels 

###count atc at distinct levels 


##remove children without any maternal information 
data.rel = 

###get unique drugs per parent 
data.pat.atc = data.pat.no.na[,c("FID", "ATC5_Code")] %>%  unique()
data.pat.atc$seen = rep(1, nrow(data.pat.atc))
data.pat.atc.asd = data.pat.atc[data.pat.atc$FID %in% data.mg.asd.children$FID]
data.pat.atc.non = data.pat.atc[! data.pat.atc$FID %in% data.mg.asd.children$FID]
data.pat.asd.sp = split(data.pat.atc.asd, f = data.pat.atc.asd$FID)
data.pat.non.sp = split(data.pat.atc.non, f = data.pat.atc.non$FID)

data.mat.atc = data.mat.no.na[,c("MID", "ATC5_Code")] %>%  unique()
data.mat.atc$seen = rep(1, nrow(data.mat.atc))
data.mat.atc.asd = data.mat.atc[data.mat.atc$MID %in% data.mg.asd.children$MID]
data.mat.atc.non = data.mat.atc[! data.mat.atc$MID %in% data.mg.asd.children$MID]
data.mat.asd.sp = split(data.mat.atc.asd, f = data.mat.atc.asd$MID)
data.mat.non.sp = split(data.mat.atc.non, f = data.mat.atc.non$MID)

###histogram of unique drugs taken by parents  
pat.asd.cnt = sapply(data.pat.asd.sp ,FUN = function(x){
  sum(x$seen)
}) %>% as.vector %>% as.character %>% as.numeric

df1 = cbind(pat.asd.cnt, rep("Paternal", length(pat.asd.cnt)), rep("ASD", length(pat.asd.cnt)) ) %>%
  as.data.frame

pat.non.cnt = sapply(data.pat.non.sp ,FUN = function(x){
  sum(x$seen)
}) %>% as.vector %>% as.character %>% as.numeric
df2 = cbind(pat.non.cnt, rep("Paternal", length(pat.non.cnt)), rep("NON", length(pat.non.cnt)) ) %>%
  as.data.frame

mat.asd.cnt = sapply(data.mat.asd.sp ,FUN = function(x){
  sum(x$seen)
}) %>% as.vector %>% as.character %>% as.numeric
df3 = cbind(mat.asd.cnt, rep("Maternal", length(mat.asd.cnt)), rep("ASD", length(mat.asd.cnt)) ) %>%
  as.data.frame

mat.non.cnt = sapply(data.mat.non.sp ,FUN = function(x){
  sum(x$seen)
}) %>% as.vector %>% as.character %>% as.numeric
df4 = cbind(mat.non.cnt, rep("Maternal", length(mat.non.cnt)), rep("NON", length(mat.non.cnt)) ) %>%
  as.data.frame
names(df1) = c("count", "type" , "class")
names(df2) = c("count", "type" , "class")
names(df3) = c("count", "type" , "class")
names(df4) = c("count", "type" , "class")
df.combined = rbind(df1, df2, df3, df4)
library(ggplot2)
p = ggplot(df.combined, aes( factor(class), as.numeric(as.character(count)), )) + 
  geom_boxplot() + theme_bw() + 
  facet_wrap(~type)
ggplotly(p)

wilcox.test(pat.asd.cnt, pat.non.cnt)
wilcox.test(mat.asd.cnt, mat.non.cnt)

###get proprotion of atc class per parent 
store_atc_level_data = function(df)
{
  atc_ = df$ATC5_Code %>% unique 
  ###catches all atc codes that have mutiple listings 
  atcfull = atc.linked[atc_] %>%  unlist %>%  as.vector() %>%  unique 
  level1 = as.data.table(table(str_sub(atcfull, start = 1 , end = 1))) 
  level2 = as.data.table(table(str_sub(atcfull, start = 1 , end = 3)))
  level3 = as.data.table(table(str_sub(atcfull, start = 1 , end = 4)))
  level4 = as.data.table(table(str_sub(atcfull, start = 1 , end = 5)))
  level1$freq = level1$N / sum(level1$N)
  level2$freq = level2$N / sum(level2$N)
  level3$freq = level3$N / sum(level3$N)
  level4$freq = level4$N / sum(level4$N)
  level1$type = rep("level1", nrow(level1))
  level2$type = rep("level2", nrow(level2))
  level3$type = rep("level3", nrow(level3))
  level4$type = rep("level4", nrow(level4))
  atclevels = rbind(level1, level2, level3, level4)
  atclevels$FID = rep(unique(df$FID), nrow(atclevels))
  atclevels$N = NULL
  atclevels.wide = spread(atclevels, key = "V1", value = "freq", fill = 0 )
  atclevels.wide$count_unique_drugs = length(atc_)
  return(atclevels.wide)
}
pat.atc.level = list()
for (i in 1:length(data.pat.sp))
{
  print(i)
  df = data.pat.sp[i] %>%  as.data.table()
  names(df) = c("FID", "ATC5_Code", "seen")
  pat.atc.level[[i]] =  store_atc_level_data(df)
}
mat.atc.level = list()
for (i in 1:length(data.mat.sp))
{
  print(i)
  df = data.mat.sp[i] %>%  as.data.table()
  names(df) = c("FID", "ATC5_Code", "seen")
  mat.atc.level[[i]] =  store_atc_level_data(df)
}
##flatten df list  
pat.atc.wide = pat.atc.level %>% rbind.fill() %>% as.data.table()
pat.atc.wide[is.na(pat.atc.wide)] = 0
mat.atc.wide = mat.atc.level %>% rbind.fill() %>% as.data.table()
mat.atc.wide[is.na(mat.atc.wide)] = 0
n = names(mat.atc.wide)
n[1] = "MID"
names(mat.atc.wide) = n 
##level 5 data wide format
data.mat.wide = spread(data.mat.atc, key = "ATC5_Code", value = "seen", fill = 0 )
data.pat.wide = spread(data.pat.atc, key = "ATC5_Code", value = "seen", fill = 0 )
##write out 
write.csv(data.pat.wide, file = "level5.pat.drug.freq.csv",  
          quote = F, eol = "\n", row.names = F, col.names = T, 
          fileEncoding = "UTF8", sep = "," )
write.csv(data.mat.wide, file = "level5.mat.drug.freq.csv", 
          quote = F, eol = "\n", row.names = F, col.names = T,
          fileEncoding = "UTF8", sep = "," )
write.csv(pat.atc.wide, file = "level1-4.pat.drug.freq.csv", 
          quote = F, eol = "\n", row.names = F, col.names = T,
          fileEncoding = "UTF8", sep = "," )
write.csv(mat.atc.wide, file = "level1-4.mat.drug.freq.csv", 
          quote = F, eol = "\n", row.names = F, col.names = T,
          fileEncoding = "UTF8", sep = "," )



merge(data.mg.asd.children, data.mat.no.na[,c("MID", "")] 
