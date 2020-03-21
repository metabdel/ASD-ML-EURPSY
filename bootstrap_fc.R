setwd("~/schlesslab/Autism/")
library(parallel)
source("Autism_project_R/format_data.R") 
source("Autism_project_R/sample_cases.R")
library(doParallel)
library(foreach)
library(ggplot2)
library(data.table)
for ( i in 1:10 )
{
    ff = generate_fc(asd.cases, unaffected.cases)
    bootstrapedfc = as.data.frame(rbind(bootstrapedfc, ff ))
}
ff = fread("bootstraped.1000.mothersdrug.csv", header = T,  sep = ",", verbose = T, stringsAsFactors = T , showProgress = T )
ff = ff[ff$fccnt != "Inf", ] 
ff = ff[ff$fccnt != "-Inf", ]

ff$dg = as.factor(as.character(ff$dg))
ff.sp = split(ff, f = ff$dg)
means = unlist(lapply(ff.sp, function(x) {round(mean(x$acnt) , digit  = 0 ) }))
means.gt.1 = means[means>1.5]
ff.top = ff[ff$dg %in% names(means.gt.1), ]
ggplot(ff.top, aes(x = reorder(dg, log(pvcnt) , FUN = median), y = -1*log(pvcnt) , color = "-log(pvalue)")) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_boxplot(aes(y =  fccnt , color = "fold change"), outlier.shape = NA) + 
  geom_hline(yintercept = 0, color = "blue", size = 1.5) + 
  coord_cartesian(xlim = c(0,50), ylim = c(0,11)) + 
  geom_hline(yintercept = -log(.05), size = 1.25, color = "red") +
  geom_text(data = data.frame(), aes(x = names(means.gt.1), y = 10, label = means.gt.1), 
            color = "black" , size = 4.5) +
  theme(axis.text.x = element_text(angle = 90, hjust =1 , vjust = 1, size = 25) , 
        legend.position = "none") 
min(unlist((lapply(ff.sp, FUN = nrow))))

ff.sp.2 = split(ff.top, f = as.factor(as.character(ff.top$dg)))
median.drugs = sort(unlist(lapply(ff.sp, FUN = function(x) { median(-log(x$pvcnt)) }  )), decreasing = T)
snames.top.100 = names(median.drugs[1:100])
write.table(names.top.100, "names.top.100.gt.1", quote = F, eol = "\n", row.names = F , col.names = F)
###in shell  cat names.top.100 | sed -E 's/(.+)!/!\1/gi'  
##| sed -E 's/(.).+/\1/gi' | sort | uniq -c | sed -E 's/^\s+//gi'  | tr " " "," > top.100.pie.csv
top100.pie =fread("top.100.pie.csv", header =F, sep = ",")
all.pie = fread("all.pie.csv", header =F, sep = ",")
top100.pie$type = rep("Top100",15)
all.pie$type = rep("all",15)
top100.pie$perc = top100.pie$V1 / sum(top100.pie$V1)
all.pie$perc = all.pie$V1 / sum(all.pie$V1)
cols = c("#6B82A8", "#FEB47B", "#FF7E5F", "#765285",
         "#351C4D", "#A1BBD0", "#E3BAB3", "#613A43",
         "#849974", "#36384C", "#037367",
         "#F26968", "#687672", "#6CBF84", "#323339")

combined = rbind(top100.pie, all.pie)
ggplot(combined, aes(x = type,  y = perc, fill = reorder(factor(V2), perc) )) + 
  geom_bar(position = "fill", stat = "identity", width = .5) + 
  scale_fill_manual(labels=c(
    "Various", "Antiparasitic products" , "	Musculo-skeletal system" , "Systemic hormonal preparations" ,
    "Dermatologicals" , "Sensory organs" , "Blood" , "Genito-urinary system and sex hormones" ,
    "Cardiovascular system" ,  "Alimentary tract and metabolism" ,   "Antineoplastic and immunomodulating agents" , 
    "Respiratory system" , "Antiinfectives" , "Nervous system" ,  "Multiple" 
  ), 
  values = cols) +
  geom_text(aes(label = paste0(round(perc*100, digits = 0),"%")), 
            position = position_stack(vjust = 0.5), size = 7.5, color = "white") 











