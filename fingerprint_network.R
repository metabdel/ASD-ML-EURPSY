setwd("~/schlesslab/Autism/")
library(data.table)
finger = fread("fingerprint.fp.txt.2", sep = "\t", header = T, stringsAsFactors = F)
finger$ECFP4 = as.numeric(as.character(finger$ECFP4))
finger$wsum = finger$ECFP4*.4 + finger$DL*.4 + finger$MACCS*.2
df = as.data.frame(cbind(names(median.drugs),median.drugs))
names(df) = c("Ref", "Median")
df.m = merge(df,finger, by = "Ref")
names(df) = c("Que", "Median")
df.m$que.med = sapply(df.m$Que, FUN =  function(x) { 
  if (x %in% df$Que)
    { 
      return (as.numeric(as.character(df[df$Que == x,2]))) 
  }
  else 
  {
    return(0)
  }
})
fin.95 = df.m[df.m$wsum  >= .5 ,]

library(dplyr)
fin.95[,c(2,4,5,6,7,8,9)] = mutate_all(fin.95[,c(2,4,5,6,7,8,9)],  
                                       function(x) as.numeric(as.character(x)))
missing = cbind(unique(fin.95[! fin.95$Que %in% fin.95$Ref,c(3,9)]),
      unique(fin.95[! fin.95$Que %in% fin.95$Ref,c(3)]),
      rep(0,length(unique(fin.95[! fin.95$Que %in% fin.95$Ref,c(3)]))),
      rep(0,length(unique(fin.95[! fin.95$Que %in% fin.95$Ref,c(3)]))),
      rep(0,length(unique(fin.95[! fin.95$Que %in% fin.95$Ref,c(3)]))),
      rep(0,length(unique(fin.95[! fin.95$Que %in% fin.95$Ref,c(3)]))),
      rep(1,length(unique(fin.95[! fin.95$Que %in% fin.95$Ref,c(3)]))),
      rep(0,length(unique(fin.95[! fin.95$Que %in% fin.95$Ref,c(3)])))
      )
names(missing) = names(fin.95)
fin.95 = rbind(fin.95, missing)
fin.95$a = substr(fin.95$Ref, 1,1)
smi = read.table("atc.smi.txt.sorted.mannually.annotated.missing.smi.csv", header = T, sep = ",",comment.char = "" )
names(smi) = c("smi","Ref")
nrow(fin.95)
fin.95.smi = merge(fin.95, smi, by = "Ref")

write.table(fin.95.smi, file = "chemical.network.csv", sep =",", quote = F, eol = "\n", row.names = F, col.names = T )


hm = read.table("ATC_HEATMAP.csv", sep = ",", header = T)
names(hm) = c("Ref", "target", "name")
hm$cnt = rep(1, nrow(hm))
library(tidyr)
hm.w = spread(hm, key = "target", value = "cnt", fill = 0)
hm.w$name = NULL
hm.w= hm.w[! duplicated(hm.w),]
row.names(hm.w) = hm.w$Ref
hm.w.m = merge(hm.w, df, by = "Ref")

library(gplots)
hm.w$Ref = NULL
hm.w.m$Ref = NULL
hm.w.m = mutate_all(hm.w.m,  
           function(x) as.numeric(as.character(x)))
heatmap.2(data.matrix(hm.w), trace = "none")

hist(finger$wsum)
