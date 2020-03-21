db = read.table("../data/drugbankid.csv", header = T, sep =",")
atc = read.table("../data/atcid.csv", header = T, sep =",")
head(atc)
head(db)
colnames(atc) = c("n", "atc")
db.atc = merge(db,atc, by = "n")
mat.atc = colnames(mat)
db.atc.mat = db.atc[which(db.atc$atc %in% mat.atc), ]
db.atc.mat.rel = db.atc.mat[,c(3,5)]
db.atc.mat.rel$pres = rep(1, nrow(db.atc.mat.rel))
library(tidyverse)
db.atc.mat.rel = db.atc.mat.rel[! duplicated(db.atc.mat.rel), ]
db.atc.wide = db.atc.mat.rel %>% spread(UniProt.Name ,  pres, fill = 0 )
db.atc.wide[1:5,1:5]
write.table(db.atc.wide, "../data/db.atc.wide.csv", sep = ",", quote = F, row.names = F, eol = "\n")
