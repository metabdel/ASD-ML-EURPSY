setwd("~/Autism/")
source("Autism_project_R/format_data.R") 
library(tidyr)
library(data.table)
library(sparklyr)
asd.s = asd.cases# [sample(1:nrow(asd.cases), 42),]
un.s = unaffected.cases[sample(1:nrow(unaffected.cases), 94882),]
tot = rbind(asd.s, un.s)
atclevel = c("A","B","C","D","G","H","J", "L", "M", "N", "P", "R", "S","V")
'%ni%' <- Negate('%in%')
final.data = data.frame()
seen = c()
for ( i in 1:nrow(tot ) ) 
{
  print( paste0("working on row ", i, " of ", nrow(tot)))
  rw = tot[i,]
  rw.dat = dat.2[which(dat.2$CID == rw$CID ), ]
  c.sex = unique(rw.dat$sex)
  c.doby = unique(rw.dat$doby)
  id = paste(rw$CID, rw$MID, rw$FID , sep = "-")
  if ( id %in% seen )
  {
    next 
  }
  else 
  {
    if (nrow(rw.dat) > 0 )
    {
      seen = c(seen, id)
      sibasd = 0 
      if ( unique(rw$MID) %in% asd.cases$MID )
      {
        sibasd = 1 
      }
      page = unique(rw.dat$page)
      mage = unique(rw.dat$mage)
      par.age.diff = abs(page - mage)
      page.y = strsplit(unique(rw.dat$pdob.y), "-")[[1]][1]
      mage.y = strsplit(unique(rw.dat$mdob.y), "-")[[1]][1]
      dx = unique(rw.dat$atc_manual)
      level1atc = unlist(lapply(strsplit(dx,  split = ""), FUN = function(x) x[1]))
      atc.freq  = as.data.frame(table(level1atc))
      atc.freq$Freq = as.numeric(as.character(atc.freq$Freq))
      missingatc = which(atclevel %ni% atc.freq$level1atc )
      missing.tab = as.data.frame(cbind(atclevel[missingatc], rep(0, length(atclevel[missingatc]))))
      names(missing.tab) = names(atc.freq)
      missing.tab$Freq = as.numeric(as.character(missing.tab$Freq))
      atc.total.freq = rbind(missing.tab, atc.freq) 
      atc.total.freq = atc.total.freq[order(as.character(atc.total.freq$level1atc)),]
      atc.total.freq$Freq = as.numeric(as.character(atc.total.freq$Freq))
      atc.total.freq$level1atc = as.character(atc.total.freq$level1atc)
      final.df  = atc.total.freq %>% spread(level1atc, Freq) %>% as.data.frame() 
      class = unique(rw.dat$asd)
      final.df = cbind(final.df, c.sex  , c.doby , id , sibasd , page ,  mage , par.age.diff , page.y , mage.y, class )
      #row.names(final.df) = final.df$id 
      #final.df$id = NULL
      final.data = rbind(final.data, final.df) 
    }
  }
}
nrow(final.data)
final.data.no.dup  = final.data[!duplicated(final.data),]
write.table("asd.data.for.model.oversample.csv", sep = ",", x = final.data.no.dup, quote = F, eol = "\n", na = "NA", col.names = T, fileEncoding = "UTF8", row.names = F )
92y
asd.dat = fread("asd.data.for.model.oversample.csv", sep = ",", header = T, na.strings = "NA", stringsAsFactors = F, strip.white = T, data.table = T )
asd.dat = asd.dat[! duplicated(asd.dat$id),]
row.names(asd.dat) = asd.dat$id
asd.dat$id = NULL
asd.dat$c.sex = as.factor(asd.dat$c.sex)
asd.dat$sibasd = as.factor(asd.dat$sibasd)
asd.dat$class = as.factor(asd.dat$class)
asd.dat = asd.dat[sample(nrow(asd.dat)), ]
asd.dat$page.y = NULL
asd.dat$mage.y = NULL
asd.dat$sibasd = NULL
asd.dat.omit.na = asd.dat[complete.cases(asd.dat),]

#library(party)
library(randomForest)
#library(rpart.plot)
library(pROC)
#fit = randomForest(class ~ .,  asd.dat.omit.na , ntree = 1000 ,  mtry = 15 )
asd.dat.imput = rfImpute(asd.dat, asd.dat$class )
asd.dat.imput$`asd.dat$class` = NULL
asd.dat.imput = asd.dat.imput[,c(1:14,22)]
fit = randomForest(formula = asd.dat.omit.na$class ~ ., data = asd.dat.omit.na , 
                   ntree = 1000 , 
                   
                   
                   
                   verbose = T)
rf.roc = roc(asd.dat.omit.na$class, fit$votes[,2])
auc(rf.roc)
plot(rf.roc)
imp = as.data.frame(fit$importance)
imp$features = row.names(imp)
imp =  imp[order(imp$MeanDecreaseGini),]

View(imp)
library(randomForest)
t$asd = factor(t$asd)
