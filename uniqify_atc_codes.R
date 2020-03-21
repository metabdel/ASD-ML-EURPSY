#setwd("D:\\Autism/")
library(data.table)
library(magrittr)
library(tidyr)
library(stringr)

atc = fread("ATC codes/atcid.no.combs.csv", sep = ",", header = T)
###only last level atcs 
atc = atc[nchar(atc$ATCCode) > 5, ]
atc.sp = split(atc, f = atc$NAME)
atc.linked = list()
for ( x in atc.sp)
{
   codes = unique(x$ATCCode)  
   for ( i in 1:length(codes))
   {
     atccode = codes[i] 
     matches = codes[-i]
     matches = c(atccode, matches)
     atc.linked[[atccode]] = matches 
   }
}
