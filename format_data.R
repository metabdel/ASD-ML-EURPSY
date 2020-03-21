library(data.table)
library(magrittr)
setwd("D://Autism/")

dat = fread("unorganized/israeli_data_utf8.manual_atc.csv", sep = ",", header = F, na.strings = c("NA","")
            , stringsAsFactors = F, showProgress = T , fill = T, data.table = F )

names(dat) = c(
    "NA",    "CID",
    "FID",    "MID",
    "as.x",    "drugname",
    "pills",    "boxes",
    "atc",    "dddcode",
    "pillspaid",    "datemp",
    "datrp",    "dob",
    "sex",    "dod",
    "ses",    "sid",
    "sibtype.x",    "mdob.y",
    "sibtype.y",    "pdob.y",
    "mage",    "page",
    "datediff",    "exp",
    "atc2",    "preg",
    "dxdate",    "as.y",
    "dxdaty",    "dxdatm",
    "dxdatd",    "dxcode",
    "dxdesc",    "asd",
    "fudate",    "min",
    "timefu",    "doby",
    "atc_manual" 
)
dat.2 =dat[! duplicated(dat[,c(2,3,4,12,41)]),]
dat.sp = split(dat.2, f = dat.2$asd)
asd = dat.sp[[1]]
asd.preg = asd[asd$preg ==1 , ]
nrow(asd)
unaffected = dat.sp[[2]]
un.preg = unaffected[unaffected$preg == 1, ]
nrow(unaffected)
rm(dat.sp)
rm(dat)
unaffected.cases = unique(unaffected[,c(2,3,4)])
asd.cases = unique(asd[,c(2,3,4)])








