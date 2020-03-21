setwd("~/schlesslab/Autism/")
source("Autism_project_R/format_data.R") 
source("Autism_project_R/sample_cases.R")
library(data.table)
library(ggplot2)
head(asd.cases)
head(unaffected.cases)
unaff.sib = unaffected.cases[unaffected.cases$MID %in% asd.cases$MID,]
asd.sib = asd.cases[asd.cases$MID %in% unaffected.cases$MID, ]
ff = fread("bootstraped.1000.csv", header = T,  sep = ",", verbose = T, stringsAsFactors = T , showProgress = T )
ff = ff[ff$fccnt != "Inf", ] 
ff = ff[ff$fccnt != "-Inf", ]
ff$dg = as.factor(as.character(ff$dg))
ff.sp = split(ff, f = ff$dg)
min(unlist((lapply(ff.sp, FUN = nrow))))
median.drugs = sort(unlist(lapply(ff.sp, FUN = function(x) { median(-log(x$pvcnt)) }  )), decreasing = T)
names.top.100 = names(median.drugs[1:100])
sample_asd_unaff_cases = function(drug, numasd =150 )
{
  asd.sample = asd.sib[sample(sample(1:nrow(asd.sib), numasd)),]
  un.sample = unaff.sib[unaff.sib$MID %in% asd.sample$MID, ]
  asd.d = asd[asd$CID %in% asd.sample$CID, ]
  asd.d = asd.d[asd.d$atc_manual == drug, ]
  un.d = unaffected[unaffected$CID %in% un.sample$CID, ]
  un.d = un.d[ un.d$atc_manual == drug , ]
  asd.f = nrow(asd.d) / nrow(asd.sample)
  un.f = nrow(un.d) / nrow(un.sample)
  out = list()
  out$asdf = asd.f
  out$unf = un.f 
  return(out)
}
sibling_freq = data.frame()
drug = c()
type = c()
freqs = c()
iii = 1
for ( i in names.top.100[0:50] )
{ 
  print(paste0("    working on ", i, " ", iii , "of 100"))
  iii = iii + 1
    for( ii in 1:15)
    {
      print(paste0("working on ", ii))
      freq = sample_asd_unaff_cases(i)
      drug = c(drug, i)
      type = c(type, "asd")
      freqs = c(freqs, freq$asdf)
      drug = c(drug, i)
      type = c(type, "unaffected")
      freqs = c(freqs, freq$unf)
    }
}
sibling.compare = as.data.frame(cbind(drug, type, freqs))
sibling.compare$freqs = as.numeric(as.character(sibling.compare$freqs))
sibling.compare = read.table("sibling.compare.csv", header = T, sep = ",")
library(ggplot2)
sibling.sp = split( sibling.compare, f = sibling.compare$type )
ggplot(sibling.compare, aes(x = reorder(factor(drug), -freqs, FUN = median),
                            y = freqs, fill = factor(type))) + 
  geom_boxplot() + 
  coord_cartesian(xlim = c(1,15), ylim = c(0, 1.25)) + 
  geom_hline(yintercept = 0, color = "red", size = 1.25) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1, size = 15))
 a = sibling.compare[sibling.compare$drug =="R01AD08",]
 a.asd = a[a$type =="asd",]
 a.unaffected = a[a$type =="unaffected",]
 t.test(a.asd$freqs, a.unaffected$freqs) 



