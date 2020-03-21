level1 = read.table("../data/autismcases.pregb", sep  = ",", header = F)
head(level1)
colnames(level1 ) = c("MID-CID", "ASD", "DX", "LEVEL 1 ATC Code", "Counts") 
#level1.sp = split(level1, f = level1$`MID-CID`)
#level1.full = data.frame()
#for ( i in level1.sp )
#{
#  i$freq = i$Counts / sum(i$Counts)
#  level1.full = as.data.frame(rbind(level1.full, i))
#}

library(ggplot2)

s = level1[which(level1$`LEVEL 1 ATC Code` == "N"),]
s.asd  =  s[which(s$ASD=="ASD"),]$scale
s.un = s[which(s$ASD=="unaffected"),]$scale
t.test(s.asd, s.un, alternative = "greater" )


alevel1.asd = level1[which(level1$ASD=="ASD"),]
ggplot(level1, aes(factor(ASD), Counts, fill = factor(ASD))) + geom_boxplot() + facet_wrap(~`LEVEL 1 ATC Code`) + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))  +   theme( axis.title=  element_blank(), 
      axis.text = element_text(size = 40 )  , strip.text = element_blank(), legend.text=element_text(size=40), legend.title =  element_blank())
