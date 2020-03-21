###average performance 
lapply(performance_all.sp, FUN = function(x) {
    print(unique(x$learner))
    
   
    
    print(paste(mean(x$pre), "precision", sep = " ")) 
    print(paste(mean(x$fpr), "false pos", sep = " ")) 
    print(paste(mean(x$acc), "accuracy", sep = " ")) 
    print(paste(mean(x$sen), "sensitivity", sep = " ")) 
    print(paste(mean(x$spe), "specificity", sep = " "))  
    print(paste(mean(x$pre), "precision", sep = " ")) 
    print(paste(mean(x$fpr), "false pos", sep = " ")) 
    print(paste(mean(x$ac), "auc", sep = " ")) 
})

library(ggplot2)
library(magrittr)
reorder(as.character(varimp_all$feat) , -varimp_all$MeanDecreaseGini , FUN = median) %>% 
    head(20)


###average variable importance 


ggplot(varimp_all, aes(  x = reorder(feat , -MeanDecreaseGini , FUN = median), 
                         y = MeanDecreaseGini)) + 
    geom_boxplot() + coord_cartesian(xlim = 1:20 ) +
    theme_bw() + 
    theme(
        axis.text.x = element_text(size = 20, hjust = 1, vjust = 0, angle = 90), 
        axis.text.y = element_text(size = 20) , 
        legend.position = "none"
    )  



###plot ROCS 

library(pROC)
performance_all.sp

r.log = roc(predictor = rocs_all$logit , 
            response =  rocs_all$class )
r.log
plot(r.log, legacy.axes = T)

r.rf = roc(predictor = rocs_all$rf , response =  rocs_all$class )
r.mlp = roc(predictor = rocs_all$mlp , response =  rocs_all$class )


plot(r.rf, add=TRUE, col='red')
plot(r.mlp, add=TRUE, col='green')


###heatmap 

topfeats = c("missing_p", "N06.y", "pagediff", "count_drugs_scaled.y",
             "count_drugs_scaled.x", "L02.x", "V03.x", "P02.x",
             "A03.x", "A08.x", "D09.x", "A06.y",
             "A11.y", "G03.y", "G04.y", "B01.y",
             "D04.y", "B02.y", "D09.y", "G02.y", "asd_status")
library(gplots)
heatmap.dat = all_data_atc_scaled_com[,topfeats]
heatmap.dat$col = rep("blue", nrow(heatmap.dat))
heatmap.dat[heatmap.dat$asd_status == 1, "col"] = "green"
ht.asd = heatmap.dat[heatmap.dat$asd_status == 1, ]
ht.un = heatmap.dat[heatmap.dat$asd_status == 0, ]
ht.un = ht.un[sample(1:nrow(ht.un), 1000, replace = F),]
ht = rbind(ht.asd ,  ht.un)
cols = ht$col 
ht$col  = NULL
class = ht$asd_status 
ht$asd_status  = NULL
heatmap.2(data.matrix(ht), 
          trace = "none", col = "redgreen", 
          RowSideColors = cols)
pc = prcomp(data.matrix(ht))
pc = pc$x %>%  as.data.frame()
pc$class = class
ht$class = cols
library(plotly)
plot_ly(data = pc, x = ~PC1, y=  ~PC2, z=  ~PC3 , color = ~class )


          