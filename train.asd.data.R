library(data.table)
library(magrittr)
library(tidyr)
library(stringr)
library(plyr)
library(dplyr)
#library(caret)
library(DMwR)
library(RSNNS)
library(pROC)
library(parallelRandomForest)
library(party)
set.seed(10)
setwd("/mnt/d/Autism/")
setwd("/media/rrahman/Seagate Backup Plus Drive/Autism/")

all_data_atc_scaled_com= fread( "all_data_selected_atc.level2.softmax.csv", header =T, sep = "," ) 
all_data_atc_scaled_com$ses = all_data_atc_scaled_com$ses %>%  as.character() %>%  as.factor()
all_data_atc_scaled_com$missing_p = all_data_atc_scaled_com$missing_p %>%  as.character() %>%  as.factor()
all_data_atc_scaled_com$missing_m = NULL
#all_data_atc_scaled_com = all_data_atc_scaled_com[all_data_atc_scaled_com$missing_p == 0, ]
all_data_atc_scaled_com$missing_p = NULL
#all_data_atc_scaled_com = all_data_atc_scaled_com[, c(1:4,6:213)]
all_data_atc_scaled_com$asd_status = all_data_atc_scaled_com$asd_status %>%  as.character() %>%  as.factor()
rocs_all = data.frame()
performance_all = data.frame()
varimp_all = data.frame()
  get_perf = function(pred_class, pred_prob , obs_class )
  {
    df = cbind(pred_class, as.character(obs_class)) %>%  as.data.frame()
    colnames(df) = c("predicted", "observed")
    q = table(df)
    q[1] = q[1] %>%  as.numeric()
    q[2] = q[2] %>%  as.numeric()
    q[3] = q[3] %>%  as.numeric()
    q[4] = q[4] %>%  as.numeric()
    sen = q[4]/(q[4] + q[3])
    spe = q[1]/(q[1] + q[2])
    pre = q[4]/(q[4] + q[2])
    fpr = q[2]/(q[2] + q[1])
    fdr = q[2]/(q[2] + q[4])
    acc = (q[1] + q[4] )/(q[1] + q[2] + q[3] + q[4] )
    f1_asd = (2 * q[4] ) / ((2*q[4]) + q[2] +  q[3] )
    f1_neg = (2 * q[1] ) / ((2*q[1]) + q[2] +  q[3] )
    r = roc(predictor = pred_prob[,1] , response =  obs_class )
    ac = auc(r)
    rw = c(q[4], q[3], q[2], q[1], sen, spe, pre, fpr , fdr, acc, f1_neg, f1_asd, ac ) %>%  as.data.frame %>% t %>% as.data.frame()
    return(rw)
  }
###revaluate performance of SMOTE vs no oversampling vs inverse class weights vs undersampling 
  all_data_atc_scaled_com =all_data_atc_scaled_com[sample(1:nrow(all_data_atc_scaled_com),nrow(all_data_atc_scaled_com)),]
  all_data_atc_scaled_com.sp = split( all_data_atc_scaled_com, f= all_data_atc_scaled_com$asd_status ) 
  size.un = round(.8*nrow(all_data_atc_scaled_com.sp$`0`))
  size.asd = round(.8*nrow(all_data_atc_scaled_com.sp$`1`))
  total.un = 1:nrow(all_data_atc_scaled_com.sp$`0`)
  total.asd = 1:nrow(all_data_atc_scaled_com.sp$`1`)
  train.un = sample(1:nrow(all_data_atc_scaled_com.sp$`0`), size.un ) 
  train.asd = sample(1:nrow(all_data_atc_scaled_com.sp$`1`), size.asd ) 
  test.un = total.un[which(! total.un %in% train.un ) ] 
  test.asd = total.asd[which(! total.asd %in% train.asd ) ] 
  training.dat.un = all_data_atc_scaled_com.sp$`0`[train.un, ]
  training.dat.asd = all_data_atc_scaled_com.sp$`1`[train.asd, ]
  training.dat = rbind(training.dat.un, training.dat.asd)
  testing.dat.un = all_data_atc_scaled_com.sp$`0`[test.un, ]
  testing.dat.asd = all_data_atc_scaled_com.sp$`1`[test.asd, ]
  testing.dat = rbind(testing.dat.un, testing.dat.asd)
  testing.class = testing.dat$asd_status %>% as.factor
  testing.dat$asd_status = NULL
  ##fit no weight/smote/undersample 
  training.dat.no.fix = training.dat[sample(1:nrow(training.dat), nrow(training.dat)), ]
  training.class = training.dat.no.fix$asd_status   %>% as.factor
  training.dat.no.fix$asd_status = NULL
  fit_rf = randomForest(x =  data.matrix(training.dat.no.fix) , training.class, 
                                              ntree = 100, nthreads = 4 , mtry = 20, 
                                              localImp = F, proximity = F) 
  pred.prob.rf = predict(fit_rf, data.matrix(testing.dat), type = "prob")
  pred.prob.rf.class = round(pred.prob.rf[,2]) %>%  as.data.frame()
  rf.no.opti = get_perf(pred.prob.rf.class,pred.prob.rf, testing.class  )
  names(rf.no.opti) =   c("TP", "FN","FP","TN" , "sen", "spe", "pre", "fpr", 
                    "fdr", "acc", "f1neg", "f1asd", "ac")
  rf.no.opti
  ##weighted classes 
  wy = 1/(sum(training.class)/length(training.class) ) 
  fit_rf = randomForest(x =  data.matrix(training.dat.no.fix) , training.class, 
                                              ntree = 100, nthreads = 4 , mtry = 20, classwt = c(1,wy),  
                                              localImp = F, proximity = F) 
  pred.prob.rf = predict(fit_rf, data.matrix(testing.dat), type = "prob")
  pred.prob.rf.class = round(pred.prob.rf[,2]) %>%  as.data.frame()
  rf.no.opti = get_perf(pred.prob.rf.class,pred.prob.rf, testing.class  )
  names(rf.no.opti) =   c("TP", "FN","FP","TN" , "sen", "spe", "pre", "fpr", 
                    "fdr", "acc", "f1neg", "f1asd", "ac")
  rf.no.opti
  ## equal class balance 
  training.non = training.dat[asd_status == 0 , ] 
  training.dat.2 = training.non[sample(1:nrow(training.non), sum(training.dat$asd_status)), ]
  training.dat.2 = rbind(training.dat.2, training.dat[asd_status == 1, ])
  training.dat.2 = training.dat.2[sample(1:nrow(training.dat.2), nrow(training.dat.2)), ]
  training.class.2 = training.dat.2$asd_status   %>% as.factor
  training.dat.2$asd_status = NULL
  training.dat.2$ses = training.dat.2$ses %>% as.factor
  fit_rf = randomForest(x =  data.matrix(training.dat.2) , training.class.2 , 
                                              ntree = 100, nthreads = 4 , mtry = 20, 
                                              localImp = F, proximity = F) 
  pred.prob.rf = predict(fit_rf, data.matrix(testing.dat), type = "prob")
  pred.prob.rf.class = round(pred.prob.rf[,2]) %>%  as.data.frame()
  rf.no.opti = get_perf(pred.prob.rf.class,pred.prob.rf, testing.class  )
  names(rf.no.opti) =   c("TP", "FN","FP","TN" , "sen", "spe", "pre", "fpr", 
                    "fdr", "acc", "f1neg", "f1asd", "ac")
  rf.no.opti
  
  ##fit smote data  
  training.smote = training.dat  
  training.smote$asd_status = as.factor(training.smote$asd_status)
  training.smote$ses = as.factor(training.smote$ses)
  training.smote = SMOTE(asd_status ~ . , data = training.smote , perc.over =  500, k = 100 ) 
  training.class = training.smote$asd_status %>% as.factor
  training.smote$asd_status = NULL
  fit_rf = randomForest( x =  data.matrix(training.smote) ,  y = training.class , 
                                              ntree = 100, nthreads = 4 , mtry = 20, 
                                              localImp = F, proximity = F) 
  pred.prob.rf = predict(fit_rf, data.matrix(testing.dat), type = "prob")
  pred.prob.rf.class = round(pred.prob.rf[,2]) %>%  as.data.frame()
  rf.no.opti = get_perf(pred.prob.rf.class,pred.prob.rf, testing.class  )
  names(rf.no.opti) =   c("TP", "FN","FP","TN" , "sen", "spe", "pre", "fpr", 
                    "fdr", "acc", "f1neg", "f1asd", "ac")
  rf.no.opti
  
for ( i in 1:10 )
{
  print(paste("working on fold:", i , sep = " "))
  all_data_atc_scaled_com =all_data_atc_scaled_com[sample(1:nrow(all_data_atc_scaled_com),nrow(all_data_atc_scaled_com)),]
  all_data_atc_scaled_com.sp = split( all_data_atc_scaled_com, f= all_data_atc_scaled_com$asd_status ) 
  size.un = round(.8*nrow(all_data_atc_scaled_com.sp$`0`))
  size.asd = round(.8*nrow(all_data_atc_scaled_com.sp$`1`))
  total.un = 1:nrow(all_data_atc_scaled_com.sp$`0`)
  total.asd = 1:nrow(all_data_atc_scaled_com.sp$`1`)
  train.un = sample(1:nrow(all_data_atc_scaled_com.sp$`0`), size.un ) 
  train.asd = sample(1:nrow(all_data_atc_scaled_com.sp$`1`), size.asd ) 
  test.un = total.un[which(! total.un %in% train.un ) ] 
  test.asd = total.asd[which(! total.asd %in% train.asd ) ] 
  training.dat.un = all_data_atc_scaled_com.sp$`0`[train.un, ]
  training.dat.asd = all_data_atc_scaled_com.sp$`1`[train.asd, ]
  training.dat = rbind(training.dat.un, training.dat.asd)
  testing.dat.un = all_data_atc_scaled_com.sp$`0`[test.un, ]
  testing.dat.asd = all_data_atc_scaled_com.sp$`1`[test.asd, ]
  testing.dat = rbind(testing.dat.un, testing.dat.asd)
  testing.class = testing.dat$asd_status
  testing.dat$asd_status = NULL
  #training.smote = SMOTE(asd_status ~ . , data = training.dat , perc.over =  500, k = 100 ) 
  #training.smote = training.smote[training.smote$asd_status == 1,]
  #training.dat = training.dat[training.dat$asd_status == 0 , ]
  training.dat = training.dat[sample(1:nrow(training.dat), nrow(training.dat)), ]
  training.class = factor(training.dat$asd_status)
  crfctrl = cforest_control(mtry =20 , ntree = 1000, trace = T)
  fit_crf = cforest(formula = asd_status ~ . , data = training.dat , controls = crfctrl )
  training.dat$asd_status = NULL
  fit_mlp = mlp(x =  data.matrix(training.dat), as.numeric(as.character(training.class)), size=c(10,10,10,2) , maxit = 100,
                linOut =F) 
  fit_rf = randomForest(x =  data.matrix(training.dat) , training.class, 
                                              ntree = 100, nthreads = 4 , mtry = 20, 
                                              localImp = F, proximity = F) 
 
  training.dat$training.class = as.factor(training.class)
  fit_logi = glm(training.class ~ . , data = training.dat, family = "binomial" )
  library(randomForest)
  pred.prob.mlp = predict(fit_mlp, data.matrix(testing.dat))
  pred.prob.crf = predict(fit_crf, data.matrix(testing.dat), type = "prob")
  pred.prob.rf = predict(fit_rf, data.matrix(testing.dat), type = "prob")
  pred.prob.log = predict(fit_logi, testing.dat, type = "response")
  pred.prob.log = pred.prob.log %>%  as.data.frame()
  pred.prob.mlp.class = round(pred.prob.mlp) %>%  as.data.frame()
  pred.prob.rf.class = round(pred.prob.rf[,2]) %>%  as.data.frame()
  pred.prob.log.class = round(pred.prob.log) %>%  as.data.frame()
  names(pred.prob.all) = c("mlp", "rf", "logit")
  pred.prob.all$class = testing.class 
  pred.prob.all$fold = rep(i, nrow(pred.prob.all))
  get_perf = function(pred_class, pred_prob , obs_class )
  {
    df = cbind(pred_class, as.character(obs_class)) %>%  as.data.frame()
    colnames(df) = c("predicted", "observed")
    q = table(df)
    q[1] = q[1] %>%  as.numeric()
    q[2] = q[2] %>%  as.numeric()
    q[3] = q[3] %>%  as.numeric()
    q[4] = q[4] %>%  as.numeric()
    sen = q[4]/(q[4] + q[3])
    spe = q[1]/(q[1] + q[2])
    pre = q[4]/(q[4] + q[2])
    fpr = q[2]/(q[2] + q[1])
    fdr = q[2]/(q[2] + q[4])
    acc = (q[1] + q[4] )/(q[1] + q[2] + q[3] + q[4] )
    f1_asd = (2 * q[4] ) / ((2*q[4]) + q[2] +  q[3] )
    f1_neg = (2 * q[1] ) / ((2*q[1]) + q[2] +  q[3] )
    r = roc(predictor = pred_prob[,1] , response =  obs_class )
    ac = auc(r)
    rw = c(q[4], q[3], q[2], q[1], sen, spe, pre, fpr , fdr, acc, f1_neg, f1_asd, ac ) %>%  as.data.frame %>% t %>% as.data.frame()
    return(rw)
  }
  varimp = fit_rf$importance %>%  as.data.frame() 
  varimp$feat = row.names(varimp)
  varimp$fold= rep(i, nrow(varimp)) 
  rw.log = get_perf(pred.prob.log.class , pred.prob.log ,  testing.class ) 
  names(rw.log) = c("TP", "FN","FP","TN" , "sen", "spe", "pre", "fpr", 
                    "fdr", "acc", "f1neg", "f1asd", "ac")
  rw.mlp = get_perf(pred.prob.mlp.class , pred.prob.mlp ,  testing.class ) 
  names(rw.mlp) = c("TP", "FN","FP","TN" , "sen", "spe", "pre", "fpr", 
                    "fdr", "acc", "f1neg", "f1asd", "ac")
  rw.rf = get_perf(pred.prob.rf.class , pred.prob.rf ,  testing.class ) 
  names(rw.rf) = c("TP", "FN","FP","TN" , "sen", "spe", "pre", "fpr", 
                   "fdr", "acc", "f1neg", "f1asd", "ac")
  combined = rbind(rw.log, rw.mlp, rw.rf) %>%  as.data.table()
  combined$learner = c("logit", "mlp", "rf")
  combined$fold = rep(i, nrow(combined))
  
  rocs_all = rbind(pred.prob.all, rocs_all)
  performance_all = rbind(combined, performance_all)
  varimp_all = rbind(varimp , varimp_all)
  
}

library(ggplot2)
save.image(file = "asd.10x.trained.level3.remove.missing.p.party.Rdata")

reorder(as.character(varimp_all$feat) , -varimp_all$MeanDecreaseGini , FUN = median) %>% 
  head(20)


###average variable importance 


ggplot(varimp_all, aes(  x = reorder(feat , -MeanDecreaseGini , FUN = median), 
                         y = MeanDecreaseGini ,
                         fill = factor(feat) )) + 
  geom_boxplot() + coord_cartesian(xlim = 1:20 ) +
  theme_bw() + 
  theme(
    axis.text.x = element_text(size = 20, hjust = 1, vjust = 0, angle = 90), 
    axis.text.y = element_text(size = 20) , 
    legend.position = "none"
  )  

####roc curves
performance_all = performance_all[complete.cases(performance_all),]
performance_all.sp = split(performance_all, f= performance_all$learner)
performance_all.sp$logit$ac %>%  mean
performance_all.sp$mlp$ac %>%  mean
performance_all.sp$rf$ac %>%  mean
performance_all.sp$logit$f1asd %>%  mean
performance_all.sp$mlp$f1asd %>%  mean
performance_all.sp$rf$f1asd %>%  mean

r = roc(predictor = rocs_all$logit , response =  rocs_all$class )

plot.roc(r, legacy.axes = T)






# 
# sp.all_data_atc_scaled_com = split(all_data_atc_scaled_com, f = all_data_atc_scaled_com$asd_status)
# sample.asd = round((9/10) * nrow(sp.all_data_atc_scaled_com$`1`))
# sample.un = round((9/10) * nrow(sp.all_data_atc_scaled_com$`0`))
# asd = sp.all_data_atc_scaled_com$`1`
# un = sp.all_data_atc_scaled_com$`0`
# asd.train = sample(1:nrow(asd), sample.asd, replace = F)
# un.train = sample(1:nrow(un), sample.un, replace = F)
# asd.tot = 1:nrow(asd)
# un.tot = 1:nrow(un)
# asd.vali = asd.tot[! asd.tot %in% asd.train  ]
# un.vali = un.tot[! un.tot %in% un.train ]
# training.asd =   asd[asd.train, ] 
# training.un  =  un[un.train, ] 
# training = rbind(training.asd, training.un)
# training$asd_status = factor(training$asd_status)
# 
# training.smote = SMOTE(asd_status ~ . , data = training , perc.over =  500, k = 100 ) 
# training.smote = training.smote[training.smote$asd_status == 1   , ] 
# training.smote = rbind(training.smote, training[training$asd_status == 0 , ])
# 
# training.smote = training.smote[sample(nrow(training.smote)),]
# #training.smote$missing_p = NULL
# asd.stat = training.smote$asd_status
# training.smote$asd_status = NULL
# library(parallelRandomForest)
# #fit = parallelRandomForest::randomForest( x =  data.matrix(training.smote),  nodesize = 10, 
# #                                         y = asd.stat, ntree = 500, nthreads = 20,
# #                                          mtry = 40,  importance = T,  verbose = T ,localImp = F, proximity = F )
# library(RSNNS)
# fit = mlp(x =  data.matrix(training.smote), as.numeric(as.character(asd.stat)), size=10, linOut =F) 
# #library(e1071)
# #fit = svm(data.matrix(training.smote), y = asd.stat , scale = F, kernel = "linear", probability = T )
# test.asd = asd[asd.vali, ]
# test.un = un[un.vali, ]
# testing = rbind(test.asd, test.un)
# testing = testing[sample(1:nrow(testing), nrow(testing), replace = F), ] 
# #testing$missing_p = NULL
# case.test = testing$asd_status %>% as.character() %>%  as.numeric() %>% as.factor()
# testing$asd_status= NULL
# library(randomForest)
# 
# pred = predict(fit, testing  )
# 
# pred.prob = predict(fit, testing , type = "prob" )
# pred.prob = pred.prob %>%  as.data.frame()
# sapply(pred.prob, function(x){
#   
# })
# library(pROC)
# r = roc(predictor = pred.prob$V1, response = case.test)
# 
# plot.roc(r, legacy.axes = T)
# pred = pred %>%  as.character() %>%  as.numeric()
# pred = round(pred, digits = 0)  %>%  as.vector()
# df = cbind(pred,as.character(case.test)) %>%  as.data.frame()
# colnames(df) = c("predicted", "observed")
# q = table(df)
# q
# q[1] = q[1] %>%  as.numeric()
# q[2] = q[2] %>%  as.numeric()
# q[3] = q[3] %>%  as.numeric()
# q[4] = q[4] %>%  as.numeric()
# sen = q[4]/(q[4] + q[3])
# spe = q[1]/(q[1] + q[2])
# pre = q[4]/(q[4] + q[2])
# fpr = q[2]/(q[2] +q[1])
# fdr = q[2]/(q[2]+q[4])
# acc = (q[1] + q[4] )/(q[1] + q[2] + q[3] + q[4] )

# q
# auc(r)
# print(paste("Sensitivity: ", sen , "
#  specificity: " , spe , "
#  precision: " , pre , "
#  fdr: " , fdr, "
#  fpr: ", fpr , sep = ""))
# 
# 
# 
