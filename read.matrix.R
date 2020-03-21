mat = read.table("../data/autismcases.preg-0.paidpills-gt-0.level4.mat", header= T, sep =",", row.names = 1)
class = mat$class
fulldx =mat[,c(ncol(mat)-1,ncol(mat))]
mat[,c(ncol(mat)-1,ncol(mat))] = NULL
mat[,which(apply(mat, 2, function(x) sum(x)) ==0)] = NULL
ncol(mat)
mat.cor = cor(mat)
mat.cor[upper.tri(mat.cor)] = 0
diag(mat.cor) = 0
mat.new <- mat[,!apply(mat.cor,2,function(x) any(x > 0.8 ))]
ncol(mat.new)
mat.new$class = class
mat.cp = split(mat.new, f = mat.new$class)
cols = colnames(mat.new)
atc.pvalue = data.frame()
pvalues = c()
fc = c()
name = c()
for ( i in 1:ncol(mat.new))
{
  asd.seen = sum(as.numeric(mat.cp$ASD[,i]))
  if (asd.seen == 0) next 
  asd.tot = nrow(mat.cp$ASD)
  asd.w.o = asd.tot - asd.seen
  if (asd.w.o == 0 ) next 
  un.seen = sum(as.numeric((mat.cp$unaffected[,i])))
  if (un.seen == 0) next 
  un.tot = nrow(mat.cp$unaffected)
  un.w.o = un.tot - un.seen
  if (un.w.o == 0 ) next 
  logfc = log10((asd.seen/asd.tot )/ (un.seen/un.tot))
  tab = data.matrix(rbind(c(asd.seen,asd.w.o),c(un.seen,un.w.o)))
  test = fisher.test(tab)
  n = cols[i]
  pvalues = c(pvalues,test$p.value)
  fc = c(fc, logfc)
  name = c(name, n)
}
atc.pvalue = as.data.frame(cbind(pvalues, fc, name))
atc.pvalue = atc.pvalue[order(atc.pvalue$pvalues),]
pval = as.numeric(as.character(atc.pvalue$pvalues))
atc.pvalue$pvalues = as.numeric(as.character(atc.pvalue$pvalues))
atc.pvalue$adjusted = p.adjust(pval, method = "fdr" , n = length(pval))
#View(atc.pvalue)
finalcols = as.vector(atc.pvalue[which(atc.pvalue$adjusted < .10),]$name )
mat.final.cols = mat[,which(colnames(mat) %in% finalcols)]
library(gplots)
asd = mat.final.cols[which(fulldx$class == "ASD"),]
asd =asd[which(apply(asd, 1, function(x) sum(x)) > 0) ,] 
non = mat.final.cols[which(fulldx$class != "ASD"),]
asd.dx = fulldx[which(fulldx$class == "ASD"),]
non.rand = non[sample(1:nrow(non), size = 1200),]
non.rand =non.rand[which(apply(non.rand, 1, function(x) sum(x)) > 0) ,] 
asd$type = rep(x = "blue", nrow(asd))
non.rand$type = rep(x = "green", nrow(non.rand))
full = rbind(non.rand, asd)
full.class = full$type
full$type = NULL
#full$type = as.factor(full.class)
library(vegan)
heatmap.2(data.matrix(full), trace = "none", RowSideColors = full.class, distfun = function(x) vegdist(x, method ="euclidean", binary = T), hclustfun = function(x) hclust(x, method = "ward.D2"), cexCol = 1.5, margins = c(14, 14))
library(randomForest)
full.rf = randomForest(type ~. , data = full , ntree = 200, importance = T)
asd$type = NULLx
xxxx
head(full)
scores = as.data.frame(mat.pca$scores[,1:3])
library(plotly)
library(Rtsne)
library(vegan)
dist = vegdist(data.matrix(full), method = "jaccard",binary = T )
scores = Rtsne( X=dist, dims = 3, perplexity = 30 , theta = 0 ,is_distance = F, max_iter = 5000, verbose = T, check_duplicates = F)
s = as.data.frame(scores$Y)
s$class = full.class

s
plot_ly(data= s, x = ~V1, y = ~V2, z = ~V3, color = ~class)
