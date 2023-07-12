library(openxlsx)
dat_all <- read.xlsx("合并数据.xlsx")
library(bioDist)
library(vegan)
library(gplots)

pairwise.adonis <- function(x,factors, sim.function = 'vegdist', sim.method = 'bray', p.adjust.m ='bonferroni')
{
  co = combn(unique(as.character(factors)),2)
  pairs = c()
  F.Model =c()
  R2 = c()
  p.value = c()
  for(elem in 1:ncol(co)){
    if(sim.function == 'daisy'){
      library(cluster); x1 = daisy(x[factors %in% c(co[1,elem],co[2,elem]),],metric=sim.method)
    } else{x1 = vegdist(x[factors %in% c(co[1,elem],co[2,elem]),],method=sim.method)}
    
    ad = adonis(x1 ~ factors[factors %in% c(co[1,elem],co[2,elem])] );
    pairs = c(pairs,paste(co[1,elem],'vs',co[2,elem]));
    F.Model =c(F.Model,ad$aov.tab[1,4]);
    R2 = c(R2,ad$aov.tab[1,5]);
    p.value = c(p.value,ad$aov.tab[1,6])
  }
  p.adjusted = p.adjust(p.value,method=p.adjust.m)
  sig = c(rep('',length(p.adjusted)))
  sig[p.adjusted <= 0.05] <-'.'
  sig[p.adjusted <= 0.01] <-'*'
  sig[p.adjusted <= 0.001] <-'**'
  sig[p.adjusted <= 0.0001] <-'***'
  
  pairw.res = data.frame(pairs,F.Model,R2,p.value,p.adjusted,sig)
  print("Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1")
  return(pairw.res)
} 

bac_24w <- asin(sqrt(dat_all[,126:224]))
c<-psych::corr.test(bac_24w, method = "kendall",adjust = "BH")
genus_matrix <- as.matrix(bac_24w)
c_dist<-as.dist(1-cor(t(c$r), method="spearman"))
clu<-hclust(c_dist, method="ward.D2")
plot(clu)
tauD <- tau.dist(c$r)
tauD_new<-as.matrix(tauD)
clust = cutree(clu, k=5) ###k是可变的，看哪个结果好多少组CAG
table(clust)
clust<-as.data.frame(clust)
write.csv(clust,"CAG.csv")
colors <- seq(-0.4,0.4,length=100)
clust$clust<-factor(clust$clust)
a<-adonis(tauD_new ~ clust,data=clust);a
pairwise.adonis(tauD_new,clust$clust)##两两比较差别越大越好
plot_color <- c('#C0392B','#9B59B6','#2980B9','#1ABC9C','#F1C40F')[clust$clust] # '#F1C40F','#2ECC71','#ECF0F1''#2ECC71','#ECF0F1','#34495E'
my_palette <- colorRampPalette(c("blue","white","red"))
pdf("CAG.pdf",height = 15,width = 15)
heatmap.2(c$r, col=my_palette, 
          breaks = colors,
          density.info="none", trace="none", 
          ColSideColors = plot_color,
          RowSideColors = plot_color,
          distfun   = function(x) as.dist(1-cor(t(x), method="spearman")), 
          hclustfun = function(x) hclust(x, method="ward.D2"),
          symm=F,symkey=F,symbreaks=T, scale="none")
dev.off()

bac_24w_1 <- dat_all[,126:224]
for (i in 1:length(unique(clust$clust))){
  a = rowSums(bac_24w_1[,rownames(clust)[which(clust$clust==i)]])
  bac_24w_1 = cbind(bac_24w_1,a)
  names(bac_24w_1)[dim(bac_24w_1)[2]] = paste("CAG",i,sep = "")
}

bac_24w_1$ID <- dat_all$number2
write.csv(bac_24w_1,"bac_24w.csv",quote = F,row.names = F)
