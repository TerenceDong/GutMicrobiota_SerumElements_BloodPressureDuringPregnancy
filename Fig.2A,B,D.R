library(openxlsx)
dat_all <- read.xlsx("合并数据.xlsx")
name_all <- names(dat_all)
ele <- c(67:92)
bac <- c(126:224)

# Fig.2A ------------------------------------------------------------------
d1 <- dat_all[,ele]
d1 <- log10(d1)

result <- as.data.frame(matrix(NA,ncol=2))
colnames(result) <- c("element","mean")
for (i in 1:dim(d1)[2]) {
  result[i,1] = colnames(d1)[i]
  result[i,2] = mean(d1[,i])
}
result <- result[order(result$mean,decreasing = T),]
elementRank <- result$element
d1 <- melt(d1,measure.vars = colnames(d1),variable.name = "element",value.name = "concentration")
d1$element <- factor(d1$element,levels = elementRank)

library(ggplot2)
p <- ggplot(d1, aes(x = element, y = concentration)) +
  geom_violin(color="#89bfe9",fill="#89bfe9", alpha = 0.5,width = 2.2) +
  geom_boxplot(color="#89bfe9", width = 0.2) +
  theme_bw()+
  labs(x="",y="Concentration (ug L-1)")+
  scale_y_continuous(breaks = seq(-3,7,1),labels = paste("10^",-3:7,sep=""))+
  theme(axis.text =  element_text(color = "black",size = 20),
        axis.title = element_text(color = "black",size = 23))
ggsave("elementDistribution.pdf",width = 15,height = 9)

# Fig.2B ------------------------------------------------------------------
dat1 <- read.xlsx("画图数据.xlsx",sheet = 1)
dat1 <- dat1[order(-dat1$`f__Lachnospiraceae;g__[unknown]`,-dat1$g__Faecalibacterium),]
dat1$ID <- c(1:nrow(dat1))
dat1 <- melt(dat1,id.vars = "ID",variable.name = "microbiota",value.name = "relativeAbundance")
dat1$microbiota <- factor(dat1$microbiota, levels = rev(c("f__Lachnospiraceae;g__[unknown]","g__Faecalibacterium","g__Bacteroides","g__Subdoligranulum","g__Blautia","g__Bifidobacterium","g__Prevotella","g__Escherichia-Shigella","g__Roseburia","g__[Eubacterium]_hallii_group","Others"))) 
p <- ggplot(dat1, aes(ID,  relativeAbundance, fill = microbiota)) +
  geom_col(position = "stack", width = 0.8) +
  scale_fill_manual(values =  rev(c( "#91bde2", "#83c5b0",
                                     "#8fb79d","#eadfc3",
                                     "#eec16c","#d75032",
                                     "#ce9c83","#b9b7a2",
                                     "#f66f69","#289ebc",
                                     "#e8edf3"))) +
  labs(x = 'Individual ID', y = 'Relative Abundance (%)', fill = "") +
  theme_classic()+
  guides(fill=guide_legend(nrow=11,reverse=TRUE))
ggsave("RelativeAbundance.pdf",height = 5,width =9)

# Fig.2D ------------------------------------------------------------------

# envfit analysis
bac_24w <- dat_all[,name_all[bac]]
rownames(bac_24w) <- dat_all$number2
bac_24w <- asin(sqrt(bac_24w))
metadata <- dat_all[,c("age","education","income","pre_BMI","smokHis","parity","Mn","Ni","Cu","Zn","As","Mo","Cd","Hg","Tl","Fe","Sr","Ag","Sb","U","V","Ba","Co","Rb","Cs","La","Pr","Nd","Na","Mg","K","Ca","weight_24","TP_24","ALB_24","CHOL_24","TG_24","FBG_24")]
rownames(metadata) <- dat_all$number2
mds <- cmdscale(dist(bac_24w,method = "euclidean"), eig = TRUE)
mds_point <- data.frame(mds$points)   # 得到各样本的坐标
colnames(mds_point) <- c('X1','X2')
eig <- mds$eig
library(vegan)
set.seed(2023)
fit <- envfit(mds, metadata,permutations = 999)
fit

# visualization

dat24 <- read.xlsx("画图数据.xlsx",sheet = 24,rowNames = FALSE)
dat24$variable <- factor(dat24$variable,levels = dat24$variable)
p <- ggplot(dat24,aes(x=variable,y=r2,fill=group))+
  geom_col()+
  scale_fill_manual(values = c("#ce9c83","#4dac8e","#eec16c"))+
  theme_bw()+
  theme(axis.title = element_text(color = "black",size = 17),
        axis.text = element_text(color = "black",size = 13),
        axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1))+
  labs(x="",y="R2")
ggsave("GutVariationBarplot.pdf",width = 13,height = 7)





