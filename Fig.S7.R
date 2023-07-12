# 物种累积曲线和稀释曲线 -------------------------------------------------------------
# 稀释曲线
library(openxlsx)
library(dplyr)
library(ggplot2)
library(reshape)
dat25 <- read.xlsx("画图数据.xlsx",sheet = 28)
dat25_1 <- melt(dat25,id.vars = "ID",value.name = "observed",variable.name = "depth")
colnames(dat25_1) <- c("ID","depth","observed")
dat25_1$ID <- factor(dat25_1$ID)
dat25_1$depth <- gsub("X","",dat25_1$depth)
dat25_1$depth <- as.numeric(dat25_1$depth)
str(dat25_1)
spline_d <- dat25_1 %>% 
  split(.$ID) %>% 
  lapply(function(x) as.data.frame(spline(x$depth, x$observed))) %>% 
  bind_rows(.id = "ID")
p <- ggplot(data = dat25_1, aes(x = depth, y = observed, color = ID)) +
  geom_line(data = spline_d, aes(x = x, y = y)) +
  scale_color_manual(values = rep("#a4bfd9",733))+
  labs(x="Sequencing Depth",y="Observed ASVs")+
  scale_x_continuous(breaks = c(0,5000,10000,15000,20000,25000),
                     label = c("0","5000","10000","15000","20000","25000"))+
  scale_y_continuous(breaks = c(0,50,100,150,200,250,300),
                     label = c("0","50","100","150","200","250","300"))+
  theme_classic()+
  theme(legend.position = "none",
        axis.text = element_text(color = "black",size = 10),
        axis.title = element_text(color = "black",size = 12))
p
ggsave("rarefactionCurve.pdf",width = 7,height = 5)

# 物种累积曲线
library(vegan)
dat26 <- read.xlsx("画图数据.xlsx",sheet = 29,rowNames = TRUE)
sp1 <- specaccum(dat26, method="random")
pdf("Accumulation_Species.pdf",width = 7,height = 5)
plot(sp1, ci.type="poly", col="#5d8aa8", lwd=2, ci.lty=0, ci.col="lightblue",xlab="Numbers of samples",ylab="Observed ASVs")
dev.off()
