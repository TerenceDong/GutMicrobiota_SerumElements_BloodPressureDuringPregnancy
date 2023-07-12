library(openxlsx)
dat_all <- read.xlsx("合并数据.xlsx")
ele <- c(67:92)

library(reshape2)
library(ggplot2)
library(viridis)
library(ggsci)
library(psych)
library(ggcorrplot)
dat_all_ele <- as.matrix(dat_all[,ele])
dat_all_ele <- log(dat_all_ele)
ele_corr <- corr.test(dat_all_ele, method = "spearman",adjust = "BH")
ele_corr_value <- ele_corr$r
ele_corr_p <- ele_corr$p

for (i in 1:26){
  for (j in 1:i){
    ele_corr_p[i,j] = ele_corr_p[j,i]
  }
}

p <- ggcorrplot(ele_corr_value,
                method = "circle", #设置相关性图展示类型
                outline.color = "white",#设置相关性图边框的颜色
                ggtheme = theme_bw(), #设置只展示定部panel
                colors = c("#002448","#ffffff","#9b2226"),
                p.mat = ele_corr_p,lab_size = 15,
                legend.title = "Spearman's rho",tl.cex = 25,tl.srt = 45
)
ggsave("ele_correlation.pdf",p,device = "pdf",height = 15,width = 15)
