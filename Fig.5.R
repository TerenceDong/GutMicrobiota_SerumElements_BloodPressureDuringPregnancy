library(openxlsx)
dat_all <- read.xlsx("合并数据.xlsx")
name_all <- names(dat_all)
bac_24w_cp <- c(126:229) # bacteria & CAG
ele_sel <- c(69,70,71,72,74,75,76,77,78,79,81,83,84,85,89,90,91,92)
alpha <- c(230:233)

# associations between serum elements and bacterial relative abundance --------

r_24w_pc_a1 <- as.data.frame(matrix(NA,ncol = 6))
names(r_24w_pc_a1) <- c("ele_selment","outcome","beta","p","l95","u95")
for (i in 1:length(bac_24w_cp)) {
  for (j in 1:length(ele_sel)) {
    m = glm(asin(sqrt(dat_all[,name_all[bac_24w_cp[i]]])) ~ log(dat_all[,name_all[ele_sel[j]]])+age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI,data = dat_all, family = gaussian())
    r_24w_pc_a1[(length(ele_sel)*(i-1)+j),1] = name_all[ele_sel[j]]
    r_24w_pc_a1[(length(ele_sel)*(i-1)+j),2] = name_all[bac_24w_cp[i]]
    r_24w_pc_a1[(length(ele_sel)*(i-1)+j),3] = summary(m)$coefficients[2,1]
    r_24w_pc_a1[(length(ele_sel)*(i-1)+j),4] = summary(m)$coefficients[2,4]
    r_24w_pc_a1[(length(ele_sel)*(i-1)+j),5] = confint(m)[2,1]
    r_24w_pc_a1[(length(ele_sel)*(i-1)+j),6] = confint(m)[2,2]
  }
}
r_24w_pc_a1_c <- r_24w_pc_a1[1:1782,] #提取菌群相对丰度对应行

p <- as.data.frame(r_24w_pc_a1_c$p)
b <- as.data.frame(matrix(NA,ncol = 1))
names(b) <- "p"
m <- 18 #循环数
n <- 99 #每次循环内部次数
for (i in 1:m){
  j = n*(i-1)+1
  k = n*i
  part = p[c(j:k),]
  a = data.frame(p.adjust(part,method = "BH"))
  names(a) = "p"
  b = rbind(b,a)
}
b <- b[-1,]
r_24w_pc_a1_c$padj <- b

write.csv(r_24w_pc_a1_c,"24w_element_bacteria_adjust.csv",quote=F,row.names = F)

# visualization Fig.5B ----------------------------------------------------

dat7 <- read.xlsx("画图数据.xlsx",sheet = 7)
dat7$ele_selment <- factor(dat7$ele_selment,levels = c("Cu","Zn","As","Mo","Hg","Tl","Fe","Sr","Ag","Sb","V","Co","Rb","Cs","Na","Mg","K","Ca"))
dat7 <- dat7[order(dat7$ele_selment),]
dat7$position <- seq(1,nrow(dat7))
dat7$sig <- ifelse(dat7$padj <0.05,"sig","non-sig")
elementBreak <- aggregate(dat7$position, by = list(dat7$ele_selment), FUN = median)

library(ggplot2)
library(ggrepel)
p <- ggplot(dat7, aes(position, -log(padj, 10))) +
  geom_hline(yintercept = -log10(0.05), color = "black",linetype="dashed",size = 0.35) +
  geom_point(aes(color = ele_selment), show.legend = FALSE) +
  scale_color_manual(values = rep(c("#85bce5","#fad76f","#857bbb","#8b9071","#e85fa2","#ef9b6a","#e4aed0","#b0e3c9","#a96976"),times=2)) +
  scale_x_continuous(breaks = elementBreak$x, labels = elementBreak$Group.1, expand = c(0, 0)) +
  scale_y_continuous(breaks = c(0,0.5,1.0,-log10(0.05),1.5,2.0,2.5), labels = c("0.0","0.5","1.0","","1.5","2.0","2.5")) +
  theme_bw()+
  theme(panel.grid = element_blank(), axis.line = element_line(colour = 'black'), panel.background = element_rect(fill = 'transparent')) +
  labs(x = '', y = expression(''~-log[10]~'(FDR-corrected P)'))+
  geom_text_repel(data = subset(dat7,sig == "sig"), aes(label = outcome), size = 5,box.padding = unit(0.3, 'lines'), segment.color = "black", show.legend = FALSE, color = "black")
ggsave("element_bacteria.pdf",width = 15,height = 10)


# associations between serum elements and alpha diversity measurements --------

r_24w_alpha_al <- as.data.frame(matrix(NA,ncol = 6))
names(r_24w_alpha_al) <- c("ele_selment","alpha","beta","p","l95","u95")
for (i in 1:length(alpha)) {
  for (j in 1:length(ele_sel)) {
    m = glm(dat_all[,name_all[alpha[i]]] ~ log(dat_all[,name_all[ele_sel[j]]])+age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI,data = dat_all, family = gaussian())
    r_24w_alpha_al[(length(ele_sel)*(i-1)+j),1] = name_all[ele_sel[j]]
    r_24w_alpha_al[(length(ele_sel)*(i-1)+j),2] = name_all[alpha[i]]
    r_24w_alpha_al[(length(ele_sel)*(i-1)+j),3] = summary(m)$coefficients[2,1]
    r_24w_alpha_al[(length(ele_sel)*(i-1)+j),4] = summary(m)$coefficients[2,4]
    r_24w_alpha_al[(length(ele_sel)*(i-1)+j),5] = confint(m)[2,1]
    r_24w_alpha_al[(length(ele_sel)*(i-1)+j),6] = confint(m)[2,2]
  }
}

p <- as.data.frame(r_24w_alpha_al$p)
b <- as.data.frame(matrix(NA,ncol = 1))
names(b) <- "p"
m <- 4 #循环数
n <- 18 #每次循环内部次数
for (i in 1:m){
  j = n*(i-1)+1
  k = n*i
  part = p[c(j:k),]
  a = data.frame(p.adjust(part,method = "BH"))
  names(a) = "p"
  b = rbind(b,a)
}
b <- b[-1,]
r_24w_alpha_al$padj <- b

write.csv(r_24w_alpha_al,"24w_element_alpha_adjust.csv",quote=F,row.names = F)

# visualization Fig.5A ----------------------------------------------------

library(pheatmap)
dat8 <- read.xlsx("画图数据.xlsx",sheet = 8,rowNames = TRUE)
dat9 <- read.xlsx("画图数据.xlsx",sheet = 9,rowNames = TRUE)

colnames(dat8) <- c("Shannon's diversity","Observed features","Faith's PD","Pielou's evenness")
dat9 <- matrix(ifelse(
  dat9 < 0.05 & dat9 >= 0.01 ,"*",ifelse(
    dat9 < 0.01 & dat9 >= 0.001,"**",ifelse(
      dat9 < 0.001,"***",""
    ))),nrow(dat9))


pdf("element-alpha.pdf",width = 3,height = 11)
pheatmap(dat8,cluster_rows=F,cluster_cols=F,
         scale = "column",
         display_numbers=dat9,
         border="black",
         angle_col = 90,
         fontsize_number=30,
         fontsize_row = 20,
         fontsize_col = 20,
         color = c(colorRampPalette(colors = c("#0505ff","#9191ff","#c1c1ff","#dedeff","#eaeaff","#f0f0ff","white","white","white","white"))(500),colorRampPalette(colors = c("white","white","white","white","#fbf4f5","#ffecec","#ffd7d7","#ffc3c3","#ff8c8c","#ff0505"))(500)),
)
dev.off()

# correlations between serum elements and relative abundance of CAGs --------
library(psych)
dat_v2 <- dat_all[,c(ele_sel,225:229)]
dat_v2[,1:length(ele_sel)] <- log(dat_v2[,1:length(ele_sel)])
dat_v2[,(length(ele_sel)+1):dim(dat_v2)[2]] <- asin(sqrt(dat_v2[,(length(ele_sel)+1):dim(dat_v2)[2]]))
spear_pc <- corr.test(dat_v2,method = "spearman",adjust = "BH")
spear_pc_r <- spear_pc$r
spear_pc_p <- spear_pc$p
r <- spear_pc_r[(1:length(ele_sel)),c((length(ele_sel)+1):dim(spear_pc_r)[2])]
write.csv(r,"spear_pc_r.csv",quote = F,row.names = T)
p <- spear_pc_p[(1:length(ele_sel)),c((length(ele_sel)+1):dim(spear_pc_p)[2])]
write.csv(p,"spear_pc_p.csv",quote = F,row.names = T)

# visualization Fig.5C ----------------------------------------------------

dat10 <- read.xlsx("画图数据.xlsx",sheet = 10,rowNames = TRUE)
dat11 <- read.xlsx("画图数据.xlsx",sheet = 11,rowNames = TRUE)
dat10 <- t(dat10)
dat11 <- t(dat11)

dat11 <- matrix(ifelse(
  dat11 < 0.05 & dat11 >= 0.01 ,"*",ifelse(
    dat11 < 0.01 & dat11 >= 0.001,"**",ifelse(
      dat11 < 0.001,"***",""
    ))),nrow(dat11))


pdf("element-cag-Trans.pdf",width = 18,height = 5)
pheatmap(dat10,cluster_rows=F,cluster_cols=F,
         scale = "none",
         display_numbers=dat11,
         border="black",
         angle_col = 0,
         fontsize_number=40,
         fontsize_row = 30,
         fontsize_col = 30,
         color = c(colorRampPalette(colors = c("#5050ff","#9191ff","#c1c1ff","#dedeff","#eaeaff","#f0f0ff","white","white","white","white"))(1210),colorRampPalette(colors = c("white","white","white","white","#fbf4f5","#ffecec","#ffd7d7","#ffc3c3","#ff8c8c","#ff0505"))(1031))
)
dev.off()

