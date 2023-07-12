library(openxlsx)
library(ggplot2)
library(ggrepel)
library(pheatmap)
dat_all <- read.xlsx("合并数据.xlsx")
name_all <- names(dat_all)

bac <- c(126:224)
cag <- c(225:229)
alpha <- c(230:233)
p24w <- c(2:5)
p32w <- c(6:9)
pdel <- c(10:13)
pall <- c(2:13)
trajectory <- c(122:125)

# Fig.6A ------------------------------------------------------------------
# associations between alpha diversity measurements and BP levels
alpha_a1 <- as.data.frame(matrix(NA,ncol = 6))
names(alpha_a1) <- c("alphateria","outcome","beta","p","l95","u95")
for (i in 1:length(pall)) {
  for (j in 1:length(alpha)) {
    m = glm(dat_all[,name_all[pall[i]]] ~ dat_all[,name_all[alpha[j]]]+age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI,data = dat_all, family = gaussian())
    alpha_a1[(length(alpha)*(i-1)+j),1] = name_all[alpha[j]]
    alpha_a1[(length(alpha)*(i-1)+j),2] = name_all[pall[i]]
    alpha_a1[(length(alpha)*(i-1)+j),3] = summary(m)$coefficients[2,1]
    alpha_a1[(length(alpha)*(i-1)+j),4] = summary(m)$coefficients[2,4]
    alpha_a1[(length(alpha)*(i-1)+j),5] = confint(m)[2,1]
    alpha_a1[(length(alpha)*(i-1)+j),6] = confint(m)[2,2]
  }
}

alpha_a1$padj <- p.adjust(alpha_a1$p,method = "BH")
write.csv(alpha_a1,"alpha_bp_a1.csv",quote = F,row.names = F)

# visualization
dat15 <- read.xlsx("画图数据.xlsx",sheet = 15,rowNames = T)
dat16 <- read.xlsx("画图数据.xlsx",sheet = 16,rowNames = T)
dat15 <- t(dat15)
dat16 <- t(dat16)

dat16 <- matrix(ifelse(
  dat16 < 0.05 & dat16 >= 0.01 ,"*",ifelse(
    dat16 < 0.01 & dat16 >= 0.001,"**",ifelse(
      dat16 < 0.001,"***",""
    ))),nrow(dat16))

annotation <- data.frame(
  Indicators = c("T2","T2","T2","T2","T3","T3","T3","T3","Before delivery","Before delivery","Before delivery","Before delivery")
)
rownames(annotation) <- colnames(dat15)

ann_colors = list(
  Indicators = c("T2" = "#6dcd5a", "T3" = "#ca92d8", "Before delivery" = "#f9cd5a")
)

pdf("alpha-bpN.pdf",width = 15,height = 4.8)
pheatmap(dat15,cluster_rows=F,cluster_cols=F,
         scale = "none",
         display_numbers=dat16,
         border="black",
         angle_col = 45,
         fontsize_number=20,
         color = colorRampPalette(colors = c("#9191ff","#c1c1ff","#dedeff","#eaeaff","#f0f0ff"))(1000),
         annotation_col = annotation,
         annotation_colors = ann_colors)
dev.off()

# Fig.6B ------------------------------------------------------------------

# associations between alpha diversity measurements and BP trajectories
alpha_trajectory_a1 <- as.data.frame(matrix(NA,ncol = 6))
names(alpha_trajectory_a1) <- c("alpha","outcome","OR","p","l95","u95")
for (i in 1:length(trajectory)) {
  for (j in 1:length(alpha)) {
    m = glm(dat_all[,name_all[trajectory[i]]] ~ dat_all[,name_all[alpha[j]]]+age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI,data = dat_all, family = binomial())
    alpha_trajectory_a1[(length(alpha)*(i-1)+j),1] = name_all[alpha[j]]
    alpha_trajectory_a1[(length(alpha)*(i-1)+j),2] = name_all[trajectory[i]]
    alpha_trajectory_a1[(length(alpha)*(i-1)+j),3] = exp(summary(m)$coefficients[2,1])
    alpha_trajectory_a1[(length(alpha)*(i-1)+j),4] = summary(m)$coefficients[2,4]
    alpha_trajectory_a1[(length(alpha)*(i-1)+j),5] = exp(confint(m)[2,1])
    alpha_trajectory_a1[(length(alpha)*(i-1)+j),6] = exp(confint(m)[2,2])
  }
}
write.csv(alpha_trajectory_a1,"alpha_trajectory_a1.csv",quote = F,row.names = F)

# visualization
dat19 <- read.xlsx("画图数据.xlsx",sheet = 19)
dat19$group <- ifelse(dat19$OR > 1,"Pos","Neg")
dat19$OR <- log(dat19$OR)
dat19$alpha <- factor(dat19$alpha,levels = rev(c("shannon","observed","faith","evenness")))
dat19$outcome <- factor(dat19$outcome,levels = c("SBP Trajectory","DBP Trajectory","MAP Trajectory","PP Trajectory"))
p <- ggplot()+
  geom_point(data = dat19,aes(outcome,alpha,size=abs(OR),fill=p),color="#999999",shape=21)+
  geom_point(data = dat19[which(dat19$group=="Pos"),],aes(outcome,alpha,size=abs(OR),color=p),shape=16)+
  scale_fill_gradientn(colours = colorRampPalette(c("#9191ff","#c1c1ff","#dedeff","#eaeaff","#f0f0ff","#dfe1e0"))(100))+
  scale_color_gradientn(colours = colorRampPalette(c("#ff8c8c","#ffc3c3","#ffd7d7","#ffecec","#fbf4f5","#d9dbd9"))(100))+
  theme_bw()+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 45,hjust = 1,colour = "black"))+
  scale_size(range = c(20,30))+
  xlab("")+
  ylab("")+
  guides(size=guide_legend(title = "ln (OR)",order = 1),
         fill=guide_legend(title = expression("Negative \nassociation \nP value"),order = 2),
         col=guide_legend(title = expression("Positive \nassociation \nP value"),order = 3))
p
ggsave("alpha-bpTrajectory.pdf",width = 9,height = 5.32)

# Fig.6C------------------------------------------------------------------
# associations between bacterial relative abundance and BP levels
bac_a1 <- as.data.frame(matrix(NA,ncol = 6))
names(bac_a1) <- c("bacteria","outcome","beta","p","l95","u95")
for (i in 1:length(pall)) {
  for (j in 1:length(bac)) {
    m = glm(dat_all[,name_all[pall[i]]] ~ asin(sqrt(dat_all[,name_all[bac[j]]]))+age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI,data = dat_all, family = gaussian())
    bac_a1[(length(bac)*(i-1)+j),1] = name_all[bac[j]]
    bac_a1[(length(bac)*(i-1)+j),2] = name_all[pall[i]]
    bac_a1[(length(bac)*(i-1)+j),3] = summary(m)$coefficients[2,1]
    bac_a1[(length(bac)*(i-1)+j),4] = summary(m)$coefficients[2,4]
    bac_a1[(length(bac)*(i-1)+j),5] = confint(m)[2,1]
    bac_a1[(length(bac)*(i-1)+j),6] = confint(m)[2,2]
  }
}

p <- as.data.frame(bac_a1$p)
b <- as.data.frame(matrix(NA,ncol = 1))
names(b) <- "p"
m <- 12 #循环数
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
bac_a1$padj <- b
write.csv(bac_a1,"bac_bp_a1.csv",quote = F,row.names = F)

# associations between bacterial relative abundance and BP trajectory
bac_trajectory_a1 <- as.data.frame(matrix(NA,ncol = 6))
names(bac_trajectory_a1) <- c("bacteria","outcome","OR","p","l95","u95")
for (i in 1:length(trajectory)) {
  for (j in 1:length(bac)) {
    m = glm(dat_all[,name_all[trajectory[i]]] ~ asin(sqrt(dat_all[,name_all[bac[j]]]))+age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI,data = dat_all, family = binomial())
    bac_trajectory_a1[(length(bac)*(i-1)+j),1] = name_all[bac[j]]
    bac_trajectory_a1[(length(bac)*(i-1)+j),2] = name_all[trajectory[i]]
    bac_trajectory_a1[(length(bac)*(i-1)+j),3] = exp(summary(m)$coefficients[2,1])
    bac_trajectory_a1[(length(bac)*(i-1)+j),4] = summary(m)$coefficients[2,4]
    bac_trajectory_a1[(length(bac)*(i-1)+j),5] = exp(confint(m)[2,1])
    bac_trajectory_a1[(length(bac)*(i-1)+j),6] = exp(confint(m)[2,2])
  }
}

p <- as.data.frame(bac_trajectory_a1$p)
b <- as.data.frame(matrix(NA,ncol = 1))
names(b) <- "p"
m <- 4 #循环数
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
bac_trajectory_a1$padj <- b
write.csv(bac_trajectory_a1,"bac_bp_trajectory_a1.csv",quote = F,row.names = F)

# visualization
dat12 <- read.xlsx("画图数据.xlsx",sheet = 12)
dat12$outcome <- factor(dat12$outcome,levels = c("SBP_24","DBP_24","MAP_24","PP_24","SBP_32","DBP_32","MAP_32","PP_32","SBP_adhospi","DBP_adhospi","MAP_adhospi","PP_adhospi","SBP Trajectory","DBP Trajectory","MAP Trajectory","PP Trajectory"),
                        labels = c("SBP_T2","DBP_T2","MAP_T2","PP_T2","SBP_T3","DBP_T3","MAP_T3","PP_T3","SBP_Before delivery","DBP_Before delivery","MAP_Before delivery","PP_Before delivery","SBP Trajectory","DBP Trajectory","MAP Trajectory","PP Trajectory"))
dat12 <- dat12[order(dat12$outcome),]
dat12$position <- seq(1,nrow(dat12))
dat12$sig <- ifelse(dat12$padj <0.05,"sig","non-sig")
outcomeBreak <- aggregate(dat12$position, by = list(dat12$outcome), FUN = median)

p <- ggplot(dat12, aes(position, -log(padj, 10))) +
  geom_hline(yintercept = -log10(0.05), color = "black",linetype="dashed",size = 0.35) +
  geom_point(aes(color = outcome), show.legend = FALSE) +
  scale_color_manual(values = rep(c("#85bce5","#fad76f","#857bbb","#8b9071","#e85fa2","#ef9b6a","#e4aed0","#b0e3c9"),times=2)) +
  scale_x_continuous(breaks = outcomeBreak$x, labels = outcomeBreak$Group.1, expand = c(0, 0)) +
  scale_y_continuous(breaks = c(0,0.5,1.0,-log10(0.05),1.5,2.0,2.5), labels = c("0.0","0.5","1.0","","1.5","2.0","2.5")) +
  theme_bw()+
  theme(panel.grid = element_blank(), axis.line = element_line(colour = 'black'), panel.background = element_rect(fill = 'transparent'),axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1)) +
  labs(x = '', y = expression(''~-log[10]~'(FDR-corrected P)'))+
  geom_text_repel(data = subset(dat12,sig == "sig"), aes(label = bacteria), size = 5,box.padding = unit(0.3, 'lines'), segment.color = "black", show.legend = FALSE, color = "black")
ggsave("bacteria_bp.pdf",width = 15,height = 10)


# Fig.6E ------------------------------------------------------------------

# associations between relative abundance of CAG and BP levels
cag_a1 <- as.data.frame(matrix(NA,ncol = 6))
names(cag_a1) <- c("bacteria","outcome","beta","p","l95","u95")
for (i in 1:length(pall)) {
  for (j in 1:length(cag)) {
    m = glm(dat_all[,name_all[pall[i]]] ~ asin(sqrt(dat_all[,name_all[cag[j]]]))+age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI,data = dat_all, family = gaussian())
    cag_a1[(length(cag)*(i-1)+j),1] = name_all[cag[j]]
    cag_a1[(length(cag)*(i-1)+j),2] = name_all[pall[i]]
    cag_a1[(length(cag)*(i-1)+j),3] = summary(m)$coefficients[2,1]
    cag_a1[(length(cag)*(i-1)+j),4] = summary(m)$coefficients[2,4]
    cag_a1[(length(cag)*(i-1)+j),5] = confint(m)[2,1]
    cag_a1[(length(cag)*(i-1)+j),6] = confint(m)[2,2]
  }
}
cag_a1$padj <- p.adjust(cag_a1$p,method = "BH")
write.csv(cag_a1,"cag_bp_a1.csv",quote = F,row.names = F)

# visualization
dat13 <- read.xlsx("画图数据.xlsx",sheet = 13,rowNames = T)
dat14 <- read.xlsx("画图数据.xlsx",sheet = 14,rowNames = T)

dat14 <- matrix(ifelse(
  dat14 < 0.05 & dat14 >= 0.01 ,"*",ifelse(
    dat14 < 0.01 & dat14 >= 0.001,"**",ifelse(
      dat14 < 0.001,"***",""
    ))),nrow(dat14))

annotation <- data.frame(
  Indicators = c("T2","T2","T2","T2","T3","T3","T3","T3","Before delivery","Before delivery","Before delivery","Before delivery")
)
rownames(annotation) <- rownames(dat13)

ann_colors = list(
  Indicators = c("T2" = "#6dcd5a", "T3" = "#ca92d8", "Before delivery" = "#f9cd5a")
)

pdf("cag-bpN.pdf",width = 5.5,height = 6.5)
pheatmap(dat13,cluster_rows=F,cluster_cols=F,
         scale = "none",
         display_numbers=dat14,
         border="black",
         angle_col = 90,
         fontsize_number=30,
         color = c(colorRampPalette(colors = c("#5050ff","#9191ff","#c1c1ff","#dedeff","#eaeaff","#f0f0ff","white","white","white","white"))(513),colorRampPalette(colors = c("white","white","white","white","#fbf4f5","#ffecec","#ffd7d7","#ffc3c3","#ff8c8c","#ff0505"))(492)),
         annotation_row = annotation,
         annotation_colors = ann_colors)
dev.off()

# Fig.6F ------------------------------------------------------------------

# associations between relative abundance of CAG and BP trajectory
cag_trajectory_a1 <- as.data.frame(matrix(NA,ncol = 6))
names(cag_trajectory_a1) <- c("bacteria","outcome","OR","p","l95","u95")
for (i in 1:length(trajectory)) {
  for (j in 1:length(cag)) {
    m = glm(dat_all[,name_all[trajectory[i]]] ~ asin(sqrt(dat_all[,name_all[cag[j]]]))+age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI,data = dat_all, family = binomial())
    cag_trajectory_a1[(length(cag)*(i-1)+j),1] = name_all[cag[j]]
    cag_trajectory_a1[(length(cag)*(i-1)+j),2] = name_all[trajectory[i]]
    cag_trajectory_a1[(length(cag)*(i-1)+j),3] = exp(summary(m)$coefficients[2,1])
    cag_trajectory_a1[(length(cag)*(i-1)+j),4] = summary(m)$coefficients[2,4]
    cag_trajectory_a1[(length(cag)*(i-1)+j),5] = exp(confint(m)[2,1])
    cag_trajectory_a1[(length(cag)*(i-1)+j),6] = exp(confint(m)[2,2])
  }
}
write.csv(cag_trajectory_a1,"cag_bp_trajectory_a1.csv",quote = F,row.names = F)

# visualization
dat17 <- read.xlsx("画图数据.xlsx",sheet = 17)
dat17$group <- ifelse(dat17$OR > 1,"Pos","Neg")
dat17$OR <- log(dat17$OR)
dat17$cag <- factor(dat17$cag,levels = c("CAG1","CAG2","CAG3","CAG4","CAG5"))
dat17$outcome <- factor(dat17$outcome,levels = rev(c("SBP Trajectory","DBP Trajectory","MAP Trajectory","PP Trajectory")))
p <- ggplot()+
  geom_point(data = dat17,aes(cag,outcome,size=abs(OR),fill=p),color="#999999",shape=21)+
  geom_point(data = dat17[which(dat17$group=="Pos"),],aes(cag,outcome,size=abs(OR),color=p),shape=16)+
  scale_fill_gradientn(colours = colorRampPalette(c("#9191ff","#c1c1ff","#dedeff","#eaeaff","#f0f0ff","#dfe1e0"))(100))+
  scale_color_gradientn(colours = colorRampPalette(c("#ff8c8c","#ffc3c3","#ffd7d7","#ffecec","#fbf4f5","#d9dbd9"))(100))+
  theme_bw()+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 45,hjust = 1,color = "black"),
        axis.text.y = element_text(color = "black"))+
  scale_size(range = c(15,25))+
  xlab("")+
  ylab("")+
  guides(size=guide_legend(title = "ln (OR)",order = 1),
         fill=guide_legend(title = expression("Negative \nassociation \nP value"),order = 2),
         col=guide_legend(title = expression("Positive \nassociation \nP value"),order = 3))
p
ggsave("cag-bpTrajectory.pdf",width = 7,height = 5)










