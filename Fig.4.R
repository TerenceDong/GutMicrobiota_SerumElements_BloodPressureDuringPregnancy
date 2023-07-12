
# Calculation to generate results for graphics ----------------------------

library(openxlsx)
dat_all <- read.xlsx("合并数据.xlsx")
p24w <- c(2:5)
p32w <- c(6:9)
pdel <- c(10:13)
trajectory <- c(122:125)
name_all <- names(dat_all)
ele_sel <- c(69,70,71,72,74,75,76,77,78,79,81,83,84,85,89,90,91,92)
ele_bp24w_a1 <- as.data.frame(matrix(NA,ncol = 6))
names(ele_bp24w_a1) <- c("element","outcome","beta","p","l95","u95")
for (i in 1:4) {
  for (j in 1:length(ele_sel)) {
    m = glm(dat_all[,name_all[p24w[i]]] ~ log(dat_all[,name_all[ele_sel[j]]])+age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI,data = dat_all, family = gaussian())
    ele_bp24w_a1[(length(ele_sel)*(i-1)+j),1] = name_all[ele_sel[j]]
    ele_bp24w_a1[(length(ele_sel)*(i-1)+j),2] = name_all[p24w[i]]
    ele_bp24w_a1[(length(ele_sel)*(i-1)+j),3] = summary(m)$coefficients[2,1]
    ele_bp24w_a1[(length(ele_sel)*(i-1)+j),4] = summary(m)$coefficients[2,4]
    ele_bp24w_a1[(length(ele_sel)*(i-1)+j),5] = confint(m)[2,1]
    ele_bp24w_a1[(length(ele_sel)*(i-1)+j),6] = confint(m)[2,2]
  }
}
p <- as.data.frame(ele_bp24w_a1$p)
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
ele_bp24w_a1$padj <- b
write.csv(ele_bp24w_a1,"ele_bp24w_a1.csv",quote = F,row.names = F)


ele_bp32w_a1 <- as.data.frame(matrix(NA,ncol = 6))
names(ele_bp32w_a1) <- c("element","outcome","beta","p","l95","u95")
for (i in 1:4) {
  for (j in 1:length(ele_sel)) {
    m = glm(dat_all[,name_all[p32w[i]]] ~ log(dat_all[,name_all[ele_sel[j]]])+age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI,data = dat_all, family = gaussian())
    ele_bp32w_a1[(length(ele_sel)*(i-1)+j),1] = name_all[ele_sel[j]]
    ele_bp32w_a1[(length(ele_sel)*(i-1)+j),2] = name_all[p32w[i]]
    ele_bp32w_a1[(length(ele_sel)*(i-1)+j),3] = summary(m)$coefficients[2,1]
    ele_bp32w_a1[(length(ele_sel)*(i-1)+j),4] = summary(m)$coefficients[2,4]
    ele_bp32w_a1[(length(ele_sel)*(i-1)+j),5] = confint(m)[2,1]
    ele_bp32w_a1[(length(ele_sel)*(i-1)+j),6] = confint(m)[2,2]
  }
}
p <- as.data.frame(ele_bp32w_a1$p)
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
ele_bp32w_a1$padj <- b
write.csv(ele_bp32w_a1,"ele_bp32w_a1.csv",quote = F,row.names = F)

ele_bp_delivery_a1 <- as.data.frame(matrix(NA,ncol = 6))
names(ele_bp_delivery_a1) <- c("element","outcome","beta","p","l95","u95")
for (i in 1:4) {
  for (j in 1:length(ele_sel)) {
    m = glm(dat_all[,name_all[pdel[i]]] ~ log(dat_all[,name_all[ele_sel[j]]])+age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI,data = dat_all, family = gaussian())
    ele_bp_delivery_a1[(length(ele_sel)*(i-1)+j),1] = name_all[ele_sel[j]]
    ele_bp_delivery_a1[(length(ele_sel)*(i-1)+j),2] = name_all[pdel[i]]
    ele_bp_delivery_a1[(length(ele_sel)*(i-1)+j),3] = summary(m)$coefficients[2,1]
    ele_bp_delivery_a1[(length(ele_sel)*(i-1)+j),4] = summary(m)$coefficients[2,4]
    ele_bp_delivery_a1[(length(ele_sel)*(i-1)+j),5] = confint(m)[2,1]
    ele_bp_delivery_a1[(length(ele_sel)*(i-1)+j),6] = confint(m)[2,2]
  }
}
p <- as.data.frame(ele_bp_delivery_a1$p)
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
ele_bp_delivery_a1$padj <- b
write.csv(ele_bp_delivery_a1,"ele_bp_delivery_a1.csv",quote = F,row.names = F)

# 轨迹
ele_bp_trajectory_a1 <- as.data.frame(matrix(NA,ncol = 6))
names(ele_bp_trajectory_a1) <- c("element","outcome","OR","p","l95","u95")
for (i in 1:4) {
  for (j in 1:length(ele_sel)) {
    m = glm(dat_all[,name_all[trajectory[i]]] ~ log(dat_all[,name_all[ele_sel[j]]])+age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI,data = dat_all, family = binomial())
    ele_bp_trajectory_a1[(length(ele_sel)*(i-1)+j),1] = name_all[ele_sel[j]]
    ele_bp_trajectory_a1[(length(ele_sel)*(i-1)+j),2] = name_all[trajectory[i]]
    ele_bp_trajectory_a1[(length(ele_sel)*(i-1)+j),3] = exp(summary(m)$coefficients[2,1])
    ele_bp_trajectory_a1[(length(ele_sel)*(i-1)+j),4] = summary(m)$coefficients[2,4]
    ele_bp_trajectory_a1[(length(ele_sel)*(i-1)+j),5] = exp(confint(m)[2,1])
    ele_bp_trajectory_a1[(length(ele_sel)*(i-1)+j),6] = exp(confint(m)[2,2])
  }
}
p <- as.data.frame(ele_bp_trajectory_a1$p)
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
ele_bp_trajectory_a1$padj <- b
write.csv(ele_bp_trajectory_a1,"ele_bp_trajectory_a1.csv",quote = F,row.names = F)

# Fig. 4A -----------------------------------------------------------------
library(pheatmap)
dat2 <- read.xlsx("画图数据.xlsx",sheet = 2,rowNames = TRUE)
dat3 <- read.xlsx("画图数据.xlsx",sheet = 3,rowNames = TRUE)

dat3 <- matrix(ifelse(
  dat3 < 0.05 & dat3 >= 0.01 ,"*",ifelse(
    dat3 < 0.01 & dat3 >= 0.001,"**",ifelse(
      dat3 < 0.001,"***",""
    ))),nrow(dat3))

annotation <- data.frame(
  Indicators = c("T2","T2","T2","T2","T3","T3","T3","T3","Before delivery","Before delivery","Before delivery","Before delivery")
)
rownames(annotation) <- colnames(dat2)

ann_colors = list(
  Indicators = c("T2" = "#6dcd5a", "T3" = "#ca92d8", "Before delivery" = "#f9cd5a")
)

pdf("element-bp.pdf",width = 9,height = 10)
pheatmap(dat2,cluster_rows=F,cluster_cols=F,
         scale = "row",
         display_numbers=dat3,
         border="black",
         angle_col = 45,
         fontsize_number=20,
         color = c(colorRampPalette(colors = c("#5050ff","#9191ff","#c1c1ff","#dedeff","#eaeaff","#f0f0ff","white","white","white","white"))(500),colorRampPalette(colors = c("white","white","white","white","#fbf4f5","#ffecec","#ffd7d7","#ffc3c3","#ff8c8c","#ff0505"))(500)),
         annotation_col = annotation,
         annotation_colors = ann_colors)
dev.off()

# Fig. 4B -----------------------------------------------------------------

dat26 <- read.xlsx("画图数据.xlsx",sheet = 26)
dat26$OR <- log(dat26$OR);dat26$l95 <- log(dat26$l95);dat26$u95 <- log(dat26$u95);
dat26$element <- factor(dat26$element,levels = c("Cu","Zn","As","Mo","Hg","Tl","Fe","Sr","Ag","Sb","V","Co","Rb","Cs","Na","Mg","K","Ca"))
d26_1 <- subset(dat26,dat26$outcome=="SBP_Trajectory")
d26_2 <- subset(dat26,dat26$outcome=="DBP_Trajectory")
d26_3 <- subset(dat26,dat26$outcome=="PP_Trajectory")
d26_4 <- subset(dat26,dat26$outcome=="MAP_Trajectory")

p <- ggplot(d26_1,aes(x=element,y=OR))+
  geom_point(size=2,position=position_dodge(width = 1),fill="#4dd2d6",color="#4dd2d6") +
  geom_errorbar(aes(ymax =u95, ymin = l95),color="#4dd2d6",size=1,
                width=0.2,position=position_dodge(width = 1))+
  geom_hline(aes(yintercept = 0),lty=2) +
  labs(x="",y="ln (OR)")+
  theme_bw() +
  theme(axis.text = element_text(color='black',size = 22),
        axis.title = element_text(color = "black",size = 22)) 
ggsave("ele_SBP_trajectory.pdf",width = 12,height = 5)

p <- ggplot(d26_2,aes(x=element,y=OR))+
  geom_point(size=2,position=position_dodge(width = 1),fill="#8ca959",color="#8ca959") +
  geom_errorbar(aes(ymax =u95, ymin = l95),color="#8ca959",size=1,
                width=0.2,position=position_dodge(width = 1))+
  geom_hline(aes(yintercept = 0),lty=2) +
  labs(x="",y="ln (OR)")+
  theme_bw() +
  theme(axis.text = element_text(color='black',size = 22),
        axis.title = element_text(color = "black",size = 22)) 
ggsave("ele_DBP_trajectory.pdf",width = 12,height = 5)

p <- ggplot(d26_3,aes(x=element,y=OR))+
  geom_point(size=2,position=position_dodge(width = 1),fill="#8d7299",color="#8d7299") +
  geom_errorbar(aes(ymax =u95, ymin = l95),color="#8d7299",size=1,
                width=0.2,position=position_dodge(width = 1))+
  geom_hline(aes(yintercept = 0),lty=2) +
  labs(x="",y="ln (OR)")+
  theme_bw() +
  theme(axis.text = element_text(color='black',size = 22),
        axis.title = element_text(color = "black",size = 22)) 
ggsave("ele_PP_trajectory.pdf",width = 12,height = 5)

p <- ggplot(d26_4,aes(x=element,y=OR))+
  geom_point(size=2,position=position_dodge(width = 1),fill="#8492c4",color="#8492c4") +
  geom_errorbar(aes(ymax =u95, ymin = l95),color="#8492c4",size=1,
                width=0.2,position=position_dodge(width = 1))+
  geom_hline(aes(yintercept = 0),lty=2) +
  labs(x="",y="ln (OR)")+
  theme_bw() +
  theme(axis.text = element_text(color='black',size = 22),
        axis.title = element_text(color = "black",size = 22)) 
ggsave("ele_MAP_trajectory.pdf",width = 12,height = 5)










