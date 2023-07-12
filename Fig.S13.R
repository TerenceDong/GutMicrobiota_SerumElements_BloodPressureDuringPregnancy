library(openxlsx)
dat_all <- read.xlsx("合并数据.xlsx")
library(reshape2)
library(lme4)
library(nlme)
name_all <- names(dat_all)
covariate <- c(14,16,18,21,24,29)
ele_sel <- c(69,70,71,72,74,75,76,77,78,79,81,83,84,85,89,90,91,92)
bac_24w_cp <- c(126:229)
alpha <- c(230:233)

# associations between bacterial relative abundance and repetitive BP -------------------------------------

dat_all_sbp <- melt(dat_all[,c(1,2,6,10,ele_sel,bac_24w_cp,alpha,covariate)],
                    id.vars = name_all[c(1,ele_sel,bac_24w_cp,alpha,covariate)],
                    measure.vars = name_all[c(2,6,10)],
                    variable.name = "Time",
                    value.name = "sbp")
response<- colnames(dat_all_sbp)[20:118]
outcome <- c("sbp")
result <- as.data.frame(matrix(NA,ncol = 6))
colnames(result) <- c("outcome","response","beta","u95","l95","p")
for (i in 1:length(outcome)){
  for(j in 1:length(response)){
    fit = lmer(sbp ~ asin(sqrt(dat_all_sbp[,response[j]]))+age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI+(1|number2),data=dat_all_sbp,REML=FALSE)
    m = summary(fit)
    n = confint(fit)
    result[(i-1)*length(response)+j,1] = outcome[i]
    result[(i-1)*length(response)+j,2] = response[j]
    result[(i-1)*length(response)+j,3] = m$coefficients[2,1]
    result[(i-1)*length(response)+j,4] = n[4,1]
    result[(i-1)*length(response)+j,5] = n[4,2]
    result[(i-1)*length(response)+j,6] = 2 * pt(abs(m$coefficients[,3]), df = df.residual(fit), lower.tail = FALSE)[[2]]
  }
}  
result$padj <- p.adjust(result$p,method = "BH")
write.csv(result,"mixed_effects_bacAbundance_sbp.csv",quote = F,row.names = F)

dat_all_dbp <- melt(dat_all[,c(1,3,7,11,ele_sel,bac_24w_cp,alpha,covariate)],
                    id.vars = name_all[c(1,ele_sel,bac_24w_cp,alpha,covariate)],
                    measure.vars = name_all[c(3,7,11)],
                    variable.name = "Time",
                    value.name = "dbp")
response<- colnames(dat_all_dbp)[20:118]
outcome <- c("dbp")
result <- as.data.frame(matrix(NA,ncol = 6))
colnames(result) <- c("outcome","response","beta","u95","l95","p")
for (i in 1:length(outcome)){
  for(j in 1:length(response)){
    fit = lmer(dbp ~ asin(sqrt(dat_all_dbp[,response[j]]))+age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI+(1|number2),data=dat_all_dbp,REML=FALSE)
    m = summary(fit)
    n = confint(fit)
    result[(i-1)*length(response)+j,1] = outcome[i]
    result[(i-1)*length(response)+j,2] = response[j]
    result[(i-1)*length(response)+j,3] = m$coefficients[2,1]
    result[(i-1)*length(response)+j,4] = n[4,1]
    result[(i-1)*length(response)+j,5] = n[4,2]
    result[(i-1)*length(response)+j,6] = 2 * pt(abs(m$coefficients[,3]), df = df.residual(fit), lower.tail = FALSE)[[2]]
  }
}  
result$padj <- p.adjust(result$p,method = "BH")
write.csv(result,"mixed_effects_bacAbundance_dbp.csv",quote = F,row.names = F)

dat_all_map <- melt(dat_all[,c(1,5,9,13,ele_sel,bac_24w_cp,alpha,covariate)],
                    id.vars = name_all[c(1,ele_sel,bac_24w_cp,alpha,covariate)],
                    measure.vars = name_all[c(5,9,13)],
                    variable.name = "Time",
                    value.name = "map")
response<- colnames(dat_all_map)[20:118]
outcome <- c("map")
result <- as.data.frame(matrix(NA,ncol = 6))
colnames(result) <- c("outcome","response","beta","u95","l95","p")
for (i in 1:length(outcome)){
  for(j in 1:length(response)){
    fit = lmer(map ~ asin(sqrt(dat_all_map[,response[j]]))+age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI+(1|number2),data=dat_all_map,REML=FALSE)
    m = summary(fit)
    n = confint(fit)
    result[(i-1)*length(response)+j,1] = outcome[i]
    result[(i-1)*length(response)+j,2] = response[j]
    result[(i-1)*length(response)+j,3] = m$coefficients[2,1]
    result[(i-1)*length(response)+j,4] = n[4,1]
    result[(i-1)*length(response)+j,5] = n[4,2]
    result[(i-1)*length(response)+j,6] = 2 * pt(abs(m$coefficients[,3]), df = df.residual(fit), lower.tail = FALSE)[[2]]
  }
}  
result$padj <- p.adjust(result$p,method = "BH")
write.csv(result,"mixed_effects_bacAbundance_map.csv",quote = F,row.names = F)

dat_all_pp <- melt(dat_all[,c(1,4,8,12,ele_sel,bac_24w_cp,alpha,covariate)],
                   id.vars = name_all[c(1,ele_sel,bac_24w_cp,alpha,covariate)],
                   measure.vars = name_all[c(4,8,12)],
                   variable.name = "Time",
                   value.name = "pp")
response<- colnames(dat_all_pp)[20:118]
outcome <- c("pp")
result <- as.data.frame(matrix(NA,ncol = 6))
colnames(result) <- c("outcome","response","beta","u95","l95","p")
for (i in 1:length(outcome)){
  for(j in 1:length(response)){
    fit = lmer(pp ~ asin(sqrt(dat_all_pp[,response[j]]))+age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI+(1|number2),data=dat_all_pp,REML=FALSE)
    m = summary(fit)
    n = confint(fit)
    result[(i-1)*length(response)+j,1] = outcome[i]
    result[(i-1)*length(response)+j,2] = response[j]
    result[(i-1)*length(response)+j,3] = m$coefficients[2,1]
    result[(i-1)*length(response)+j,4] = n[4,1]
    result[(i-1)*length(response)+j,5] = n[4,2]
    result[(i-1)*length(response)+j,6] = 2 * pt(abs(m$coefficients[,3]), df = df.residual(fit), lower.tail = FALSE)[[2]]
  }
}  
result$padj <- p.adjust(result$p,method = "BH")
write.csv(result,"mixed_effects_bacAbundance_pp.csv",quote = F,row.names = F)

# Visualization Fig.S13-----------------------------------------------------------

dat21 <- read.xlsx("画图数据.xlsx",sheet = 21,rowNames = TRUE)
dat22 <- read.xlsx("画图数据.xlsx",sheet = 22,rowNames = TRUE)

dat22 <- matrix(ifelse(
  dat22 < 0.05 & dat22 >= 0.01 ,"*",ifelse(
    dat22 < 0.01 & dat22 >= 0.001,"**",ifelse(
      dat22 < 0.001,"***",""
    ))),nrow(dat22))

library(pheatmap)
pdf("gutMicrobiota-BPLongitudinal.pdf",width = 7,height = 15)
pheatmap(dat21,cluster_rows=F,cluster_cols=F,
         scale = "none",
         display_numbers=dat22,
         border="black",
         angle_col = 0,
         fontsize_number=10,
         color = c(colorRampPalette(colors = c("#3372ab","#5a8bba","#759ec5","white"))(346),colorRampPalette(colors = c("white","#d28688","#c96c6e","#c25a5c","#b33133"))(673)),
         #annotation_col = annotation,
         #annotation_colors = ann_colors
         border_color = "black")
dev.off()

# Visualization Fig.S11B-----------------------------------------------------------
dat18 <- read.xlsx("画图数据.xlsx",sheet = 18)
dat18$group <- ifelse(dat18$beta > 0,"Pos","Neg")
dat18$beta <- dat18$beta
dat18$cag <- factor(dat18$cag,levels = c("CAG1","CAG2","CAG3","CAG4","CAG5"))
dat18$outcome <- factor(dat18$outcome,levels = rev(c("SBP","DBP","MAP","PP")))
p <- ggplot()+
  geom_point(data = dat18,aes(cag,outcome,size=abs(beta),fill=padj),color="#999999",shape=21)+
  geom_point(data = dat18[which(dat18$group=="Pos"),],aes(cag,outcome,size=abs(beta),color=p),shape=16)+
  scale_fill_gradientn(colours = colorRampPalette(c("#212c5f","#3366b1","#42b0e4","#7bc6ed","#dfe1e0"))(100))+
  scale_color_gradientn(colours = colorRampPalette(c("#f26666","#f49699","#facccc","#facccc","#d9dbd9"))(100))+
  theme_bw()+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 45,hjust = 1))+
  xlab("")+
  ylab("")+
  guides(size=guide_legend(title = "β",order = 1),
         fill=guide_legend(title = expression("Negative \nassociation \nP value"),order = 2),
         col=guide_legend(title = expression("Positive \nassociation \nP value"),order = 3))
ggsave("cag-bpTrajectoryMixed.pdf",width = 7,height = 5)














# associations between bacterial alpha diversity and repetitive BP -------------------------------------

outcome <- "sbp"
response<- colnames(dat_all_sbp)[124:127]
result <- as.data.frame(matrix(NA,ncol = 6))
colnames(result) <- c("outcome","response","beta","u95","l95","p")
for (i in 1:length(outcome)){
  for(j in 1:length(response)){
    fit = lmer(sbp ~ dat_all_sbp[,response[j]]+age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI+(1|number2),data=dat_all_sbp,REML=FALSE)
    m = summary(fit)
    n = confint(fit)
    result[(i-1)*length(response)+j,1] = outcome[i]
    result[(i-1)*length(response)+j,2] = response[j]
    result[(i-1)*length(response)+j,3] = m$coefficients[2,1]
    result[(i-1)*length(response)+j,4] = n[4,1]
    result[(i-1)*length(response)+j,5] = n[4,2]
    result[(i-1)*length(response)+j,6] = 2 * pt(abs(m$coefficients[,3]), df = df.residual(fit), lower.tail = FALSE)[[2]]
  }
}  
result$padj <- p.adjust(result$p,method = "BH")
write.csv(result,"mixed_effects_bcaAlpha_sbp.csv",quote = F,row.names = F)

outcome <- "dbp"
response<- colnames(dat_all_dbp)[124:127]
result <- as.data.frame(matrix(NA,ncol = 6))
colnames(result) <- c("outcome","response","beta","u95","l95","p")
for (i in 1:length(outcome)){
  for(j in 1:length(response)){
    fit = lmer(dbp ~ dat_all_dbp[,response[j]]+age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI+(1|number2),data=dat_all_dbp,REML=FALSE)
    m = summary(fit)
    n = confint(fit)
    result[(i-1)*length(response)+j,1] = outcome[i]
    result[(i-1)*length(response)+j,2] = response[j]
    result[(i-1)*length(response)+j,3] = m$coefficients[2,1]
    result[(i-1)*length(response)+j,4] = n[4,1]
    result[(i-1)*length(response)+j,5] = n[4,2]
    result[(i-1)*length(response)+j,6] = 2 * pt(abs(m$coefficients[,3]), df = df.residual(fit), lower.tail = FALSE)[[2]]
  }
}  
result$padj <- p.adjust(result$p,method = "BH")
write.csv(result,"mixed_effects_bcaAlpha_dbp.csv",quote = F,row.names = F)

outcome <- "pp"
response<- colnames(dat_all_pp)[124:127]
result <- as.data.frame(matrix(NA,ncol = 6))
colnames(result) <- c("outcome","response","beta","u95","l95","p")
for (i in 1:length(outcome)){
  for(j in 1:length(response)){
    fit = lmer(pp ~ dat_all_pp[,response[j]]+age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI+(1|number2),data=dat_all_pp,REML=FALSE)
    m = summary(fit)
    n = confint(fit)
    result[(i-1)*length(response)+j,1] = outcome[i]
    result[(i-1)*length(response)+j,2] = response[j]
    result[(i-1)*length(response)+j,3] = m$coefficients[2,1]
    result[(i-1)*length(response)+j,4] = n[4,1]
    result[(i-1)*length(response)+j,5] = n[4,2]
    result[(i-1)*length(response)+j,6] = 2 * pt(abs(m$coefficients[,3]), df = df.residual(fit), lower.tail = FALSE)[[2]]
  }
}  
result$padj <- p.adjust(result$p,method = "BH")
write.csv(result,"mixed_effects_bcaAlpha_pp.csv",quote = F,row.names = F)


outcome <- "map"
response<- colnames(dat_all_map)[124:127]
result <- as.data.frame(matrix(NA,ncol = 6))
colnames(result) <- c("outcome","response","beta","u95","l95","p")
for (i in 1:length(outcome)){
  for(j in 1:length(response)){
    fit = lmer(map ~ dat_all_map[,response[j]]+age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI+(1|number2),data=dat_all_map,REML=FALSE)
    m = summary(fit)
    n = confint(fit)
    result[(i-1)*length(response)+j,1] = outcome[i]
    result[(i-1)*length(response)+j,2] = response[j]
    result[(i-1)*length(response)+j,3] = m$coefficients[2,1]
    result[(i-1)*length(response)+j,4] = n[4,1]
    result[(i-1)*length(response)+j,5] = n[4,2]
    result[(i-1)*length(response)+j,6] = 2 * pt(abs(m$coefficients[,3]), df = df.residual(fit), lower.tail = FALSE)[[2]]
  }
}  
result$padj <- p.adjust(result$p,method = "BH")
write.csv(result,"mixed_effects_bcaAlpha_map.csv",quote = F,row.names = F)







