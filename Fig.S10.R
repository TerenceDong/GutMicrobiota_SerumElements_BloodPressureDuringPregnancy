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

# associations between element and repetitive BP -------------------------------------
dat_all_sbp <- melt(dat_all[,c(1,2,6,10,ele_sel,bac_24w_cp,alpha,covariate)],
                    id.vars = name_all[c(1,ele_sel,bac_24w_cp,alpha,covariate)],
                    measure.vars = name_all[c(2,6,10)],
                    variable.name = "Time",
                    value.name = "sbp")
outcome <- "sbp"
response<- colnames(dat_all_sbp)[2:19]
result <- as.data.frame(matrix(NA,ncol = 6))
colnames(result) <- c("outcome","response","beta","u95","l95","p")
for (i in 1:length(outcome)){
  for(j in 1:length(response)){
    fit = lmer(sbp ~ log(dat_all_sbp[,response[j]]) +age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI+(1|number2),data=dat_all_sbp,REML=FALSE)
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
write.csv(result,"mixed_effects_element_sbp.csv",quote = F,row.names = F)

dat_all_dbp <- melt(dat_all[,c(1,3,7,11,ele_sel,bac_24w_cp,alpha,covariate)],
                    id.vars = name_all[c(1,ele_sel,bac_24w_cp,alpha,covariate)],
                    measure.vars = name_all[c(3,7,11)],
                    variable.name = "Time",
                    value.name = "dbp")
outcome <- "dbp"
response<- colnames(dat_all_dbp)[2:19]
result <- as.data.frame(matrix(NA,ncol = 6))
colnames(result) <- c("outcome","response","beta","u95","l95","p")
for (i in 1:length(outcome)){
  for(j in 1:length(response)){
    fit = lmer(dbp ~ log(dat_all_dbp[,response[j]]) +age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI+(1|number2),data=dat_all_dbp,REML=FALSE)
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
write.csv(result,"mixed_effects_element_dbp.csv",quote = F,row.names = F)

dat_all_pp <- melt(dat_all[,c(1,4,8,12,ele_sel,bac_24w_cp,alpha,covariate)],
                   id.vars = name_all[c(1,ele_sel,bac_24w_cp,alpha,covariate)],
                   measure.vars = name_all[c(4,8,12)],
                   variable.name = "Time",
                   value.name = "pp")
outcome <- "pp"
response<- colnames(dat_all_pp)[2:19]
result <- as.data.frame(matrix(NA,ncol = 6))
colnames(result) <- c("outcome","response","beta","u95","l95","p")
for (i in 1:length(outcome)){
  for(j in 1:length(response)){
    fit = lmer(pp ~ log(dat_all_pp[,response[j]]) +age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI+(1|number2),data=dat_all_pp,REML=FALSE)
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
write.csv(result,"mixed_effects_element_pp.csv",quote = F,row.names = F)

dat_all_map <- melt(dat_all[,c(1,5,9,13,ele_sel,bac_24w_cp,alpha,covariate)],
                    id.vars = name_all[c(1,ele_sel,bac_24w_cp,alpha,covariate)],
                    measure.vars = name_all[c(5,9,13)],
                    variable.name = "Time",
                    value.name = "map")
outcome <- "map"
response<- colnames(dat_all_map)[2:19]
result <- as.data.frame(matrix(NA,ncol = 6))
colnames(result) <- c("outcome","response","beta","u95","l95","p")
for (i in 1:length(outcome)){
  for(j in 1:length(response)){
    fit = lmer(map ~ log(dat_all_map[,response[j]]) +age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI+(1|number2),data=dat_all_map,REML=FALSE)
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
write.csv(result,"mixed_effects_element_map.csv",quote = F,row.names = F)

# visualization -----------------------------------------------------------

dat27 <- read.xlsx("画图数据.xlsx",sheet = 27)
dat27$response <- factor(dat27$response,levels = c("Cu","Zn","As","Mo","Hg","Tl","Fe","Sr","Ag","Sb","V","Co","Rb","Cs","Na","Mg","K","Ca"))
d27_1 <- subset(dat27,dat27$outcome=="sbp")
d27_2 <- subset(dat27,dat27$outcome=="dbp")
d27_3 <- subset(dat27,dat27$outcome=="pp")
d27_4 <- subset(dat27,dat27$outcome=="map")

library(ggplot2)
p <- ggplot(d27_1,aes(x=response,y=beta))+
  geom_point(size=2,position=position_dodge(width = 1),fill="#4dd2d6",color="#4dd2d6") +
  geom_errorbar(aes(ymax =u95, ymin = l95),color="#4dd2d6",size=1,
                width=0.2,position=position_dodge(width = 1))+
  geom_hline(aes(yintercept = 0),lty=2) +
  labs(x="",y="beta")+
  theme_bw() +
  theme(axis.text = element_text(color='black',size = 22),
        axis.title = element_text(color = "black",size = 22)) 
ggsave("ele_SBP_mixed.pdf",width = 12,height = 5)


p <- ggplot(d27_2,aes(x=response,y=beta))+
  geom_point(size=2,position=position_dodge(width = 1),fill="#8ca959",color="#8ca959") +
  geom_errorbar(aes(ymax =u95, ymin = l95),color="#8ca959",size=1,
                width=0.2,position=position_dodge(width = 1))+
  geom_hline(aes(yintercept = 0),lty=2) +
  labs(x="",y="beta")+
  theme_bw() +
  theme(axis.text = element_text(color='black',size = 22),
        axis.title = element_text(color = "black",size = 22)) 
ggsave("ele_DBP_mixed.pdf",width = 12,height = 5)

p <- ggplot(d27_3,aes(x=response,y=beta))+
  geom_point(size=2,position=position_dodge(width = 1),fill="#8d7299",color="#8d7299") +
  geom_errorbar(aes(ymax =u95, ymin = l95),color="#8d7299",size=1,
                width=0.2,position=position_dodge(width = 1))+
  geom_hline(aes(yintercept = 0),lty=2) +
  labs(x="",y="beta")+
  theme_bw() +
  theme(axis.text = element_text(color='black',size = 22),
        axis.title = element_text(color = "black",size = 22)) 
ggsave("ele_PP_mixed.pdf",width = 12,height = 5)

p <- ggplot(d27_4,aes(x=response,y=beta))+
  geom_point(size=2,position=position_dodge(width = 1),fill="#8492c4",color="#8492c4") +
  geom_errorbar(aes(ymax =u95, ymin = l95),color="#8492c4",size=1,
                width=0.2,position=position_dodge(width = 1))+
  geom_hline(aes(yintercept = 0),lty=2) +
  labs(x="",y="beta")+
  theme_bw() +
  theme(axis.text = element_text(color='black',size = 22),
        axis.title = element_text(color = "black",size = 22)) 
ggsave("ele_MAP_mixed.pdf",width = 12,height = 5)







