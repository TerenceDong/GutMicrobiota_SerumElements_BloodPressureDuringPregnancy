# Sankey plot
library(ggalluvial)
library(ggplot2)
library(openxlsx)
dat27 <- read.xlsx("画图数据.xlsx",sheet = 30,rowNames = TRUE)
dat27$SBP_traj_Group <- factor(dat27$SBP_traj_Group,levels = c("High-falling","Low-rising"))
dat27$DBP_traj_Group <- factor(dat27$DBP_traj_Group,levels = c("High-rising","Low-rising"),labels = c("High","Low"))
dat27$PP_traj_Group <- factor(dat27$PP_traj_Group,levels = c("High-falling","Low-rising"))
dat27$MAP_traj_Group <- factor(dat27$MAP_traj_Group,levels = c("High-rising","Low-rising"),labels = c("High-f","Low-r"))

gg <- ggplot(dat27,
             aes(axis1 = SBP_traj_Group, axis2 = DBP_traj_Group, axis3 = MAP_traj_Group,axis4 = PP_traj_Group)) +
  #冲击流：
  geom_alluvium(aes(fill = as.factor(type)), width = 2/5, discern = FALSE) +
  #width，宽度所占图的比例，默认1/3
  #strate层：
  geom_stratum(width = 2/5, discern = FALSE) +
  #添加文本
  geom_text(stat = "stratum", discern = TRUE,size = 4,angle=0,
            aes(label = after_stat(stratum)))+
  labs(fill="")+
  theme_classic()+
  theme(axis.text = element_text(color="black",size=10),
        legend.text = element_text(color="black",size=10),
        legend.position = "none")+
  scale_fill_manual(values = c("#0b4d8b","#60a49f","#9b805c","#a884bc","#b1ba65","#84b6db","#c6789f","#c8914f","#79c07c","#cf6261"))+
  scale_x_continuous(breaks = 1:4,labels = c("SBP trajectory","DBP trajectory","MAP trajectory","PP trajectory")) #控制X轴上图标排序
gg
ggsave("Sankey.pdf",width = 12,height = 7)
