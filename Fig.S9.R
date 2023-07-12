library(openxlsx)
library(ggplot2)
library(car)
library(vegan)

# 血压轨迹各时点箱线图-----------------------------------------------------
dat6 <- read.xlsx("画图数据.xlsx",sheet = 6)
dat6$SBP_traj_Group <- factor(dat6$SBP_traj_Group,levels = c(1,2))
dat6$DBP_traj_Group <- factor(dat6$DBP_traj_Group,levels = c(1,2))
dat6$MAP_traj_Group <- factor(dat6$MAP_traj_Group,levels = c(1,2))
dat6$PP_traj_Group <- factor(dat6$PP_traj_Group,levels = c(1,2))

#SBP
p <- ggplot(dat6,aes(SBP_traj_Group,SBP_24))+
  stat_boxplot(geom = "errorbar",aes(color=SBP_traj_Group),width=0.2)+
  geom_boxplot(aes(color=SBP_traj_Group),outlier.alpha = 0)+
  geom_jitter(aes(color=SBP_traj_Group,fill=SBP_traj_Group),width = 0.05,size=0.5)+
  scale_fill_manual(values=c("1"="#f99f98","2"="#4dd2d6"))+
  scale_color_manual(values=c("1"="#f99f98","2"="#4dd2d6"))+
  theme_bw()+
  labs(x="",y="")+
  scale_x_discrete(labels=c("Low","High"))
ggsave("SBP24 trajectory.pdf",width=5,height=7)

p <- ggplot(dat6,aes(SBP_traj_Group,SBP_32))+
  stat_boxplot(geom = "errorbar",aes(color=SBP_traj_Group),width=0.2)+
  geom_boxplot(aes(color=SBP_traj_Group),outlier.alpha = 0)+
  geom_jitter(aes(color=SBP_traj_Group,fill=SBP_traj_Group),width = 0.05,size=0.5)+
  scale_fill_manual(values=c("1"="#f99f98","2"="#4dd2d6"))+
  scale_color_manual(values=c("1"="#f99f98","2"="#4dd2d6"))+
  theme_bw()+
  labs(x="",y="")+
  scale_x_discrete(labels=c("Low","High"))
ggsave("SBP32 trajectory.pdf",width=5,height=7)

p <- ggplot(dat6,aes(SBP_traj_Group,SBP_adhospi))+
  stat_boxplot(geom = "errorbar",aes(color=SBP_traj_Group),width=0.2)+
  geom_boxplot(aes(color=SBP_traj_Group),outlier.alpha = 0)+
  geom_jitter(aes(color=SBP_traj_Group,fill=SBP_traj_Group),width = 0.05,size=0.5)+
  scale_fill_manual(values=c("1"="#f99f98","2"="#4dd2d6"))+
  scale_color_manual(values=c("1"="#f99f98","2"="#4dd2d6"))+
  theme_bw()+
  labs(x="",y="")+
  scale_x_discrete(labels=c("Low","High"))
ggsave("SBPadhospi trajectory.pdf",width=5,height=7)

#DBP
p <- ggplot(dat6,aes(DBP_traj_Group,DBP_24))+
  stat_boxplot(geom = "errorbar",aes(color=DBP_traj_Group),width=0.2)+
  geom_boxplot(aes(color=DBP_traj_Group),outlier.alpha = 0)+
  geom_jitter(aes(color=DBP_traj_Group,fill=DBP_traj_Group),width = 0.05,size=0.5)+
  scale_fill_manual(values=c("1"="#dec800","2"="#8ca959"))+
  scale_color_manual(values=c("1"="#dec800","2"="#8ca959"))+
  theme_bw()+
  labs(x="",y="")+
  scale_x_discrete(labels=c("Low","High"))
ggsave("DBP24 trajectory.pdf",width=5,height=7)

p <- ggplot(dat6,aes(DBP_traj_Group,DBP_32))+
  stat_boxplot(geom = "errorbar",aes(color=DBP_traj_Group),width=0.2)+
  geom_boxplot(aes(color=DBP_traj_Group),outlier.alpha = 0)+
  geom_jitter(aes(color=DBP_traj_Group,fill=DBP_traj_Group),width = 0.05,size=0.5)+
  scale_fill_manual(values=c("1"="#dec800","2"="#8ca959"))+
  scale_color_manual(values=c("1"="#dec800","2"="#8ca959"))+
  theme_bw()+
  labs(x="",y="")+
  scale_x_discrete(labels=c("Low","High"))
ggsave("DBP32 trajectory.pdf",width=5,height=7)

p <- ggplot(dat6,aes(DBP_traj_Group,DBP_adhospi))+
  stat_boxplot(geom = "errorbar",aes(color=DBP_traj_Group),width=0.2)+
  geom_boxplot(aes(color=DBP_traj_Group),outlier.alpha = 0)+
  geom_jitter(aes(color=DBP_traj_Group,fill=DBP_traj_Group),width = 0.05,size=0.5)+
  scale_fill_manual(values=c("1"="#dec800","2"="#8ca959"))+
  scale_color_manual(values=c("1"="#dec800","2"="#8ca959"))+
  theme_bw()+
  labs(x="",y="")+
  scale_x_discrete(labels=c("Low","High"))
ggsave("DBPadhospi trajectory.pdf",width=5,height=7)

#MAP
p <- ggplot(dat6,aes(MAP_traj_Group,MAP_24))+
  stat_boxplot(geom = "errorbar",aes(color=MAP_traj_Group),width=0.2)+
  geom_boxplot(aes(color=MAP_traj_Group),outlier.alpha = 0)+
  geom_jitter(aes(color=MAP_traj_Group,fill=MAP_traj_Group),width = 0.05,size=0.5)+
  scale_fill_manual(values=c("1"="#77b4bc","2"="#8d7299"))+
  scale_color_manual(values=c("1"="#77b4bc","2"="#8d7299"))+
  theme_bw()+
  labs(x="",y="")+
  scale_x_discrete(labels=c("Low","High"))
ggsave("MAP24 trajectory.pdf",width=5,height=7)

p <- ggplot(dat6,aes(MAP_traj_Group,MAP_32))+
  stat_boxplot(geom = "errorbar",aes(color=MAP_traj_Group),width=0.2)+
  geom_boxplot(aes(color=MAP_traj_Group),outlier.alpha = 0)+
  geom_jitter(aes(color=MAP_traj_Group,fill=MAP_traj_Group),width = 0.05,size=0.5)+
  scale_fill_manual(values=c("1"="#77b4bc","2"="#8d7299"))+
  scale_color_manual(values=c("1"="#77b4bc","2"="#8d7299"))+
  theme_bw()+
  labs(x="",y="")+
  scale_x_discrete(labels=c("Low","High"))
ggsave("MAP32 trajectory.pdf",width=5,height=7)

p <- ggplot(dat6,aes(MAP_traj_Group,MAP_adhospi))+
  stat_boxplot(geom = "errorbar",aes(color=MAP_traj_Group),width=0.2)+
  geom_boxplot(aes(color=MAP_traj_Group),outlier.alpha = 0)+
  geom_jitter(aes(color=MAP_traj_Group,fill=MAP_traj_Group),width = 0.05,size=0.5)+
  scale_fill_manual(values=c("1"="#77b4bc","2"="#8d7299"))+
  scale_color_manual(values=c("1"="#77b4bc","2"="#8d7299"))+
  theme_bw()+
  labs(x="",y="")+
  scale_x_discrete(labels=c("Low","High"))
ggsave("MAPadhospi trajectory.pdf",width=5,height=7)

#PP
p <- ggplot(dat6,aes(PP_traj_Group,PP_24))+
  stat_boxplot(geom = "errorbar",aes(color=PP_traj_Group),width=0.2)+
  geom_boxplot(aes(color=PP_traj_Group),outlier.alpha = 0)+
  geom_jitter(aes(color=PP_traj_Group,fill=PP_traj_Group),width = 0.05,size=0.5)+
  scale_fill_manual(values=c("1"="#f1ab6b","2"="#8492c4"))+
  scale_color_manual(values=c("1"="#f1ab6b","2"="#8492c4"))+
  theme_bw()+
  labs(x="",y="")+
  scale_x_discrete(labels=c("Low","High"))
ggsave("PP24 trajectory.pdf",width=5,height=7)

p <- ggplot(dat6,aes(PP_traj_Group,PP_32))+
  stat_boxplot(geom = "errorbar",aes(color=PP_traj_Group),width=0.2)+
  geom_boxplot(aes(color=PP_traj_Group),outlier.alpha = 0)+
  geom_jitter(aes(color=PP_traj_Group,fill=PP_traj_Group),width = 0.05,size=0.5)+
  scale_fill_manual(values=c("1"="#f1ab6b","2"="#8492c4"))+
  scale_color_manual(values=c("1"="#f1ab6b","2"="#8492c4"))+
  theme_bw()+
  labs(x="",y="")+
  scale_x_discrete(labels=c("Low","High"))
ggsave("PP32 trajectory.pdf",width=5,height=7)

p <- ggplot(dat6,aes(PP_traj_Group,PP_adhospi))+
  stat_boxplot(geom = "errorbar",aes(color=PP_traj_Group),width=0.2)+
  geom_boxplot(aes(color=PP_traj_Group),outlier.alpha = 0)+
  geom_jitter(aes(color=PP_traj_Group,fill=PP_traj_Group),width = 0.05,size=0.5)+
  scale_fill_manual(values=c("1"="#f1ab6b","2"="#8492c4"))+
  scale_color_manual(values=c("1"="#f1ab6b","2"="#8492c4"))+
  theme_bw()+
  labs(x="",y="")+
  scale_x_discrete(labels=c("Low","High"))
ggsave("PPadhospi trajectory.pdf",width=5,height=7)

# 组间比较 -------------------------------------------------------------------
library(car)
#SBP
shapiro.test(dat6$SBP_24[dat6$SBP_traj_Group == 1])
shapiro.test(dat6$SBP_24[dat6$SBP_traj_Group == 2])
leveneTest(dat6$SBP_24~as.factor(dat6$SBP_traj_Group),center=mean)
# wilcox.test(dat6$SBP_24[dat6$SBP_traj_Group == 1],dat6$SBP_24[dat6$SBP_traj_Group == 2])
t.test(dat6$SBP_24[dat6$SBP_traj_Group == 1],dat6$SBP_24[dat6$SBP_traj_Group == 2],var.equal = TRUE)

shapiro.test(dat6$SBP_32[dat6$SBP_traj_Group == 1])
shapiro.test(dat6$SBP_32[dat6$SBP_traj_Group == 2])
leveneTest(dat6$SBP_32~as.factor(dat6$SBP_traj_Group),center=mean)
# wilcox.test(dat6$SBP_32[dat6$SBP_traj_Group == 1],dat6$SBP_32[dat6$SBP_traj_Group == 2])
t.test(dat6$SBP_32[dat6$SBP_traj_Group == 1],dat6$SBP_32[dat6$SBP_traj_Group == 2],var.equal = TRUE)

shapiro.test(dat6$SBP_adhospi[dat6$SBP_traj_Group == 1])
shapiro.test(dat6$SBP_adhospi[dat6$SBP_traj_Group == 2])
leveneTest(dat6$SBP_adhospi~as.factor(dat6$SBP_traj_Group),center=mean)
# wilcox.test(dat6$SBP_adhospi[dat6$SBP_traj_Group == 1],dat6$SBP_adhospi[dat6$SBP_traj_Group == 2])
wilcox.test(dat6$SBP_adhospi[dat6$SBP_traj_Group == 1],dat6$SBP_adhospi[dat6$SBP_traj_Group == 2])

#DBP
shapiro.test(dat6$DBP_24[dat6$DBP_traj_Group == 1])
shapiro.test(dat6$DBP_24[dat6$DBP_traj_Group == 2])
leveneTest(dat6$DBP_24~as.factor(dat6$DBP_traj_Group),center=mean)
# wilcox.test(dat6$DBP_24[dat6$DBP_traj_Group == 1],dat6$DBP_24[dat6$DBP_traj_Group == 2])
wilcox.test(dat6$DBP_24[dat6$DBP_traj_Group == 1],dat6$DBP_24[dat6$DBP_traj_Group == 2])

shapiro.test(dat6$DBP_32[dat6$DBP_traj_Group == 1])
shapiro.test(dat6$DBP_32[dat6$DBP_traj_Group == 2])
leveneTest(dat6$DBP_32~as.factor(dat6$DBP_traj_Group),center=mean)
# wilcox.test(dat6$DBP_32[dat6$DBP_traj_Group == 1],dat6$DBP_32[dat6$DBP_traj_Group == 2])
wilcox.test(dat6$DBP_32[dat6$DBP_traj_Group == 1],dat6$DBP_32[dat6$DBP_traj_Group == 2])

shapiro.test(dat6$DBP_adhospi[dat6$DBP_traj_Group == 1])
shapiro.test(dat6$DBP_adhospi[dat6$DBP_traj_Group == 2])
leveneTest(dat6$DBP_adhospi~as.factor(dat6$DBP_traj_Group),center=mean)
# wilcox.test(dat6$DBP_adhospi[dat6$DBP_traj_Group == 1],dat6$DBP_adhospi[dat6$DBP_traj_Group == 2])
wilcox.test(dat6$DBP_adhospi[dat6$DBP_traj_Group == 1],dat6$DBP_adhospi[dat6$DBP_traj_Group == 2])

#MAP
shapiro.test(dat6$MAP_24[dat6$MAP_traj_Group == 1])
shapiro.test(dat6$MAP_24[dat6$MAP_traj_Group == 2])
leveneTest(dat6$MAP_24~as.factor(dat6$MAP_traj_Group),center=mean)
# wilcox.test(dat6$MAP_24[dat6$MAP_traj_Group == 1],dat6$MAP_24[dat6$MAP_traj_Group == 2])
t.test(dat6$MAP_24[dat6$MAP_traj_Group == 1],dat6$MAP_24[dat6$MAP_traj_Group == 2],var.equal = TRUE)

shapiro.test(dat6$MAP_32[dat6$MAP_traj_Group == 1])
shapiro.test(dat6$MAP_32[dat6$MAP_traj_Group == 2])
leveneTest(dat6$MAP_32~as.factor(dat6$MAP_traj_Group),center=mean)
# wilcox.test(dat6$MAP_32[dat6$MAP_traj_Group == 1],dat6$MAP_32[dat6$MAP_traj_Group == 2])
t.test(dat6$MAP_32[dat6$MAP_traj_Group == 1],dat6$MAP_32[dat6$MAP_traj_Group == 2],var.equal = TRUE)

shapiro.test(dat6$MAP_adhospi[dat6$MAP_traj_Group == 1])
shapiro.test(dat6$MAP_adhospi[dat6$MAP_traj_Group == 2])
leveneTest(dat6$MAP_adhospi~as.factor(dat6$MAP_traj_Group),center=mean)
# wilcox.test(dat6$MAP_adhospi[dat6$MAP_traj_Group == 1],dat6$MAP_adhospi[dat6$MAP_traj_Group == 2])
wilcox.test(dat6$MAP_adhospi[dat6$MAP_traj_Group == 1],dat6$MAP_adhospi[dat6$MAP_traj_Group == 2])

#PP
shapiro.test(dat6$PP_24[dat6$PP_traj_Group == 1])
shapiro.test(dat6$PP_24[dat6$PP_traj_Group == 2])
leveneTest(dat6$PP_24~as.factor(dat6$PP_traj_Group),center=mean)
# wilcox.test(dat6$PP_24[dat6$PP_traj_Group == 1],dat6$PP_24[dat6$PP_traj_Group == 2])
wilcox.test(dat6$PP_24[dat6$PP_traj_Group == 1],dat6$PP_24[dat6$PP_traj_Group == 2])

shapiro.test(dat6$PP_32[dat6$PP_traj_Group == 1])
shapiro.test(dat6$PP_32[dat6$PP_traj_Group == 2])
leveneTest(dat6$PP_32~as.factor(dat6$PP_traj_Group),center=mean)
# wilcox.test(dat6$PP_32[dat6$PP_traj_Group == 1],dat6$PP_32[dat6$PP_traj_Group == 2])
wilcox.test(dat6$PP_32[dat6$PP_traj_Group == 1],dat6$PP_32[dat6$PP_traj_Group == 2])

shapiro.test(dat6$PP_adhospi[dat6$PP_traj_Group == 1])
shapiro.test(dat6$PP_adhospi[dat6$PP_traj_Group == 2])
leveneTest(dat6$PP_adhospi~as.factor(dat6$PP_traj_Group),center=mean)
# wilcox.test(dat6$PP_adhospi[dat6$PP_traj_Group == 1],dat6$PP_adhospi[dat6$PP_traj_Group == 2])
wilcox.test(dat6$PP_adhospi[dat6$PP_traj_Group == 1],dat6$PP_adhospi[dat6$PP_traj_Group == 2])


# 血压模式总体Bray-Curtis距离---------------------------------------------------

dat6 <- read.xlsx("画图数据.xlsx",sheet = 6)
dat6$SBP_traj_Group <- factor(dat6$SBP_traj_Group,levels = c(1,2))
dat6$DBP_traj_Group <- factor(dat6$DBP_traj_Group,levels = c(1,2))
dat6$MAP_traj_Group <- factor(dat6$MAP_traj_Group,levels = c(1,2))
dat6$PP_traj_Group <- factor(dat6$PP_traj_Group,levels = c(1,2))

# SBP
sbp_dist <- vegdist(dat6[,c(2,6,10)], method="bray", binary=F)
sbp_pcoa <- cmdscale(sbp_dist, k=3, eig=T)
sbp_pcoa_points <- as.data.frame(sbp_pcoa$points)
sum_eig <- sum(sbp_pcoa$eig)
eig_percent <- round(sbp_pcoa$eig/sum_eig*100,1)
colnames(sbp_pcoa_points) <- paste0("PCoA", 1:3)
sbp_pcoa_result <- cbind(sbp_pcoa_points, dat6$SBP_traj_Group)

set.seed(2022)
group_permanova <- adonis(dat6[,c(2,6,10)]~dat6$SBP_traj_Group,data = dat6,permutations = 999)
a <- round(group_permanova$aov.tab$R2[1],digits = 3)
R2 <- paste("adonis R2: ",a, sep = "")
b <- group_permanova$aov.tab$`Pr(>F)`[1]
p_v <- paste("p: ",b, sep = "")
title <- paste(R2,"; ",p_v, sep = "")

p <- ggplot(sbp_pcoa_result, aes(x=PCoA1, y=PCoA2, color=dat6$SBP_traj_Group)) +
  geom_point(alpha=.7, size=2) + 
  labs(x=paste("PCoA 1 (", format(eig_percent[1], digits=4), "%)", sep=""),
       y=paste("PCoA 2 (", format(eig_percent[2], digits=4), "%)", sep=""),
       title=title)+
  stat_ellipse(level=0.95) +
  scale_color_manual(values=c("1"="#f99f98","2"="#4dd2d6"))+
  # geom_encircle(aes(fill=Group1), alpha = 0.1, show.legend = F) +
  theme_bw()
ggsave("sbp_bray_curtis.pdf", p, width = 9, height = 7)

# DBP
dbp_dist <- vegdist(dat6[,c(3,7,11)], method="bray", binary=F)
dbp_pcoa <- cmdscale(dbp_dist, k=3, eig=T)
dbp_pcoa_points <- as.data.frame(dbp_pcoa$points)
sum_eig <- sum(dbp_pcoa$eig)
eig_percent <- round(dbp_pcoa$eig/sum_eig*100,1)
colnames(dbp_pcoa_points) <- paste0("PCoA", 1:3)
dbp_pcoa_result <- cbind(dbp_pcoa_points, dat6$DBP_traj_Group)

set.seed(2022)
group_permanova <- adonis(dat6[,c(3,7,11)]~dat6$DBP_traj_Group,data = dat6,permutations = 999)
a <- round(group_permanova$aov.tab$R2[1],digits = 3)
R2 <- paste("adonis R2: ",a, sep = "")
b <- group_permanova$aov.tab$`Pr(>F)`[1]
p_v <- paste("p: ",b, sep = "")
title <- paste(R2,"; ",p_v, sep = "")

p <- ggplot(dbp_pcoa_result, aes(x=PCoA1, y=PCoA2, color=dat6$DBP_traj_Group)) +
  geom_point(alpha=.7, size=2) + 
  labs(x=paste("PCoA 1 (", format(eig_percent[1], digits=4), "%)", sep=""),
       y=paste("PCoA 2 (", format(eig_percent[2], digits=4), "%)", sep=""),
       title=title)+
  stat_ellipse(level=0.95) +
  scale_color_manual(values=c("1"="#dec800","2"="#8ca959"))+
  # geom_encircle(aes(fill=Group1), alpha = 0.1, show.legend = F) +
  theme_bw()
ggsave("dbp_bray_curtis.pdf", p, width = 9, height = 7)

# MAP
map_dist <- vegdist(dat6[,c(5,9,13)], method="bray", binary=F)
map_pcoa <- cmdscale(map_dist, k=3, eig=T)
map_pcoa_points <- as.data.frame(map_pcoa$points)
sum_eig <- sum(map_pcoa$eig)
eig_percent <- round(map_pcoa$eig/sum_eig*100,1)
colnames(map_pcoa_points) <- paste0("PCoA", 1:3)
map_pcoa_result <- cbind(map_pcoa_points, dat6$MAP_traj_Group)

set.seed(2022)
group_permanova <- adonis(dat6[,c(5,9,13)]~dat6$MAP_traj_Group,data = dat6,permutations = 999)
a <- round(group_permanova$aov.tab$R2[1],digits = 3)
R2 <- paste("adonis R2: ",a, sep = "")
b <- group_permanova$aov.tab$`Pr(>F)`[1]
p_v <- paste("p: ",b, sep = "")
title <- paste(R2,"; ",p_v, sep = "")

p <- ggplot(map_pcoa_result, aes(x=PCoA1, y=PCoA2, color=dat6$MAP_traj_Group)) +
  geom_point(alpha=.7, size=2) + 
  labs(x=paste("PCoA 1 (", format(eig_percent[1], digits=4), "%)", sep=""),
       y=paste("PCoA 2 (", format(eig_percent[2], digits=4), "%)", sep=""),
       title=title)+
  stat_ellipse(level=0.95) +
  scale_color_manual(values=c("1"="#77b4bc","2"="#8d7299"))+
  # geom_encircle(aes(fill=Group1), alpha = 0.1, show.legend = F) +
  theme_bw()
ggsave("map_bray_curtis.pdf", p, width = 9, height = 7)

# PP
pp_dist <- vegdist(dat6[,c(4,8,12)], method="bray", binary=F)
pp_pcoa <- cmdscale(pp_dist, k=3, eig=T)
pp_pcoa_points <- as.data.frame(pp_pcoa$points)
sum_eig <- sum(pp_pcoa$eig)
eig_percent <- round(pp_pcoa$eig/sum_eig*100,1)
colnames(pp_pcoa_points) <- paste0("PCoA", 1:3)
pp_pcoa_result <- cbind(pp_pcoa_points, dat6$PP_traj_Group)

set.seed(2022)
group_permanova <- adonis(dat6[,c(4,8,12)]~dat6$PP_traj_Group,data = dat6,permutations = 999)
a <- round(group_permanova$aov.tab$R2[1],digits = 3)
R2 <- paste("adonis R2: ",a, sep = "")
b <- group_permanova$aov.tab$`Pr(>F)`[1]
p_v <- paste("p: ",b, sep = "")
title <- paste(R2,"; ",p_v, sep = "")

p <- ggplot(pp_pcoa_result, aes(x=PCoA1, y=PCoA2, color=dat6$PP_traj_Group)) +
  geom_point(alpha=.7, size=2) + 
  labs(x=paste("PCoA 1 (", format(eig_percent[1], digits=4), "%)", sep=""),
       y=paste("PCoA 2 (", format(eig_percent[2], digits=4), "%)", sep=""),
       title=title)+
  stat_ellipse(level=0.95) +
  scale_color_manual(values=c("1"="#f1ab6b","2"="#8492c4"))+
  # geom_encircle(aes(fill=Group1), alpha = 0.1, show.legend = F) +
  theme_bw()
ggsave("pp_bray_curtis.pdf", p, width = 9, height = 7)
