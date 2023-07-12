library(patchwork)
library(ape) 
library(vegan) 
library(ggsci)
library(ggplot2) 
library(openxlsx)
library(ggpubr)
library(openxlsx)

unwei_unif_dis <- read.xlsx("菌群数据.xlsx",sheet = 7,rowNames = T)
dat_all <- read.xlsx("合并数据.xlsx")
meta <- dat_all[,c("number2","SBP_traj_Group","DBP_traj_Group","MAP_traj_Group","PP_traj_Group")]
rownames(meta) <- meta$number2
colnames(meta) <- c("ID","km_sbp","km_dbp","km_map","km_pp")

def_beta_plot<-function(j){
  data<-unwei_unif_dis
  design <- meta[,c("ID",j)]
  idx <- rownames(design) %in% colnames(data) 
  sub_design <- data.frame(design[idx,])
  rownames(sub_design) <- rownames(design)[idx]
  data <- data[rownames(sub_design), rownames(sub_design)]   
  Group <-  factor(meta[,j])
  PCOA <- pcoa(data, correction="none", rn=NULL) 
  result <-PCOA$values[,"Relative_eig"]
  pro1 = as.numeric(sprintf("%.4f",result[1]))*100
  pro2 = as.numeric(sprintf("%.4f",result[2]))*100
  x = PCOA$vectors
  sample_names = rownames(x)
  pc = as.data.frame(PCOA$vectors)
  pc$names = sample_names
  legend_title = ""
  group = Group
  pc$group = group
  xlab=paste("PCoA1(",pro1,"%)",sep="") 
  ylab=paste("PCoA2(",pro2,"%)",sep="")
  
  p2<-  ggboxplot(pc, x="group", y="Axis.1", fill = "group", 
                  palette = "jco")+
    stat_compare_means(aes(group=group),label = "p.format")+
    labs(x=" ", y="Axis.1")+theme_bw()+
    theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),legend.position = "none")+
    #    scale_y_continuous(limits=c(-0.4, 0.4))+
    theme(axis.ticks.length = unit(0.4,"lines"), 
          axis.ticks = element_line(color='black'),
          axis.line = element_line(colour = "black"), 
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_text(colour='black',size=10,face = "plain"),
          axis.text.x=element_blank(),
          legend.position = "none")+
    scale_fill_manual(values = cbPalette)+coord_flip()
  
  p3<-ggboxplot(pc, x="group", y="Axis.2", fill = "group", 
                palette = "jco")+
    stat_compare_means(aes(group=group),label = "p.format")+
    labs(x=" ", y="Axis.2")+theme_bw()+
    theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),legend.position = "none")+
    scale_fill_manual(values = cbPalette)+
    #scale_y_continuous(limits=c(-0.4, 0.4))+
    theme(axis.ticks.length = unit(0.4,"lines"), 
          axis.ticks = element_line(color='black'),
          axis.line = element_line(colour = "black"), 
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x=element_text(colour='black',size=10,angle = 0,
                                   vjust = 1,hjust = 0.5,face = "plain"),
          axis.text.y=element_blank(),
          legend.position = "none")
  
  # cbPalette<-c("#4DBBD5FF","#E64B35FF")
  
  p1<-ggplot(pc,aes(Axis.1,Axis.2))+geom_point(size=2,aes(color=group))+
    labs(x=xlab,y=ylab,color=legend_title,shape=legend_title)+
    geom_hline(yintercept=0,linetype=4,color="grey")+geom_vline(xintercept=0,linetype=4,color="grey")+
    theme_bw()+
    stat_ellipse(aes(Axis.1,Axis.2,color=group))+#theme(legend.title=element_blank())+
    #  scale_x_continuous(limits=c(-0.5, 0.5))+scale_y_continuous(limits=c(-0.3, 0.3))+
    
    theme(panel.background = element_rect(fill='white', colour='black'),
          axis.title.x=element_text(colour='black', size=12),
          axis.title.y=element_text(colour='black', size=12),
          axis.text=element_text(colour='black',size=12),
          legend.title=element_blank(),
          legend.key.height=unit(0.6,"cm"),
          legend.position = c(0.75, 0.95),legend.direction = "horizontal")+
    scale_color_manual(values = cbPalette)
  
  set.seed(2023)
  otu.adonis=adonis(as.formula(paste0("data~",j)),data=sub_design)
  
  p4 <- ggplot(pc,
               aes(Axis.1, Axis.2))+
    geom_text(aes(x = -0.5,
                  y = 0.6,
                  label = paste("PERMANOVA:",
                                # otu.adonis$aov.tab$Df[1],#
                                "\nR2 = ",
                                round(otu.adonis$aov.tab$R2[1],3),
                                "\np-value = ",
                                otu.adonis$aov.tab$`Pr(>F)`[1],
                                sep = "")),size = 4) +theme_bw() +
    xlab(NULL) + ylab(NULL) +
    theme(panel.grid=element_blank(), 
          axis.title = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank())
  
  p2+p4+p1+p3 + 
    plot_layout(heights = c(1,4),widths = c(4,1),ncol = 2,nrow = 2)
  # dev.off()
  ggsave(file = paste0("Unwei","_",j,".pdf"),width = 8,height = 8)
}

cbPalette<-c("#f99f98", "#4dd2d6")
def_beta_plot("km_sbp")

cbPalette<-c("#dec800", "#8ca959")
def_beta_plot("km_dbp")

cbPalette<-c("#77b4bc", "#8d7299")
def_beta_plot("km_map")

cbPalette<-c("#f1ab6b", "#8492c4")
def_beta_plot("km_pp")
