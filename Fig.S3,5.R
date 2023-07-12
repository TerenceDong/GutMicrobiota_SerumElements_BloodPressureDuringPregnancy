library(openxlsx)
library(ggplot2)
library(bkmr)

dat_all <- read.xlsx("合并数据.xlsx")
name_all <- names(dat_all)
ele_sel <- c(69,70,71,72,74,75,76,77,78,79,81,83,84,85,89,90,91,92)

outcome <- as.matrix(data.frame(dat_all[,c("f__Lachnospiraceae","g__Faecalibacterium","g__Bacteroides","g__Subdoligranulum")]))
outcome <- asin(sqrt(outcome))
exposure <- as.matrix(log(dat_all[,name_all[ele_sel]]))
X <- as.matrix(data.frame(dat_all[,c("age","education","income","smokHis","parity","pre_BMI")]))
dir.create("BKMR_bac")

# f__Lachnospiraceae------------------------------------------------------------
fitkm <-kmbayes(y=outcome[,1],Z=exposure,X=X,
            verbose = FALSE,varsel = TRUE,family="gaussian",iter = 1000,est.h = TRUE)
pip <-  ExtractPIPs(fitkm)
write.csv(pip,"BKMR_bac/f__Lachnospiraceae_pip.csv",quote = F,row.names = F)

pred.resp.univar <- PredictorResponseUnivar(fit = fitkm)
p <- ggplot(pred.resp.univar, aes(z, est, ymin = est - 1.96*se, ymax = est + 1.96*se)) + 
  geom_smooth(stat = "identity") + 
  facet_wrap(~ variable) +
  ylab("h(z)")
ggsave("BKMR_bac/f__Lachnospiraceae.1.pdf",p,width = 9,height = 9)

pred.resp.bivar <- PredictorResponseBivar(fit = fitkm, min.plot.dist = 1)
pred.resp.bivar.levels <- PredictorResponseBivarLevels(pred.resp.df = pred.resp.bivar, Z = exposure, qs = c(0.1, 0.5, 0.9))
p <- ggplot(pred.resp.bivar.levels, aes(z1, est)) + 
  geom_smooth(aes(col = quantile), stat = "identity") + 
  facet_grid(variable2 ~ variable1) +
  ggtitle("h(expos1 | quantiles of expos2)") +
  xlab("expos1")
ggsave("BKMR_bac/f__Lachnospiraceae.2.pdf",p,width = 20,height = 20)

risks.overall <- OverallRiskSummaries(fit = fitkm, y = outcome[,1], Z = exposure, X=X, 
                         qs = seq(0.25, 0.75, by = 0.05), 
                         q.fixed = 0.5, method = "exact")
risks.overall
write.csv(risks.overall,"BKMR_bac/f__Lachnospiraceae_risks.overall.csv",quote = F,row.names = F)
p <- ggplot(risks.overall, aes(quantile, est, ymin = est - 1.96*sd, ymax = est + 1.96*sd)) + 
  geom_pointrange()
ggsave("BKMR_bac/f__Lachnospiraceae.3.pdf",p,width = 4,height = 4)

risks.singvar <- SingVarRiskSummaries(fit = fitkm, y = outcome[,1], Z = exposure, X=X, 
                         qs.diff = c(0.25, 0.75), 
                         q.fixed = c(0.25, 0.50, 0.75),
                         method = "exact")
risks.singvar
write.csv(risks.singvar,"BKMR_bac/f__Lachnospiraceae.risks.singvar.csv",quote = F,row.names = F)
p <- ggplot(risks.singvar, aes(variable, est, ymin = est - 1.96*sd, 
                               ymax = est + 1.96*sd, col = q.fixed)) + 
  geom_pointrange(position = position_dodge(width = 0.75)) + 
  coord_flip()
ggsave("BKMR_bac/f__Lachnospiraceae.4.pdf",p,width = 9,height = 9)

risks.int <-SingVarIntSummaries(fit = fitkm, y = outcome[,1], Z = exposure, X=X, 
                        qs.diff = c(0.25, 0.75), 
                        qs.fixed = c(0.25, 0.75),
                        method = "exact")
risks.int
write.csv(risks.int,"BKMR_bac/f__Lachnospiraceae.risks.int.csv",quote = F,row.names = F)

# g__Faecalibacterium-------------------------------------------------------------
fitkm <-kmbayes(y=outcome[,2],Z=exposure,X=X,
            verbose = FALSE,varsel = TRUE,family="gaussian",iter = 1000,est.h = TRUE)
pip <-  ExtractPIPs(fitkm)
write.csv(pip,"BKMR_bac/g__Faecalibacterium_pip.csv",quote = F,row.names = F)

pred.resp.univar <- PredictorResponseUnivar(fit = fitkm)
p <- ggplot(pred.resp.univar, aes(z, est, ymin = est - 1.96*se, ymax = est + 1.96*se)) + 
  geom_smooth(stat = "identity") + 
  facet_wrap(~ variable) +
  ylab("h(z)")
ggsave("BKMR_bac/g__Faecalibacterium.1.pdf",p,width = 9,height = 9)

pred.resp.bivar <- PredictorResponseBivar(fit = fitkm, min.plot.dist = 1)
pred.resp.bivar.levels <- PredictorResponseBivarLevels(pred.resp.df = pred.resp.bivar, Z = exposure, qs = c(0.1, 0.5, 0.9))
p <- ggplot(pred.resp.bivar.levels, aes(z1, est)) + 
  geom_smooth(aes(col = quantile), stat = "identity") + 
  facet_grid(variable2 ~ variable1) +
  ggtitle("h(expos1 | quantiles of expos2)") +
  xlab("expos1")
ggsave("BKMR_bac/g__Faecalibacterium.2.pdf",p,width = 20,height = 20)

risks.overall <- OverallRiskSummaries(fit = fitkm, y = outcome[,2], Z = exposure, X=X, 
                         qs = seq(0.25, 0.75, by = 0.05), 
                         q.fixed = 0.5, method = "exact")
risks.overall
write.csv(risks.overall,"BKMR_bac/g__Faecalibacterium_risks.overall.csv",quote = F,row.names = F)
p <- ggplot(risks.overall, aes(quantile, est, ymin = est - 1.96*sd, ymax = est + 1.96*sd)) + 
  geom_pointrange()
ggsave("BKMR_bac/g__Faecalibacterium.3.pdf",p,width = 4,height = 4)

risks.singvar <- SingVarRiskSummaries(fit = fitkm, y = outcome[,2], Z = exposure, X=X, 
                         qs.diff = c(0.25, 0.75), 
                         q.fixed = c(0.25, 0.50, 0.75),
                         method = "exact")
risks.singvar
write.csv(risks.singvar,"BKMR_bac/g__Faecalibacterium.risks.singvar.csv",quote = F,row.names = F)
p <- ggplot(risks.singvar, aes(variable, est, ymin = est - 1.96*sd, 
                               ymax = est + 1.96*sd, col = q.fixed)) + 
  geom_pointrange(position = position_dodge(width = 0.75)) + 
  coord_flip()
ggsave("BKMR_bac/g__Faecalibacterium.4.pdf",p,width = 9,height = 9)

risks.int <- SingVarIntSummaries(fit = fitkm, y = outcome[,2], Z = exposure, X=X, 
                        qs.diff = c(0.25, 0.75), 
                        qs.fixed = c(0.25, 0.75),
                        method = "exact")
risks.int
write.csv(risks.int,"BKMR_bac/g__Faecalibacterium.risks.int.csv",quote = F,row.names = F)

# g__Bacteroides------------------------------------------------------------
fitkm <- kmbayes(y=outcome[,3],Z=exposure,X=X,
            verbose = FALSE,varsel = TRUE,family="gaussian",iter = 1000,est.h = TRUE)
pip <-  ExtractPIPs(fitkm)
write.csv(pip,"BKMR_bac/g__Bacteroides_pip.csv",quote = F,row.names = F)

pred.resp.univar <- PredictorResponseUnivar(fit = fitkm)
p <- ggplot(pred.resp.univar, aes(z, est, ymin = est - 1.96*se, ymax = est + 1.96*se)) + 
  geom_smooth(stat = "identity") + 
  facet_wrap(~ variable) +
  ylab("h(z)")
ggsave("BKMR_bac/g__Bacteroides.1.pdf",p,width = 9,height = 9)

pred.resp.bivar <- PredictorResponseBivar(fit = fitkm, min.plot.dist = 1)
pred.resp.bivar.levels <- PredictorResponseBivarLevels(pred.resp.df = pred.resp.bivar, Z = exposure, qs = c(0.1, 0.5, 0.9))
p <- ggplot(pred.resp.bivar.levels, aes(z1, est)) + 
  geom_smooth(aes(col = quantile), stat = "identity") + 
  facet_grid(variable2 ~ variable1) +
  ggtitle("h(expos1 | quantiles of expos2)") +
  xlab("expos1")
ggsave("BKMR_bac/g__Bacteroides.2.pdf",p,width = 20,height = 20)

risks.overall <- OverallRiskSummaries(fit = fitkm, y = outcome[,3], Z = exposure, X=X, 
                         qs = seq(0.25, 0.75, by = 0.05), 
                         q.fixed = 0.5, method = "exact")
risks.overall
write.csv(risks.overall,"BKMR_bac/g__Bacteroides_risks.overall.csv",quote = F,row.names = F)
p <- ggplot(risks.overall, aes(quantile, est, ymin = est - 1.96*sd, ymax = est + 1.96*sd)) + 
  geom_pointrange()
ggsave("BKMR_bac/g__Bacteroides.3.pdf",p,width = 4,height = 4)

risks.singvar <- SingVarRiskSummaries(fit = fitkm, y = outcome[,3], Z = exposure, X=X, 
                         qs.diff = c(0.25, 0.75), 
                         q.fixed = c(0.25, 0.50, 0.75),
                         method = "exact")
risks.singvar
write.csv(risks.singvar,"BKMR_bac/g__Bacteroides.risks.singvar.csv",quote = F,row.names = F)
p <- ggplot(risks.singvar, aes(variable, est, ymin = est - 1.96*sd, 
                               ymax = est + 1.96*sd, col = q.fixed)) + 
  geom_pointrange(position = position_dodge(width = 0.75)) + 
  coord_flip()
ggsave("BKMR_bac/g__Bacteroides.4.pdf",p,width = 9,height = 9)

risks.int <- SingVarIntSummaries(fit = fitkm, y = outcome[,3], Z = exposure, X=X, 
                        qs.diff = c(0.25, 0.75), 
                        qs.fixed = c(0.25, 0.75),
                        method = "exact")
risks.int
write.csv(risks.int,"BKMR_bac/g__Bacteroides.risks.int.csv",quote = F,row.names = F)
