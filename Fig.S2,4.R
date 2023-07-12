library(openxlsx)
library(ggplot2)
library(bkmr)

dat_all <- read.xlsx("合并数据.xlsx")
name_all <- names(dat_all)
pall <- c(2:13)
ele_sel <- c(69,70,71,72,74,75,76,77,78,79,81,83,84,85,89,90,91,92)

outcome <- as.matrix(data.frame(dat_all[,name_all[pall]]))
exposure <- as.matrix(log(dat_all[,name_all[ele_sel]]))
X <- as.matrix(data.frame(dat_all[,c("age","education","income","smokHis","parity","pre_BMI")]))
dir.create("BKMR")

# SBP T2 ---------------------------------------------------------------
fitkm <- kmbayes(y=outcome[,1],Z=exposure,X=X,
            verbose = FALSE,varsel = TRUE,family="gaussian",iter = 1000,est.h = TRUE)
pip <-  ExtractPIPs(fitkm)
write.csv(pip,"BKMR/sbpT2pip.csv",quote = F,row.names = F)

pred.resp.univar <- PredictorResponseUnivar(fit = fitkm)
p <- ggplot(pred.resp.univar, aes(z, est, ymin = est - 1.96*se, ymax = est + 1.96*se)) + 
  geom_smooth(stat = "identity") + 
  facet_wrap(~ variable) +
  ylab("h(z)")
ggsave("BKMR/sbpT2.1.pdf",p,width = 9,height = 9)

pred.resp.bivar <- PredictorResponseBivar(fit = fitkm, min.plot.dist = 1)
pred.resp.bivar.levels <- PredictorResponseBivarLevels(pred.resp.df = pred.resp.bivar, Z = exposure, qs = c(0.1, 0.5, 0.9))
p <- ggplot(pred.resp.bivar.levels, aes(z1, est)) + 
  geom_smooth(aes(col = quantile), stat = "identity") + 
  facet_grid(variable2 ~ variable1) +
  ggtitle("h(expos1 | quantiles of expos2)") +
  xlab("expos1")
ggsave("BKMR/sbpT2.2.pdf",p,width = 20,height = 20)

risks.overall <- OverallRiskSummaries(fit = fitkm, y = outcome[,1], Z = exposure, X=X, 
                         qs = seq(0.25, 0.75, by = 0.05), 
                         q.fixed = 0.5, method = "exact")
risks.overall
write.csv(risks.overall,"BKMR/sbpT2risks.overall.csv",quote = F,row.names = F)
p <- ggplot(risks.overall, aes(quantile, est, ymin = est - 1.96*sd, ymax = est + 1.96*sd)) + 
  geom_pointrange()
ggsave("BKMR/sbpT2.3.pdf",p,width = 4,height = 4)

risks.singvar <- SingVarRiskSummaries(fit = fitkm, y = outcome[,1], Z = exposure, X=X, 
                         qs.diff = c(0.25, 0.75), 
                         q.fixed = c(0.25, 0.50, 0.75),
                         method = "exact")
risks.singvar
write.csv(risks.singvar,"BKMR/sbpT2risks.singvar.csv",quote = F,row.names = F)
p <- ggplot(risks.singvar, aes(variable, est, ymin = est - 1.96*sd, 
                               ymax = est + 1.96*sd, col = q.fixed)) + 
  geom_pointrange(position = position_dodge(width = 0.75)) + 
  coord_flip()
ggsave("BKMR/sbpT2.4.pdf",p,width = 9,height = 9)

risks.int <- SingVarIntSummaries(fit = fitkm, y = outcome[,1], Z = exposure, X=X, 
                        qs.diff = c(0.25, 0.75), 
                        qs.fixed = c(0.25, 0.75),
                        method = "exact")
risks.int
write.csv(risks.int,"BKMR/sbpT2risks.int.csv",quote = F,row.names = F)

# DBP T2 -----------------------------------------------------------------
fitkm <- kmbayes(y=outcome[,2],Z=exposure,X=X,
            verbose = FALSE,varsel = TRUE,family="gaussian",iter = 1000,est.h = TRUE)
pip <-  ExtractPIPs(fitkm)
write.csv(pip,"BKMR/dbpT2pip.csv",quote = F,row.names = F)

pred.resp.univar <- PredictorResponseUnivar(fit = fitkm)
p <- ggplot(pred.resp.univar, aes(z, est, ymin = est - 1.96*se, ymax = est + 1.96*se)) + 
  geom_smooth(stat = "identity") + 
  facet_wrap(~ variable) +
  ylab("h(z)")
ggsave("BKMR/dbpT2.1.pdf",p,width = 9,height = 9)

pred.resp.bivar <- PredictorResponseBivar(fit = fitkm, min.plot.dist = 1)
pred.resp.bivar.levels <- PredictorResponseBivarLevels(pred.resp.df = pred.resp.bivar, Z = exposure, qs = c(0.1, 0.5, 0.9))
p <- ggplot(pred.resp.bivar.levels, aes(z1, est)) + 
  geom_smooth(aes(col = quantile), stat = "identity") + 
  facet_grid(variable2 ~ variable1) +
  ggtitle("h(expos1 | quantiles of expos2)") +
  xlab("expos1")
ggsave("BKMR/dbpT2.2.pdf",p,width = 20,height = 20)

risks.overall <- OverallRiskSummaries(fit = fitkm, y = outcome[,2], Z = exposure, X=X, 
                         qs = seq(0.25, 0.75, by = 0.05), 
                         q.fixed = 0.5, method = "exact")
risks.overall
write.csv(risks.overall,"BKMR/dbpT2risks.overall.csv",quote = F,row.names = F)
p <- ggplot(risks.overall, aes(quantile, est, ymin = est - 1.96*sd, ymax = est + 1.96*sd)) + 
  geom_pointrange()
ggsave("BKMR/dbpT2.3.pdf",p,width = 4,height = 4)

risks.singvar <- SingVarRiskSummaries(fit = fitkm, y = outcome[,2], Z = exposure, X=X, 
                         qs.diff = c(0.25, 0.75), 
                         q.fixed = c(0.25, 0.50, 0.75),
                         method = "exact")
risks.singvar
write.csv(risks.singvar,"BKMR/dbpT2risks.singvar.csv",quote = F,row.names = F)
p <- ggplot(risks.singvar, aes(variable, est, ymin = est - 1.96*sd, 
                               ymax = est + 1.96*sd, col = q.fixed)) + 
  geom_pointrange(position = position_dodge(width = 0.75)) + 
  coord_flip()
ggsave("BKMR/dbpT2.4.pdf",p,width = 9,height = 9)

risks.int <- SingVarIntSummaries(fit = fitkm, y = outcome[,2], Z = exposure, X=X, 
                        qs.diff = c(0.25, 0.75), 
                        qs.fixed = c(0.25, 0.75),
                        method = "exact")
risks.int
write.csv(risks.int,"BKMR/dbpT2risks.int.csv",quote = F,row.names = F)

# PP T2 ----------------------------------------------------------------------
fitkm <- kmbayes(y=outcome[,3],Z=exposure,X=X,
            verbose = FALSE,varsel = TRUE,family="gaussian",iter = 1000,est.h = TRUE)
pip <-  ExtractPIPs(fitkm)
write.csv(pip,"BKMR/ppT2pip.csv",quote = F,row.names = F)

pred.resp.univar <- PredictorResponseUnivar(fit = fitkm)
p <- ggplot(pred.resp.univar, aes(z, est, ymin = est - 1.96*se, ymax = est + 1.96*se)) + 
  geom_smooth(stat = "identity") + 
  facet_wrap(~ variable) +
  ylab("h(z)")
ggsave("BKMR/ppT2.1.pdf",p,width = 9,height = 9)

pred.resp.bivar <-  PredictorResponseBivar(fit = fitkm, min.plot.dist = 1)
pred.resp.bivar.levels <- PredictorResponseBivarLevels(pred.resp.df = pred.resp.bivar, Z = exposure, qs = c(0.1, 0.5, 0.9))
p <- ggplot(pred.resp.bivar.levels, aes(z1, est)) + 
  geom_smooth(aes(col = quantile), stat = "identity") + 
  facet_grid(variable2 ~ variable1) +
  ggtitle("h(expos1 | quantiles of expos2)") +
  xlab("expos1")
ggsave("BKMR/ppT2.2.pdf",p,width = 20,height = 20)

risks.overall <- OverallRiskSummaries(fit = fitkm, y = outcome[,3], Z = exposure, X=X, 
                         qs = seq(0.25, 0.75, by = 0.05), 
                         q.fixed = 0.5, method = "exact")
risks.overall
write.csv(risks.overall,"BKMR/ppT2risks.overall.csv",quote = F,row.names = F)
p <- ggplot(risks.overall, aes(quantile, est, ymin = est - 1.96*sd, ymax = est + 1.96*sd)) + 
  geom_pointrange()
ggsave("BKMR/ppT2.3.pdf",p,width = 4,height = 4)

risks.singvar <- SingVarRiskSummaries(fit = fitkm, y = outcome[,3], Z = exposure, X=X, 
                         qs.diff = c(0.25, 0.75), 
                         q.fixed = c(0.25, 0.50, 0.75),
                         method = "exact")
risks.singvar
write.csv(risks.singvar,"BKMR/ppT2risks.singvar.csv",quote = F,row.names = F)
p <- ggplot(risks.singvar, aes(variable, est, ymin = est - 1.96*sd, 
                               ymax = est + 1.96*sd, col = q.fixed)) + 
  geom_pointrange(position = position_dodge(width = 0.75)) + 
  coord_flip()
ggsave("BKMR/ppT2.4.pdf",p,width = 9,height = 9)

risks.int <- SingVarIntSummaries(fit = fitkm, y = outcome[,3], Z = exposure, X=X, 
                        qs.diff = c(0.25, 0.75), 
                        qs.fixed = c(0.25, 0.75),
                        method = "exact")
risks.int
write.csv(risks.int,"BKMR/ppT2risks.int.csv",quote = F,row.names = F)

# MAP T2 ---------------------------------------------------------------------
fitkm <- kmbayes(y=outcome[,4],Z=exposure,X=X,
            verbose = FALSE,varsel = TRUE,family="gaussian",iter = 1000,est.h = TRUE)
pip <-  ExtractPIPs(fitkm)
write.csv(pip,"BKMR/mapT2pip.csv",quote = F,row.names = F)

pred.resp.univar <- PredictorResponseUnivar(fit = fitkm)
p <- ggplot(pred.resp.univar, aes(z, est, ymin = est - 1.96*se, ymax = est + 1.96*se)) + 
  geom_smooth(stat = "identity") + 
  facet_wrap(~ variable) +
  ylab("h(z)")
ggsave("BKMR/mapT2.1.pdf",p,width = 9,height = 9)

pred.resp.bivar <- PredictorResponseBivar(fit = fitkm, min.plot.dist = 1)
pred.resp.bivar.levels <- PredictorResponseBivarLevels(pred.resp.df = pred.resp.bivar, Z = exposure, qs = c(0.1, 0.5, 0.9))
p <- ggplot(pred.resp.bivar.levels, aes(z1, est)) + 
  geom_smooth(aes(col = quantile), stat = "identity") + 
  facet_grid(variable2 ~ variable1) +
  ggtitle("h(expos1 | quantiles of expos2)") +
  xlab("expos1")
ggsave("BKMR/mapT2.2.pdf",p,width = 20,height = 20)

risks.overall <- OverallRiskSummaries(fit = fitkm, y = outcome[,4], Z = exposure, X=X, 
                         qs = seq(0.25, 0.75, by = 0.05), 
                         q.fixed = 0.5, method = "exact")
risks.overall
write.csv(risks.overall,"BKMR/mapT2risks.overall.csv",quote = F,row.names = F)
p <- ggplot(risks.overall, aes(quantile, est, ymin = est - 1.96*sd, ymax = est + 1.96*sd)) + 
  geom_pointrange()
ggsave("BKMR/mapT2.3.pdf",p,width = 4,height = 4)

risks.singvar <- SingVarRiskSummaries(fit = fitkm, y = outcome[,4], Z = exposure, X=X, 
                         qs.diff = c(0.25, 0.75), 
                         q.fixed = c(0.25, 0.50, 0.75),
                         method = "exact")
risks.singvar
write.csv(risks.singvar,"BKMR/mapT2risks.singvar.csv",quote = F,row.names = F)
p <- ggplot(risks.singvar, aes(variable, est, ymin = est - 1.96*sd, 
                               ymax = est + 1.96*sd, col = q.fixed)) + 
  geom_pointrange(position = position_dodge(width = 0.75)) + 
  coord_flip()
ggsave("BKMR/mapT2.4.pdf",p,width = 9,height = 9)

risks.int <- SingVarIntSummaries(fit = fitkm, y = outcome[,4], Z = exposure, X=X, 
                        qs.diff = c(0.25, 0.75), 
                        qs.fixed = c(0.25, 0.75),
                        method = "exact")
risks.int
write.csv(risks.int,"BKMR/mapT2risks.int.csv",quote = F,row.names = F)

# SBP T3 ------------------------------------------------------------------------
fitkm <- kmbayes(y=outcome[,5],Z=exposure,X=X,
            verbose = FALSE,varsel = TRUE,family="gaussian",iter = 1000,est.h = TRUE)
pip <-  ExtractPIPs(fitkm)
write.csv(pip,"BKMR/sbpT3pip.csv",quote = F,row.names = F)

pred.resp.univar <- PredictorResponseUnivar(fit = fitkm)
p <- ggplot(pred.resp.univar, aes(z, est, ymin = est - 1.96*se, ymax = est + 1.96*se)) + 
  geom_smooth(stat = "identity") + 
  facet_wrap(~ variable) +
  ylab("h(z)")
ggsave("BKMR/sbpT3.1.pdf",p,width = 9,height = 9)

pred.resp.bivar <- PredictorResponseBivar(fit = fitkm, min.plot.dist = 1)
pred.resp.bivar.levels <- PredictorResponseBivarLevels(pred.resp.df = pred.resp.bivar, Z = exposure, qs = c(0.1, 0.5, 0.9))
p <- ggplot(pred.resp.bivar.levels, aes(z1, est)) + 
  geom_smooth(aes(col = quantile), stat = "identity") + 
  facet_grid(variable2 ~ variable1) +
  ggtitle("h(expos1 | quantiles of expos2)") +
  xlab("expos1")
ggsave("BKMR/sbpT3.2.pdf",p,width = 20,height = 20)

risks.overall <-OverallRiskSummaries(fit = fitkm, y = outcome[,5], Z = exposure, X=X, 
                         qs = seq(0.25, 0.75, by = 0.05), 
                         q.fixed = 0.5, method = "exact")
risks.overall
write.csv(risks.overall,"BKMR/sbpT3risks.overall.csv",quote = F,row.names = F)
p <- ggplot(risks.overall, aes(quantile, est, ymin = est - 1.96*sd, ymax = est + 1.96*sd)) + 
  geom_pointrange()
ggsave("BKMR/sbpT3.3.pdf",p,width = 4,height = 4)

risks.singvar <- SingVarRiskSummaries(fit = fitkm, y = outcome[,5], Z = exposure, X=X, 
                         qs.diff = c(0.25, 0.75), 
                         q.fixed = c(0.25, 0.50, 0.75),
                         method = "exact")
risks.singvar
write.csv(risks.singvar,"BKMR/sbpT3risks.singvar.csv",quote = F,row.names = F)
p <- ggplot(risks.singvar, aes(variable, est, ymin = est - 1.96*sd, 
                               ymax = est + 1.96*sd, col = q.fixed)) + 
  geom_pointrange(position = position_dodge(width = 0.75)) + 
  coord_flip()
ggsave("BKMR/sbpT3.4.pdf",p,width = 9,height = 9)

risks.int <- SingVarIntSummaries(fit = fitkm, y = outcome[,5], Z = exposure, X=X, 
                        qs.diff = c(0.25, 0.75), 
                        qs.fixed = c(0.25, 0.75),
                        method = "exact")
risks.int
write.csv(risks.int,"BKMR/sbpT3risks.int.csv",quote = F,row.names = F)


# DBP T3------------------------------------------------------------------------
fitkm <-kmbayes(y=outcome[,6],Z=exposure,X=X,
            verbose = FALSE,varsel = TRUE,family="gaussian",iter = 1000,est.h = TRUE)
pip <-  ExtractPIPs(fitkm)
write.csv(pip,"BKMR/dbpT3pip.csv",quote = F,row.names = F)

pred.resp.univar <- PredictorResponseUnivar(fit = fitkm)
p <- ggplot(pred.resp.univar, aes(z, est, ymin = est - 1.96*se, ymax = est + 1.96*se)) + 
  geom_smooth(stat = "identity") + 
  facet_wrap(~ variable) +
  ylab("h(z)")
ggsave("BKMR/dbpT3.1.pdf",p,width = 9,height = 9)

pred.resp.bivar <-PredictorResponseBivar(fit = fitkm, min.plot.dist = 1)
pred.resp.bivar.levels <- PredictorResponseBivarLevels(pred.resp.df = pred.resp.bivar, Z = exposure, qs = c(0.1, 0.5, 0.9))
p <- ggplot(pred.resp.bivar.levels, aes(z1, est)) + 
  geom_smooth(aes(col = quantile), stat = "identity") + 
  facet_grid(variable2 ~ variable1) +
  ggtitle("h(expos1 | quantiles of expos2)") +
  xlab("expos1")
ggsave("BKMR/dbpT3.2.pdf",p,width = 20,height = 20)

risks.overall <-OverallRiskSummaries(fit = fitkm, y = outcome[,6], Z = exposure, X=X, 
                         qs = seq(0.25, 0.75, by = 0.05), 
                         q.fixed = 0.5, method = "exact")
risks.overall
write.csv(risks.overall,"BKMR/dbpT3risks.overall.csv",quote = F,row.names = F)
p <- ggplot(risks.overall, aes(quantile, est, ymin = est - 1.96*sd, ymax = est + 1.96*sd)) + 
  geom_pointrange()
ggsave("BKMR/dbpT3.3.pdf",p,width = 4,height = 4)

risks.singvar <- SingVarRiskSummaries(fit = fitkm, y = outcome[,6], Z = exposure, X=X, 
                         qs.diff = c(0.25, 0.75), 
                         q.fixed = c(0.25, 0.50, 0.75),
                         method = "exact")
risks.singvar
write.csv(risks.singvar,"BKMR/dbpT3risks.singvar.csv",quote = F,row.names = F)
p <- ggplot(risks.singvar, aes(variable, est, ymin = est - 1.96*sd, 
                               ymax = est + 1.96*sd, col = q.fixed)) + 
  geom_pointrange(position = position_dodge(width = 0.75)) + 
  coord_flip()
ggsave("BKMR/dbpT3.4.pdf",p,width = 9,height = 9)

risks.int <- SingVarIntSummaries(fit = fitkm, y = outcome[,6], Z = exposure, X=X, 
                        qs.diff = c(0.25, 0.75), 
                        qs.fixed = c(0.25, 0.75),
                        method = "exact")
risks.int
write.csv(risks.int,"BKMR/dbpT3risks.int.csv",quote = F,row.names = F)

# PP T3------------------------------------------------------------------------
fitkm <-kmbayes(y=outcome[,7],Z=exposure,X=X,
            verbose = FALSE,varsel = TRUE,family="gaussian",iter = 1000,est.h = TRUE)
pip <-  ExtractPIPs(fitkm)
write.csv(pip,"BKMR/ppT3pip.csv",quote = F,row.names = F)

pred.resp.univar <- PredictorResponseUnivar(fit = fitkm)
p <- ggplot(pred.resp.univar, aes(z, est, ymin = est - 1.96*se, ymax = est + 1.96*se)) + 
  geom_smooth(stat = "identity") + 
  facet_wrap(~ variable) +
  ylab("h(z)")
ggsave("BKMR/ppT3.1.pdf",p,width = 9,height = 9)

pred.resp.bivar <-PredictorResponseBivar(fit = fitkm, min.plot.dist = 1)
pred.resp.bivar.levels <- PredictorResponseBivarLevels(pred.resp.df = pred.resp.bivar, Z = exposure, qs = c(0.1, 0.5, 0.9))
p <- ggplot(pred.resp.bivar.levels, aes(z1, est)) + 
  geom_smooth(aes(col = quantile), stat = "identity") + 
  facet_grid(variable2 ~ variable1) +
  ggtitle("h(expos1 | quantiles of expos2)") +
  xlab("expos1")
ggsave("BKMR/ppT3.2.pdf",p,width = 20,height = 20)

risks.overall <- OverallRiskSummaries(fit = fitkm, y = outcome[,7], Z = exposure, X=X, 
                         qs = seq(0.25, 0.75, by = 0.05), 
                         q.fixed = 0.5, method = "exact")
risks.overall
write.csv(risks.overall,"BKMR/ppT3risks.overall.csv",quote = F,row.names = F)
p <- ggplot(risks.overall, aes(quantile, est, ymin = est - 1.96*sd, ymax = est + 1.96*sd)) + 
  geom_pointrange()
ggsave("BKMR/ppT3.3.pdf",p,width = 4,height = 4)

risks.singvar <-SingVarRiskSummaries(fit = fitkm, y = outcome[,7], Z = exposure, X=X, 
                         qs.diff = c(0.25, 0.75), 
                         q.fixed = c(0.25, 0.50, 0.75),
                         method = "exact")
risks.singvar
write.csv(risks.singvar,"BKMR/ppT3risks.singvar.csv",quote = F,row.names = F)
p <- ggplot(risks.singvar, aes(variable, est, ymin = est - 1.96*sd, 
                               ymax = est + 1.96*sd, col = q.fixed)) + 
  geom_pointrange(position = position_dodge(width = 0.75)) + 
  coord_flip()
ggsave("BKMR/ppT3.4.pdf",p,width = 9,height = 9)

risks.int <- SingVarIntSummaries(fit = fitkm, y = outcome[,7], Z = exposure, X=X, 
                        qs.diff = c(0.25, 0.75), 
                        qs.fixed = c(0.25, 0.75),
                        method = "exact")
risks.int
write.csv(risks.int,"BKMR/ppT3risks.int.csv",quote = F,row.names = F)


# MAP T3------------------------------------------------------------------------
fitkm <- kmbayes(y=outcome[,8],Z=exposure,X=X,
            verbose = FALSE,varsel = TRUE,family="gaussian",iter = 1000,est.h = TRUE)
pip <-  ExtractPIPs(fitkm)
write.csv(pip,"BKMR/mapT3pip.csv",quote = F,row.names = F)

pred.resp.univar <- PredictorResponseUnivar(fit = fitkm)
p <- ggplot(pred.resp.univar, aes(z, est, ymin = est - 1.96*se, ymax = est + 1.96*se)) + 
  geom_smooth(stat = "identity") + 
  facet_wrap(~ variable) +
  ylab("h(z)")
ggsave("BKMR/mapT3.1.pdf",p,width = 9,height = 9)

pred.resp.bivar <-PredictorResponseBivar(fit = fitkm, min.plot.dist = 1)
pred.resp.bivar.levels <- PredictorResponseBivarLevels(pred.resp.df = pred.resp.bivar, Z = exposure, qs = c(0.1, 0.5, 0.9))
p <- ggplot(pred.resp.bivar.levels, aes(z1, est)) + 
  geom_smooth(aes(col = quantile), stat = "identity") + 
  facet_grid(variable2 ~ variable1) +
  ggtitle("h(expos1 | quantiles of expos2)") +
  xlab("expos1")
ggsave("BKMR/mapT3.2.pdf",p,width = 20,height = 20)

risks.overall <- OverallRiskSummaries(fit = fitkm, y = outcome[,8], Z = exposure, X=X, 
                         qs = seq(0.25, 0.75, by = 0.05), 
                         q.fixed = 0.5, method = "exact")
risks.overall
write.csv(risks.overall,"BKMR/mapT3risks.overall.csv",quote = F,row.names = F)
p <- ggplot(risks.overall, aes(quantile, est, ymin = est - 1.96*sd, ymax = est + 1.96*sd)) + 
  geom_pointrange()
ggsave("BKMR/mapT3.3.pdf",p,width = 4,height = 4)

risks.singvar <- SingVarRiskSummaries(fit = fitkm, y = outcome[,8], Z = exposure, X=X, 
                         qs.diff = c(0.25, 0.75), 
                         q.fixed = c(0.25, 0.50, 0.75),
                         method = "exact")
risks.singvar
write.csv(risks.singvar,"BKMR/mapT3risks.singvar.csv",quote = F,row.names = F)
p <- ggplot(risks.singvar, aes(variable, est, ymin = est - 1.96*sd, 
                               ymax = est + 1.96*sd, col = q.fixed)) + 
  geom_pointrange(position = position_dodge(width = 0.75)) + 
  coord_flip()
ggsave("BKMR/mapT3.4.pdf",p,width = 9,height = 9)

risks.int <- SingVarIntSummaries(fit = fitkm, y = outcome[,8], Z = exposure, X=X, 
                        qs.diff = c(0.25, 0.75), 
                        qs.fixed = c(0.25, 0.75),
                        method = "exact")
risks.int
write.csv(risks.int,"BKMR/mapT3risks.int.csv",quote = F,row.names = F)

# SBP Del------------------------------------------------------------------------
fitkm <-kmbayes(y=outcome[,9],Z=exposure,X=X,
            verbose = FALSE,varsel = TRUE,family="gaussian",iter = 1000,est.h = TRUE)
pip <-  ExtractPIPs(fitkm)
write.csv(pip,"BKMR/sbpDelpip.csv",quote = F,row.names = F)

pred.resp.univar <- PredictorResponseUnivar(fit = fitkm)
p <- ggplot(pred.resp.univar, aes(z, est, ymin = est - 1.96*se, ymax = est + 1.96*se)) + 
  geom_smooth(stat = "identity") + 
  facet_wrap(~ variable) +
  ylab("h(z)")
ggsave("BKMR/sbpDel.1.pdf",p,width = 9,height = 9)

pred.resp.bivar <-PredictorResponseBivar(fit = fitkm, min.plot.dist = 1)
pred.resp.bivar.levels <- PredictorResponseBivarLevels(pred.resp.df = pred.resp.bivar, Z = exposure, qs = c(0.1, 0.5, 0.9))
p <- ggplot(pred.resp.bivar.levels, aes(z1, est)) + 
  geom_smooth(aes(col = quantile), stat = "identity") + 
  facet_grid(variable2 ~ variable1) +
  ggtitle("h(expos1 | quantiles of expos2)") +
  xlab("expos1")
ggsave("BKMR/sbpDel.2.pdf",p,width = 20,height = 20)

risks.overall <-OverallRiskSummaries(fit = fitkm, y = outcome[,9], Z = exposure, X=X, 
                         qs = seq(0.25, 0.75, by = 0.05), 
                         q.fixed = 0.5, method = "exact")
risks.overall
write.csv(risks.overall,"BKMR/sbpDelrisks.overall.csv",quote = F,row.names = F)
p <- ggplot(risks.overall, aes(quantile, est, ymin = est - 1.96*sd, ymax = est + 1.96*sd)) + 
  geom_pointrange()
ggsave("BKMR/sbpDel.3.pdf",p,width = 4,height = 4)

risks.singvar <-SingVarRiskSummaries(fit = fitkm, y = outcome[,9], Z = exposure, X=X, 
                         qs.diff = c(0.25, 0.75), 
                         q.fixed = c(0.25, 0.50, 0.75),
                         method = "exact")
risks.singvar
write.csv(risks.singvar,"BKMR/sbpDelrisks.singvar.csv",quote = F,row.names = F)
p <- ggplot(risks.singvar, aes(variable, est, ymin = est - 1.96*sd, 
                               ymax = est + 1.96*sd, col = q.fixed)) + 
  geom_pointrange(position = position_dodge(width = 0.75)) + 
  coord_flip()
ggsave("BKMR/sbpDel.4.pdf",p,width = 9,height = 9)

risks.int <- SingVarIntSummaries(fit = fitkm, y = outcome[,9], Z = exposure, X=X, 
                        qs.diff = c(0.25, 0.75), 
                        qs.fixed = c(0.25, 0.75),
                        method = "exact")
risks.int
write.csv(risks.int,"BKMR/sbpDelrisks.int.csv",quote = F,row.names = F)


# DBP Del------------------------------------------------------------------------
fitkm <- kmbayes(y=outcome[,10],Z=exposure,X=X,
            verbose = FALSE,varsel = TRUE,family="gaussian",iter = 1000,est.h = TRUE)
pip <-  ExtractPIPs(fitkm)
write.csv(pip,"BKMR/dbpDelpip.csv",quote = F,row.names = F)

pred.resp.univar <- PredictorResponseUnivar(fit = fitkm)
p <- ggplot(pred.resp.univar, aes(z, est, ymin = est - 1.96*se, ymax = est + 1.96*se)) + 
  geom_smooth(stat = "identity") + 
  facet_wrap(~ variable) +
  ylab("h(z)")
ggsave("BKMR/dbpDel.1.pdf",p,width = 9,height = 9)

pred.resp.bivar <-PredictorResponseBivar(fit = fitkm, min.plot.dist = 1)
pred.resp.bivar.levels <- PredictorResponseBivarLevels(pred.resp.df = pred.resp.bivar, Z = exposure, qs = c(0.1, 0.5, 0.9))
p <- ggplot(pred.resp.bivar.levels, aes(z1, est)) + 
  geom_smooth(aes(col = quantile), stat = "identity") + 
  facet_grid(variable2 ~ variable1) +
  ggtitle("h(expos1 | quantiles of expos2)") +
  xlab("expos1")
ggsave("BKMR/dbpDel.2.pdf",p,width = 20,height = 20)

risks.overall <- OverallRiskSummaries(fit = fitkm, y = outcome[,10], Z = exposure, X=X, 
                         qs = seq(0.25, 0.75, by = 0.05), 
                         q.fixed = 0.5, method = "exact")
risks.overall
write.csv(risks.overall,"BKMR/dbpDelrisks.overall.csv",quote = F,row.names = F)
p <- ggplot(risks.overall, aes(quantile, est, ymin = est - 1.96*sd, ymax = est + 1.96*sd)) + 
  geom_pointrange()
ggsave("BKMR/dbpDel.3.pdf",p,width = 4,height = 4)

risks.singvar <- SingVarRiskSummaries(fit = fitkm, y = outcome[,10], Z = exposure, X=X, 
                         qs.diff = c(0.25, 0.75), 
                         q.fixed = c(0.25, 0.50, 0.75),
                         method = "exact")
risks.singvar
write.csv(risks.singvar,"BKMR/dbpDelrisks.singvar.csv",quote = F,row.names = F)
p <- ggplot(risks.singvar, aes(variable, est, ymin = est - 1.96*sd, 
                               ymax = est + 1.96*sd, col = q.fixed)) + 
  geom_pointrange(position = position_dodge(width = 0.75)) + 
  coord_flip()
ggsave("BKMR/dbpDel.4.pdf",p,width = 9,height = 9)

risks.int <- SingVarIntSummaries(fit = fitkm, y = outcome[,10], Z = exposure, X=X, 
                        qs.diff = c(0.25, 0.75), 
                        qs.fixed = c(0.25, 0.75),
                        method = "exact")
risks.int
write.csv(risks.int,"BKMR/dbpDelrisks.int.csv",quote = F,row.names = F)

# PP Del------------------------------------------------------------------------
fitkm <- kmbayes(y=outcome[,11],Z=exposure,X=X,
            verbose = FALSE,varsel = TRUE,family="gaussian",iter = 1000,est.h = TRUE)
pip <-  ExtractPIPs(fitkm)
write.csv(pip,"BKMR/ppDelpip.csv",quote = F,row.names = F)

pred.resp.univar <- PredictorResponseUnivar(fit = fitkm)
p <- ggplot(pred.resp.univar, aes(z, est, ymin = est - 1.96*se, ymax = est + 1.96*se)) + 
  geom_smooth(stat = "identity") + 
  facet_wrap(~ variable) +
  ylab("h(z)")
ggsave("BKMR/ppDel.1.pdf",p,width = 9,height = 9)

pred.resp.bivar <- PredictorResponseBivar(fit = fitkm, min.plot.dist = 1)
pred.resp.bivar.levels <- PredictorResponseBivarLevels(pred.resp.df = pred.resp.bivar, Z = exposure, qs = c(0.1, 0.5, 0.9))
p <- ggplot(pred.resp.bivar.levels, aes(z1, est)) + 
  geom_smooth(aes(col = quantile), stat = "identity") + 
  facet_grid(variable2 ~ variable1) +
  ggtitle("h(expos1 | quantiles of expos2)") +
  xlab("expos1")
ggsave("BKMR/ppDel.2.pdf",p,width = 20,height = 20)

risks.overall <- OverallRiskSummaries(fit = fitkm, y = outcome[,11], Z = exposure, X=X, 
                         qs = seq(0.25, 0.75, by = 0.05), 
                         q.fixed = 0.5, method = "exact")
risks.overall
write.csv(risks.overall,"BKMR/ppDelrisks.overall.csv",quote = F,row.names = F)
p <- ggplot(risks.overall, aes(quantile, est, ymin = est - 1.96*sd, ymax = est + 1.96*sd)) + 
  geom_pointrange()
ggsave("BKMR/ppDel.3.pdf",p,width = 4,height = 4)

risks.singvar <- SingVarRiskSummaries(fit = fitkm, y = outcome[,11], Z = exposure, X=X, 
                         qs.diff = c(0.25, 0.75), 
                         q.fixed = c(0.25, 0.50, 0.75),
                         method = "exact")
risks.singvar
write.csv(risks.singvar,"BKMR/ppDelrisks.singvar.csv",quote = F,row.names = F)
p <- ggplot(risks.singvar, aes(variable, est, ymin = est - 1.96*sd, 
                               ymax = est + 1.96*sd, col = q.fixed)) + 
  geom_pointrange(position = position_dodge(width = 0.75)) + 
  coord_flip()
ggsave("BKMR/ppDel.4.pdf",p,width = 9,height = 9)

risks.int <- SingVarIntSummaries(fit = fitkm, y = outcome[,11], Z = exposure, X=X, 
                        qs.diff = c(0.25, 0.75), 
                        qs.fixed = c(0.25, 0.75),
                        method = "exact")
risks.int
write.csv(risks.int,"BKMR/ppDelrisks.int.csv",quote = F,row.names = F)


# MAP Del------------------------------------------------------------------------
fitkm <- kmbayes(y=outcome[,12],Z=exposure,X=X,
            verbose = FALSE,varsel = TRUE,family="gaussian",iter = 1000,est.h = TRUE)
pip <-  ExtractPIPs(fitkm)
write.csv(pip,"BKMR/mapDelpip.csv",quote = F,row.names = F)

pred.resp.univar <- PredictorResponseUnivar(fit = fitkm)
p <- ggplot(pred.resp.univar, aes(z, est, ymin = est - 1.96*se, ymax = est + 1.96*se)) + 
  geom_smooth(stat = "identity") + 
  facet_wrap(~ variable) +
  ylab("h(z)")
ggsave("BKMR/mapDel.1.pdf",p,width = 9,height = 9)

pred.resp.bivar <- PredictorResponseBivar(fit = fitkm, min.plot.dist = 1)
pred.resp.bivar.levels <- PredictorResponseBivarLevels(pred.resp.df = pred.resp.bivar, Z = exposure, qs = c(0.1, 0.5, 0.9))
p <- ggplot(pred.resp.bivar.levels, aes(z1, est)) + 
  geom_smooth(aes(col = quantile), stat = "identity") + 
  facet_grid(variable2 ~ variable1) +
  ggtitle("h(expos1 | quantiles of expos2)") +
  xlab("expos1")
ggsave("BKMR/mapDel.2.pdf",p,width = 20,height = 20)

risks.overall <-OverallRiskSummaries(fit = fitkm, y = outcome[,12], Z = exposure, X=X, 
                         qs = seq(0.25, 0.75, by = 0.05), 
                         q.fixed = 0.5, method = "exact")
risks.overall
write.csv(risks.overall,"BKMR/mapDelrisks.overall.csv",quote = F,row.names = F)
p <- ggplot(risks.overall, aes(quantile, est, ymin = est - 1.96*sd, ymax = est + 1.96*sd)) + 
  geom_pointrange()
ggsave("BKMR/mapDel.3.pdf",p,width = 4,height = 4)

risks.singvar <- SingVarRiskSummaries(fit = fitkm, y = outcome[,12], Z = exposure, X=X, 
                         qs.diff = c(0.25, 0.75), 
                         q.fixed = c(0.25, 0.50, 0.75),
                         method = "exact")
risks.singvar
write.csv(risks.singvar,"BKMR/mapDelrisks.singvar.csv",quote = F,row.names = F)
p <- ggplot(risks.singvar, aes(variable, est, ymin = est - 1.96*sd, 
                               ymax = est + 1.96*sd, col = q.fixed)) + 
  geom_pointrange(position = position_dodge(width = 0.75)) + 
  coord_flip()
ggsave("BKMR/mapDel.4.pdf",p,width = 9,height = 9)

risks.int <- SingVarIntSummaries(fit = fitkm, y = outcome[,12], Z = exposure, X=X, 
                        qs.diff = c(0.25, 0.75), 
                        qs.fixed = c(0.25, 0.75),
                        method = "exact")
risks.int
write.csv(risks.int,"BKMR/mapDelrisks.int.csv",quote = F,row.names = F)











