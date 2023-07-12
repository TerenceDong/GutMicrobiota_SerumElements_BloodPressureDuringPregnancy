library(openxlsx)
library(mediation)
dat_all <- read.xlsx("合并数据.xlsx")

# Tl-CAG2-DBP_24
m1 <- glm(asin(sqrt(CAG2)) ~ log(Tl)+age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI,data = dat_all, family = gaussian())
m2 <- glm(DBP_24 ~ log(Tl)+asin(sqrt(CAG2))+age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI,data = dat_all, family = gaussian())
set.seed(2023)
model <- mediate(m1,m2,sims = 1000,treat = "log(Tl)",mediator = "asin(sqrt(CAG2))")
summary(model)

m3 <- glm(DBP_24 ~ asin(sqrt(CAG2))+age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI,data = dat_all, family = gaussian())
summary(m1)
summary(m2)
summary(m3)

# Tl-CAG2-MAP_24
m1 <- glm(asin(sqrt(CAG2)) ~ log(Tl)+age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI,data = dat_all, family = gaussian())
m2 <- glm(MAP_24 ~ log(Tl)+asin(sqrt(CAG2))+age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI,data = dat_all, family = gaussian())
set.seed(2023)
model <- mediate(m1,m2,sims = 1000,treat = "log(Tl)",mediator = "asin(sqrt(CAG2))")
summary(model)

m3 <- glm(MAP_24 ~ asin(sqrt(CAG2))+age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI,data = dat_all, family = gaussian())
summary(m1)
summary(m2)
summary(m3)

# Tl-CAG2-SBP Trajectory
m1 <- glm(asin(sqrt(CAG2)) ~ log(Tl)+age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI,data = dat_all, family = gaussian())
m2 <- glm(SBP_traj_Group ~ log(Tl)+asin(sqrt(CAG2))+age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI,data = dat_all, family = binomial())
set.seed(2023)
model <- mediate(m1,m2,sims = 1000,treat = "log(Tl)",mediator = "asin(sqrt(CAG2))")
summary(model)

m3 <- glm(SBP_traj_Group ~ asin(sqrt(CAG2))+age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI,data = dat_all, family = binomial())
summary(m1)
summary(m2);exp(coef(m2))
summary(m3);exp(coef(m3))

# Tl-CAG2-DBP Trajectory
m1 <- glm(asin(sqrt(CAG2)) ~ log(Tl)+age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI,data = dat_all, family = gaussian())
m2 <- glm(DBP_traj_Group ~ log(Tl)+asin(sqrt(CAG2))+age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI,data = dat_all, family = binomial())
set.seed(2023)
model <- mediate(m1,m2,sims = 1000,treat = "log(Tl)",mediator = "asin(sqrt(CAG2))")
summary(model)

m3 <- glm(DBP_traj_Group ~ asin(sqrt(CAG2))+age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI,data = dat_all, family = binomial())
summary(m1)
summary(m2);exp(coef(m2))
summary(m3);exp(coef(m3))

# Tl-CAG2-MAP Trajectory
m1 <- glm(asin(sqrt(CAG2)) ~ log(Tl)+age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI,data = dat_all, family = gaussian())
m2 <- glm(MAP_traj_Group ~ log(Tl)+asin(sqrt(CAG2))+age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI,data = dat_all, family = binomial())
set.seed(2023)
model <- mediate(m1,m2,sims = 1000,treat = "log(Tl)",mediator = "asin(sqrt(CAG2))")
summary(model)

m3 <- glm(MAP_traj_Group ~ asin(sqrt(CAG2))+age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI,data = dat_all, family = binomial())
summary(m1)
summary(m2);exp(coef(m2))
summary(m3);exp(coef(m3))

# Rb-evenness-SBP_24
m1 <- glm(evenness ~ log(Rb)+age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI,data = dat_all, family = gaussian())
m2 <- glm(SBP_24 ~ log(Rb)+evenness+age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI,data = dat_all, family = gaussian())
set.seed(2023)
model <- mediate(m1,m2,sims = 1000,treat = "log(Rb)",mediator = "evenness")
summary(model)

m3 <- glm(SBP_24 ~ evenness+age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI,data = dat_all, family = gaussian())
summary(m1)
summary(m2)
summary(m3)

# Rb-evenness-DBP_24
m1 <- glm(evenness ~ log(Rb)+age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI,data = dat_all, family = gaussian())
m2 <- glm(DBP_24 ~ log(Rb)+evenness+age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI,data = dat_all, family = gaussian())
set.seed(2023)
model <- mediate(m1,m2,sims = 1000,treat = "log(Rb)",mediator = "evenness")
summary(model)

m3 <- glm(DBP_24 ~ evenness+age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI,data = dat_all, family = gaussian())
summary(m1)
summary(m2)
summary(m3)

# Rb-evenness-MAP_24
m1 <- glm(evenness ~ log(Rb)+age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI,data = dat_all, family = gaussian())
m2 <- glm(MAP_24 ~ log(Rb)+evenness+age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI,data = dat_all, family = gaussian())
set.seed(2023)
model <- mediate(m1,m2,sims = 1000,treat = "log(Rb)",mediator = "evenness")
summary(model)

m3 <- glm(MAP_24 ~ evenness+age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI,data = dat_all, family = gaussian())
summary(m1)
summary(m2)
summary(m3)

# Rb-evenness-SBP_32
m1 <- glm(evenness ~ log(Rb)+age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI,data = dat_all, family = gaussian())
m2 <- glm(SBP_32 ~ log(Rb)+evenness+age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI,data = dat_all, family = gaussian())
set.seed(2023)
model <- mediate(m1,m2,sims = 1000,treat = "log(Rb)",mediator = "evenness")
summary(model)

m3 <- glm(SBP_32 ~ evenness+age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI,data = dat_all, family = gaussian())
summary(m1)
summary(m2)
summary(m3)

# Rb-evenness-PP_32
m1 <- glm(evenness ~ log(Rb)+age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI,data = dat_all, family = gaussian())
m2 <- glm(PP_32 ~ log(Rb)+evenness+age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI,data = dat_all, family = gaussian())
set.seed(2023)
model <- mediate(m1,m2,sims = 1000,treat = "log(Rb)",mediator = "evenness")
summary(model)

m3 <- glm(PP_32 ~ evenness+age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI,data = dat_all, family = gaussian())
summary(m1)
summary(m2)
summary(m3)

# Rb-evenness-SBP Trajectory
m1 <- glm(evenness ~ log(Rb)+age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI,data = dat_all, family = gaussian())
m2 <- glm(SBP_traj_Group ~ log(Rb)+evenness+age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI,data = dat_all, family = binomial())
set.seed(2023)
model <- mediate(m1,m2,sims = 1000,treat = "log(Rb)",mediator = "evenness")
summary(model)

m3 <- glm(SBP_traj_Group ~ evenness+age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI,data = dat_all, family = binomial())
summary(m1)
summary(m2);exp(coefficients(m2))
summary(m3);exp(coefficients(m3))

# Rb-evenness-MAP_traj_Group
m1 <- glm(evenness ~ log(Rb)+age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI,data = dat_all, family = gaussian())
m2 <- glm(MAP_traj_Group ~ log(Rb)+evenness+age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI,data = dat_all, family = binomial())
set.seed(2023)
model <- mediate(m1,m2,sims = 1000,treat = "log(Rb)",mediator = "evenness")
summary(model)

m3 <- glm(MAP_traj_Group ~ evenness+age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI,data = dat_all, family = binomial())
summary(m1)
summary(m2);exp(coefficients(m2))
summary(m3);exp(coefficients(m3))

# Rb-evenness-PP_traj_Group
m1 <- glm(evenness ~ log(Rb)+age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI,data = dat_all, family = gaussian())
m2 <- glm(PP_traj_Group ~ log(Rb)+evenness+age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI,data = dat_all, family = binomial())
set.seed(2023)
model <- mediate(m1,m2,sims = 1000,treat = "log(Rb)",mediator = "evenness")
summary(model)

m3 <- glm(PP_traj_Group ~ evenness+age+as.factor(education)+income+as.factor(smokHis)+as.factor(parity)+pre_BMI,data = dat_all, family = binomial())
summary(m1)
summary(m2);exp(coefficients(m2))
summary(m3);exp(coefficients(m3))
