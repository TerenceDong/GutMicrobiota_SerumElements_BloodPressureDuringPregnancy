library(openxlsx)
dat_all <- read.xlsx("合并数据.xlsx")
library(tableone)
var_cat <- c("education","smokHis","parity")
var_con <- c("SBP_24","DBP_24","PP_24","MBP_24","SBP_32","DBP_32","PP_32","MBP_32","SBP_adhospi","DBP_adhospi","PP_adhospi","MBP_adhospi","age","income","pre_BMI")
table_cat <- CreateCatTable(vars = var_cat,data = dat_all)
table_con <- CreateContTable(vars = var_con,data = dat_all)
table_cat <- print(table_cat,showAllLevels =T,quote = F,noSpaces=T,printoggle=F)
table_con <- print(table_con,showAllLevels =T,quote = F,noSpaces=T,printoggle=F)
write.csv(table_cat,"table_cat.csv")
write.csv(table_con,"table_con.csv")


