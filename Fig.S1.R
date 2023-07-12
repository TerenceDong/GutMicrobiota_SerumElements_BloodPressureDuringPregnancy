library(openxlsx)
data <- read_excel("原始数据.xlsx",sheet = 1)
data$SBP_adhospi <- as.numeric(data$SBP_adhospi)
data$DBP_adhospi <- as.numeric(data$DBP_adhospi)
data$antibioticUse_3month[is.na(data$antibioticUse_3month)] <- "NA"
# N=1527 --> 1000
data <- subset(data,!is.na(data$Mn) & data$F24 ==1 )
# N=1000 --> 943
data <- subset(data,data$ART == 0 & data$twins == 0 & data$antibioticUse_3month != 2 & data$hypertension_treatment ==0 )
# N=943 --> 788
data <- subset(data,data$GDM_24 == 0 & data$GDM_32 == 0)
# N=788 --> 733
data <- subset(data,!is.na(data$SBP_24) & !is.na(data$SBP_32) & !is.na(data$SBP_adhospi))
