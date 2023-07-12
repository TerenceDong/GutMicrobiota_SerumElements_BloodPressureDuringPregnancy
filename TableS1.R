library(openxlsx)
dat_all <- read.xlsx("合并数据.xlsx")
ele <- c(67:92)
ele_detection <- c(96:121)
# element distribution & correlation (<LOD was square root 2 transformed) --------------------------------------
# detection rate
distri_ele_detec <- as.data.frame(matrix(NA,ncol = 2))
names(distri_ele_detec) <- c("element","detection_rate")
for (i in 1:length(ele_detection)) {
  distri_ele_detec[i,1] <- names(dat_all)[ele_detection[i]]
  distri_ele_detec[i,2] <- tryCatch({prop.table(table(dat_all[,ele_detection[i]]))[[2]]},error=function(e){prop.table(table(dat_all[,ele_detection[i]]))[[1]]})
}
write.csv(distri_ele_detec,"distri_ele_detec.csv",quote = F,row.names = F)

#distrubition
distri_ele <- as.data.frame(matrix(NA,ncol = 10))
names(distri_ele) <- c("element","mean","GM","p2.5","p25","p50","p75","p97.5","min","max")
geo_mean <- function(data) {
  log_data <- log(data)
  gm <- exp(mean(log_data[is.finite(log_data)]))
  return(gm)
}
quar <- function(data){
  quartile <- quantile(data,probs = c(0.025,0.25,0.5,0.75,0.975))
  return(quartile)
}

# range_diff <- function(data){
#   ran <- diff(range(data))
#   return(ran)
# }

range_diff <- function(data){
  ran <- range(data)
  return(ran)
}

for (i in 1:length(ele)){
  distri_ele[i,1] <- names(dat_all)[ele[i]]
  distri_ele[i,2] <- mean(dat_all[,ele[i]])
  distri_ele[i,3] <- geo_mean(dat_all[,ele[i]])
  distri_ele[i,4] <- quantile(dat_all[,ele[i]],probs = 0.025)
  distri_ele[i,5] <- quantile(dat_all[,ele[i]],probs = 0.25)
  distri_ele[i,6] <- quantile(dat_all[,ele[i]],probs = 0.5)
  distri_ele[i,7] <- quantile(dat_all[,ele[i]],probs = 0.75)
  distri_ele[i,8] <- quantile(dat_all[,ele[i]],probs = 0.975)
  distri_ele[i,9] <- range_diff(dat_all[,ele[i]])[1]
  distri_ele[i,10] <- range_diff(dat_all[,ele[i]])[2]
}
write.csv(distri_ele,"distri_ele.csv",quote = F,row.names = F)

