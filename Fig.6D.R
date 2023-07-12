library(Maaslin2)
input_data <- read.table("Trajectory.tsv",header = T,row.names = 1)
input_metadata <- read.table("metadata.tsv",header = T,row.names = 1)
dir.create("maAslin2Result2/")
maaslin_out<-function(i){
  Maaslin2(
    input_data, input_metadata, 
    paste("maAslin2Result2/",i,sep = ""),
    fixed_effects = c("age","education","income","smokHis","parity",
                      "pre_BMI",i),
    normalization = 'NONE',
    analysis_method = "LM",
    max_significance=0.2,
    transform= "AST",
    correction = "BH",
    standardize = FALSE,
    min_prevalence = 0.01,
    min_abundance = 0.0001)
}

for (i in c("SBP_traj_Group","DBP_traj_Group","MAP_traj_Group","PP_traj_Group")){
  maaslin_out(i)
}

# 关注文件夹下significant_results.tsv的结果


