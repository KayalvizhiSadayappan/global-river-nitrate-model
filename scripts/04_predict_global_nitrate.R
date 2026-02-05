# ---------------------------------------------------------------
# Script: 04_predict_global_nitrate.R
# Purpose: Use the model ensemble to predict nitrate concentrations. 
#         We have provided catchment characteristics for a subset of 
#         global river networks as example input
# Author: Kayalvizhi Sadayappan
# Date: 2026-01-26
# Version: 1.0.0
#
# Inputs:
#   - Folder: data/processed/
#   - Files: BRT_input_global_subset.txt
#   - Source: The example input data is in the repository 
#             
# Outputs:
#   - Folder: results/
#   - Files: predicted_global_nitrate_subset.txt
#
# Dependencies:
#   - R packages: readr, xgboost, caret
# Runtime: The code ran on 1 CPU with 32 GB of memory and completed in approximately 1 minute
#         for sample subset. 
# ---------------------------------------------------------------
library(caret)
library(xgboost)
library(readr)

#select root folder (user specific)
root_folder <- "/global-river-nitrate-model/"

#set working directory
setwd(root_folder)

start_time <- Sys.time()

#load input data
load("models/trained_models.RData")
stream_char_cons <- read_delim(file="data/processed/BRT_input_global_subset.txt",
                            delim = "\t", escape_double = FALSE,trim_ws = TRUE)
model_runs <- 1000

#allocate memory to save predicted model results
stream_nit_df <- data.frame(matrix(NA,nrow=nrow(stream_char_cons),ncol=2+model_runs))
colnames(stream_nit_df) <- c("MERIT","COMID",paste0("model_",seq(1,model_runs,1)))
stream_nit_df$MERIT <- stream_char_cons$MERIT
stream_nit_df$COMID <- stream_char_cons$COMID

#predict results from all model runs
for(i in 1:model_runs){
  sel_input=predict(caret_models[[i]],stream_char_cons[,3:28])
  dglobal_nit <- xgb.DMatrix(data = data.matrix(sel_input))
  set.seed(i+model_runs)
  nit_pred=predict(model_ensemble[[i]],dglobal_nit)
  stream_nit_df[,i+2]=10^nit_pred
  print(i)
}

#get model ensemble averaged predicted nitrate concentrations and
#their standard deviation as measure of uncertainty
mean_nit_df=stream_nit_df[,1:2]
mean_nit_df$mean_nit=apply(stream_nit_df[,(1:model_runs+2)],1,mean)
mean_nit_df$sd_nit=apply(stream_nit_df[,(1:model_runs+2)],1,sd)

write.table(mean_nit_df,"results/predicted_global_nitrate_subset.txt",
            sep="\t",row.names=FALSE)

run_time <- Sys.time()-start_time
sprintf("Code ran in %02d:%02d:%02d", 
        floor(as.numeric(run_time, "secs")/3600),
        floor((as.numeric(run_time, "secs")%%3600)/60),
        round(as.numeric(run_time, "secs")%%60))