# ---------------------------------------------------------------
# Script: 05_predict_global_nitrate.R
# Purpose: Use the model ensemble to predict nitrate concentrations. 
#         We have provided catchment characteristics for a subset of 
#         global river networks as example input
# Author: Kayalvizhi Sadayappan
# Date: 2026-08-07
# Version: 1.0.1
#
# Inputs:
#   - Folder: data/raaw/
#   - Files: BRT_input_global_subset.txt
#   - Source: The example input data is in the repository 
#             
# Outputs:
#   - Folder: results/
#   - Files: predicted_global_nitrate_subset.txt
#
# Dependencies:
#   - R packages: readr, xgboost, caret, matrixStats
# Runtime: The code ran on 4 CPU with 32 GB of memory each and completed in approximately 5 hours 10 minutes
# ---------------------------------------------------------------
library(caret)
library(xgboost)
library(readr)
library(matrixStats)

#select root folder (user specific)
root_folder <- "/global-river-nitrate-model"

#set working directory
setwd(root_folder)

start_time <- Sys.time()

#load input data
load("models/trained_models.RData")
stream_char_cons <- read_delim(file="data/raw/BRT_input_data_global.txt",
                            delim = "\t", escape_double = FALSE,trim_ws = TRUE)
model_runs <- 1000

#allocate memory to save predicted model results
stream_nit_df <- stream_char_cons[,c("COMID")]
stream_nit_df[paste0("model_", 1:model_runs)] <- NA_real_

#predict results from all model runs
for(i in 1:model_runs){
  sel_input=predict(caret_models[[i]],stream_char_cons[,3:28])
  dglobal_nit <- xgb.DMatrix(data = data.matrix(sel_input))
  set.seed(i+model_runs)
  nit_pred=predict(model_ensemble[[i]],dglobal_nit)
  stream_nit_df[paste0("model_", i)]=10^nit_pred
  print(i)
}

#extract values as matrix to reduce computation time for calcualting stastics over 1000 model runs
model_values <- as.matrix(stream_nit_df[paste0("model_", 1:model_runs)])

#get model ensemble averaged predicted nitrate concentrations and
#their standard deviation as measure of uncertainty
stream_nit_df_summary <- data.frame(
  COMID = stream_nit_df$COMID,
  mean_nit = rowMeans2(model_values),
  sd_nit = rowSds(model_values),
  cv_nit = NA_real_,
  p10_nit = rowQuantiles(model_values, probs = 0.10),
  p50_nit = rowQuantiles(model_values, probs = 0.50),
  p90_nit = rowQuantiles(model_values, probs = 0.90)
)
 
stream_nit_df_summary$cv_nit=stream_nit_df_summary$sd_nit*100/stream_nit_df_summary$mean_nit

write.table(stream_nit_df_summary,"results/predicted_nitrate_concentrations_global.txt",
            sep="\t",row.names=FALSE)

write.table(stream_nit_df,"results/predicted_nitrate_concentrations_global_ensemble.txt",
            sep="\t",row.names=FALSE)

run_time <- Sys.time()-start_time
sprintf("Code ran in %02d:%02d:%02d", 
        floor(as.numeric(run_time, "secs")/3600),
        floor((as.numeric(run_time, "secs")%%3600)/60),
        round(as.numeric(run_time, "secs")%%60))
