# ---------------------------------------------------------------
# Script: 03_train_BRT_model.R
# Purpose: Train the model to predict nitrate concentrations
# Author: Kayalvizhi Sadayappan
# Date: 2026-01-26
# Version: 1.0.0
#
# Inputs:
#   - Folder: data/processed/
#   - Files: BRT_train_data_7213_catchments.txt
#   - Source: The input data for training model is in the repository
#
# Outputs:
#   - Folder: results/
#   - Files: BRT_mean_importance.txt, BRT_model_performance.txt,
#   - Folder: models/
#   - Files: trained_models.RData (contains trained models)
#            
#
# Dependencies:
#   - R packages: dplyr, readr, hydroGOF, xgboost, caret
# Runtime: The code ran on 4 CPU each with 32 GB of memory and completed in approximately 43 minutes.
# ---------------------------------------------------------------
library(xgboost)
library(readr)
library(hydroGOF)
library(dplyr)
library(caret)

#select root folder and cores for parallel computing (user specific)
root_folder <- "/global-river-nitrate-model/"
ncores <- 4

#set working directory
setwd(root_folder)

start_time <- Sys.time()

#ensure results and models subfolder exist
if (!dir.exists("results")) { 
  dir.create("results", recursive = TRUE)
}
if (!dir.exists("models")) { 
  dir.create("models", recursive = TRUE)
}

#read in input data
nit_df <- read_delim(file="data/processed/BRT_train_data_7213_catchments.txt",
                  delim = "\t", escape_double = FALSE,trim_ws = TRUE)

#attributes to be used to train the model                  
sel_att <- c("forest_mean","shrub_mean","grass_mean","wetland_mean","crop_mean",
          "urban_mean","crop_nat_mean","ice_mean","barren_mean","water_mean",
          "MAP","MAT","porosity","perm_harm_m2perd","aqf_thick_m",
          "sand_mean","soc_mean","cec_mean","bdod_mean","nitrogen_mean",
          "mean_elev","mean_slope","stream_density_perkm","max_order",
          "order1_per","area_km2")
          
#prepare data for testing model
model_input <- nit_df[,sel_att];model_target <- log10(nit_df$mean_nit)

#calibrated model parameters
param_model <- list(booster = "gbtree", 
                objective = "reg:squarederror", 
                eval_metric="rmse",
                eta = 0.05,
                max_depth = 8,
                min_child_weight = 11,
                subsample = 0.6,
                colsample_bytree = 0.6,
                nthread = ncores)
nrounds_model <- 370
model_runs <- 1000

#allocate storage for model ensembles, importance, and model performance metrics
caret_models <- vector(mode="list",length = model_runs)
model_ensemble <- vector(mode="list",length = model_runs)
importance_matrix_list <- vector(mode="list",length = model_runs)
model_perf_df <- data.frame(model_run = seq(1,model_runs,1),train_NSE = NA,train_KGE = NA,train_RMSE = NA,train_PBIAS = NA,
    test_NSE = NA, test_KGE = NA, test_RMSE = NA, test_PBIAS = NA)
    
for(i in 1:model_runs){
  
  #sample training versus testign sites for ith model run
  set.seed(i)
  training.samples <- sample(1:nrow(model_input), round(0.8*nrow(model_input)), replace=FALSE)
  train_data <- model_input[training.samples,]
  test_data <- model_input[-training.samples,]
  train_label <- model_target[training.samples]
  test_label <- model_target[-training.samples]
  
  #process model input data and save preprocessing caret model
  process <- caret::preProcess(train_data, method = c("center", "scale"))
  train_data <- predict(process,train_data)
  test_data <- predict(process,test_data)
  caret_models[[i]] <- process
  
  #prepare data for XGBoost model
  dtrain <- xgb.DMatrix(data = data.matrix(train_data), label= data.matrix(train_label))
  dtest <- xgb.DMatrix(data =  data.matrix(test_data), label= data.matrix(test_label))

  #train the model and save it
  set.seed(i+model_runs)
  model <- xgb.train(params = param_model, nrounds = nrounds_model, data = dtrain)
  nit_pred <- predict(model,dtest)
  model_ensemble[[i]] <- model
  
  #get predictions for training and testing sites for ith model run in normal scale
  train_pred <- predict(model,dtrain)
  test_pred <- predict(model,dtest)
  train_pred <- 10^train_pred
  test_pred <- 10^test_pred
  
  #get observed nitrate concentrations in normal scale for training and testing sites
  train_obs=nit_df$mean_nit[training.samples]
  test_obs=nit_df$mean_nit[-training.samples]

  #calcualte model performance
  model_perf_df$train_NSE[i] <- NSE(train_pred,train_obs)
  model_perf_df$train_KGE[i] <- KGE(train_pred,train_obs)
  model_perf_df$train_RMSE[i] <- rmse(train_pred,train_obs)
  model_perf_df$train_PBIAS[i] <- pbias(train_pred,train_obs)
  model_perf_df$test_NSE[i] <- NSE(test_pred,test_obs)
  model_perf_df$test_KGE[i] <- KGE(test_pred,test_obs)
  model_perf_df$test_RMSE[i] <- rmse(test_pred,test_obs)
  model_perf_df$test_PBIAS[i] <- pbias(test_pred,test_obs)
  
  #get model feature importance
  importance_matrix_list[[i]] <- xgb.importance(model = model)
  
}

#get mean importance of catchment characteristics or model features
combined_imp_df <- importance_matrix_list %>%
  purrr::map_df(~ .)
mean_importance_df <- combined_imp_df %>%
  group_by(Feature) %>%
  summarize(
    Gain_mean = mean(Gain, na.rm = TRUE),
    Cover_mean = mean(Cover, na.rm = TRUE),
    Frequency_mean = mean(Frequency, na.rm = TRUE),
    Gain_sd = sd(Gain, na.rm = TRUE),
    Cover_sd = sd(Cover, na.rm = TRUE),
    Frequency_sd = sd(Frequency, na.rm = TRUE)
  )

#save preprocessing caret models and BRT model ensembles  
save(model_ensemble,caret_models,
     file = "models/trained_models.RData")

#save model performance metrics and model feature importance
write.table(model_perf_df,"results/BRT_model_performance.txt",
            sep="\t",row.names=FALSE)
write.table(mean_importance_df,"results/BRT_mean_importance.txt",
            sep="\t",row.names=FALSE)

run_time <- Sys.time()-start_time
sprintf("Code ran in %02d:%02d:%02d", 
        floor(as.numeric(run_time, "secs")/3600),
        floor((as.numeric(run_time, "secs")%%3600)/60),
        round(as.numeric(run_time, "secs")%%60))
