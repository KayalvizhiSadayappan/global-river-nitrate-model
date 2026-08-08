# ---------------------------------------------------------------
# Script: 04_get_SHAP.R
# Purpose: Derive SHAP values and interaction terms for one model
# Author: Kayalvizhi Sadayappan
# Date: 2026-08-07
# Version: 1.0.1
#
# Inputs:
#   - Folder: data/raw/
#   - Files: BRT_train_data_7213_catchments.txt
#   - Source: The input data for training model is in the repository
#
# Outputs:
#   - Folder: results/
#   - Files: "SHAP_model_338.txt", SHAP_model_338_interaction.txt
# Here 338 is the model run selected for SHAP analysis           
#
# Dependencies:
#   - R packages: dplyr, readr, hydroGOF, xgboost, caret, SHAPforxgboost
# Runtime: The code ran on 4 CPU each with 32 GB of memory and completed in approximately 5 minutes.
# ---------------------------------------------------------------
library(xgboost)
library(readr)
library(hydroGOF)
library(dplyr)
library(caret)
library(SHAPforxgboost)

#select root folder and cores for parallel computing (user specific)
root_folder <- "/global-river-nitrate-model"
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
nit_df <- read_delim(file="data/raw/BRT_train_data_7213_catchments.txt",
                  delim = "\t", escape_double = FALSE,trim_ws = TRUE)
mean_importance_df <- read_delim(file="results/BRT_mean_importance.txt",    
                        delim = "\t", escape_double = FALSE,trim_ws = TRUE)

#attributes to be used to train the model                  
sel_att <- c("forest_mean","shrub_mean","grass_mean","wetland_mean","crop_mean",
          "urban_mean","crop_nat_mean","ice_mean","barren_mean","water_mean",
          "MAP","MAT","porosity","perm_harm_m2perd","aqf_thick_m",
          "sand_mean","soc_mean","cec_mean","bdod_mean","nitrogen_mean",
          "mean_elev","mean_slope","stream_density_perkm","max_order",
          "order1_per","area_km2")

#order attributes in order of importance according to Gain metric over model ensemble
att_order <- mean_importance_df$Feature[order(mean_importance_df$Gain_mean, decreasing = TRUE)]

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

#the model whose performance in terms of test NSE was closest to median model performance was selected
model_sel <- 338

#sample training versus testign sites for seelcted model run
set.seed(model_sel)
training.samples <- sample(1:nrow(model_input), round(0.8*nrow(model_input)), replace=FALSE)
train_data <- model_input[training.samples,]
test_data <- model_input[-training.samples,]
train_label <- model_target[training.samples]
test_label <- model_target[-training.samples]

#process model input data and save preprocessing caret model
process <- caret::preProcess(train_data, method = c("center", "scale"))
train_data <- predict(process,train_data)
test_data <- predict(process,test_data)

#prepare data for XGBoost model
dtrain <- xgb.DMatrix(data = data.matrix(train_data), label= data.matrix(train_label))
dtest <- xgb.DMatrix(data =  data.matrix(test_data), label= data.matrix(test_label))

#train the model and save it
set.seed(model_runs+model_sel)
model <- xgb.train(params = param_model, nrounds = nrounds_model, data = dtrain)

#get SHAP values for selected model
shapely_values_list <- shap.values(model, X_train = dtrain)
shap_values=data.frame(shapely_values_list$shap_score)
colnames(shap_values) = paste0("SHAP_",colnames(shap_values))

#combine training data attributes with their SHAP values in SHAP_df dataframe
SHAP_df <- train_data
colnames(SHAP_df) <- paste0("X_",colnames(train_data))
SHAP_df <- cbind(SHAP_df,shap_values)

#get SHAP interactions between attributes
shap_int <- predict(model, dtrain,predinteraction = TRUE)
n_att <- length(sel_att)
shap_int_df=data.frame(shap_int[,-(n_att+1),-(n_att+1)])

#interaction_strength is symmetric matrix with non-diagonal values indicating 
#attribute-attribute interactions and diagonals indicating SHAP of attribute
interaction_strength <- apply(abs(shap_int[,-(n_att+1),-(n_att+1)]), c(2, 3), mean)

#order attributes in the order of their importance
interaction_strength <- interaction_strength[att_order,att_order]

#diagonal elements are replaced with NA as they are SHAP of features not interactions
diag(interaction_strength) <- NA

#get mean interaction strength for each attribute
mean_interaction_strength <- data.frame(att=colnames(interaction_strength),
                                        interaction=colMeans(interaction_strength,na.rm=T))


#save model performance metrics and model feature importance
write.table(SHAP_df,paste0("results/SHAP_model_",model_sel,".txt"),
            sep="\t",row.names=FALSE)
write.table(interaction_strength,paste0("results/SHAP_interaction_model_",model_sel,".txt"),
            sep="\t",)

run_time <- Sys.time()-start_time
sprintf("Code ran in %02d:%02d:%02d", 
        floor(as.numeric(run_time, "secs")/3600),
        floor((as.numeric(run_time, "secs")%%3600)/60),
        round(as.numeric(run_time, "secs")%%60))

