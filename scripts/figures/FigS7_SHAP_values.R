################################################################################
# Author : Kayalvizhi Sadayappan                                               # 
#                                                                              #
# This code was used to plot fig S7 in Climate Aridity                         #
# Amplifies Global Riverine Nitrate Beyond Human Inputs paper                  # 
#                                                                              #
# Inputs: Download "global_river_segment_classes.txt" from                     #
# https://zenodo.org/records/7056647 and "BRT_train_data_7213_catchments.txt"  #
# from https://github.com/KayalvizhiSadayappan/global-river-nitrate-model/     #
#                                                                              #
# R version 4.3.2 was used - Package versions in comments                      #
################################################################################
library(readr) # ‘2.1.5’
library(xgboost) #‘1.7.7.1’
library(caret) #‘6.0.94’
library(SHAPforxgboost)# ‘0.1.3’
library(ggplot2) #‘3.5.1’
library(patchwork) #‘1.3.0’

classify_landuse=function(lulc){
  lulc$landuse_class <- "mixed"
  lulc$landuse_class[lulc$crop_mean + lulc$crop_nat_mean <= 25 & lulc$urban_mean > 25] <- "urban"
  lulc$landuse_class[lulc$crop_mean + lulc$crop_nat_mean > 50 & lulc$urban_mean <= 5] <- "agri"
  lulc$landuse_class[lulc$crop_mean + lulc$crop_nat_mean+
                       lulc$urban_mean < 0.01 & lulc$popden < 1] <- "undev"
  return(lulc)
}

root_folder <- "global-river-nitrate-model"
setwd(root_folder)

output_folder <- file.path("figures","figS7")
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)

ncores=4
#read in input data
nit_df <- read_delim(file="data/raw/BRT_train_data_7213_catchments.txt",
                     delim = "\t", escape_double = FALSE,trim_ws = TRUE)
global_river_segment_classes <- read_delim("data/raw/global_river_segment_classes.txt",
                                           delim = "\t", escape_double = FALSE, trim_ws = TRUE)
nit_df$popden <- global_river_segment_classes$popden_2020_per_km2[match(nit_df$COMID,global_river_segment_classes$COMID)]
nit_df <- classify_landuse(nit_df)

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
SHAP_df$nit_pred <- 10^predict(model,dtrain) 
SHAP_df$landuse <- nit_df$landuse_class[training.samples]

#plot SHAP values for 10 most important features - Fig S7
si_shap=ggplot(data=SHAP_df)+geom_point(aes(x = X_crop_mean,y = SHAP_crop_mean,
                                       fill=nit_pred),pch=21,alpha=0.5)+
  scale_fill_gradient(low = "white", high = "black")+
  geom_smooth(aes(x = X_crop_mean, y = SHAP_crop_mean), method = "loess", color = "black", se = FALSE)+
  geom_smooth(data=SHAP_df[which(SHAP_df$landuse=="undev"),],
              aes(x = X_crop_mean, y = SHAP_crop_mean), method = "loess", color = alpha("#A3F0FF",0.75), se = FALSE)+
  geom_smooth(data=SHAP_df[which(SHAP_df$landuse=="urban"),],
              aes(x = X_crop_mean, y = SHAP_crop_mean), method = "loess", color = alpha("#1F8DFF",0.75), se = FALSE)+
  geom_smooth(data=SHAP_df[which(SHAP_df$landuse=="agri"),],
              aes(x = X_crop_mean, y = SHAP_crop_mean), method = "loess", color = alpha("#2A45FF",0.75), se = FALSE)+
  geom_hline(yintercept = 0,linetype="dashed")+
  theme_classic()+theme(legend.position = "none",
                        axis.title=element_blank())+
  ggplot(data=SHAP_df)+geom_point(aes(x=X_urban_mean,y=SHAP_urban_mean,
                                 fill=nit_pred),pch=21,alpha=0.5)+
  scale_fill_gradient(low = "white", high = "black")+
  geom_smooth(aes(x = X_urban_mean, y = SHAP_urban_mean), method = "loess", color = "black", se = FALSE)+
  geom_smooth(data=SHAP_df[which(SHAP_df$landuse=="undev"),],
              aes(x = X_urban_mean, y = SHAP_urban_mean), method = "loess", color = alpha("#A3F0FF",0.75), se = FALSE)+
  geom_smooth(data=SHAP_df[which(SHAP_df$landuse=="urban"),],
              aes(x = X_urban_mean, y = SHAP_urban_mean), method = "loess", color = alpha("#1F8DFF",0.75), se = FALSE)+
  geom_smooth(data=SHAP_df[which(SHAP_df$landuse=="agri"),],
              aes(x = X_urban_mean, y = SHAP_urban_mean), method = "loess", color = alpha("#2A45FF",0.75), se = FALSE)+
  geom_hline(yintercept = 0,linetype="dashed")+
  theme_classic()+theme(legend.position = "none",
                        axis.title=element_blank())+
  ggplot(data=SHAP_df)+geom_point(aes(x=X_MAT,y=SHAP_MAT,
                                 fill=nit_pred),pch=21,alpha=0.5)+
  scale_fill_gradient(low = "white", high = "black")+
  geom_smooth(aes(x = X_MAT,y=SHAP_MAT), method = "loess", color = "black", se = FALSE)+
  geom_smooth(data=SHAP_df[which(SHAP_df$landuse=="undev"),],
              aes(x = X_MAT,y=SHAP_MAT), method = "loess", color = alpha("#A3F0FF",0.75), se = FALSE)+
  geom_smooth(data=SHAP_df[which(SHAP_df$landuse=="urban"),],
              aes(x = X_MAT,y=SHAP_MAT), method = "loess", color = alpha("#1F8DFF",0.75), se = FALSE)+
  geom_smooth(data=SHAP_df[which(SHAP_df$landuse=="agri"),],
              aes(x = X_MAT,y=SHAP_MAT), method = "loess", color = alpha("#2A45FF",0.75), se = FALSE)+
  geom_hline(yintercept = 0,linetype="dashed")+
  theme_classic()+theme(legend.position = "none",
                        axis.title=element_blank())+
  ggplot(data=SHAP_df)+geom_point(aes(x=X_crop_nat_mean,y=SHAP_crop_nat_mean,
                                 fill=nit_pred),pch=21,alpha=0.5)+
  scale_fill_gradient(low = "white", high = "black")+
  geom_smooth(data=SHAP_df[which(SHAP_df$landuse=="undev"),],
              aes(x = X_crop_nat_mean,y=SHAP_crop_nat_mean), method = "loess", color = alpha("#A3F0FF",0.75), se = FALSE)+
  geom_smooth(data=SHAP_df[which(SHAP_df$landuse=="urban"),],
              aes(x = X_crop_nat_mean,y=SHAP_crop_nat_mean), method = "loess", color = alpha("#1F8DFF",0.75), se = FALSE)+
  geom_smooth(data=SHAP_df[which(SHAP_df$landuse=="agri"),],
              aes(x = X_crop_nat_mean,y=SHAP_crop_nat_mean), method = "loess", color = alpha("#2A45FF",0.75), se = FALSE)+
  geom_smooth(aes(x = X_crop_nat_mean,y=SHAP_crop_nat_mean), method = "loess", color = "black", se = FALSE)+
  geom_hline(yintercept = 0,linetype="dashed")+
  theme_classic()+theme(legend.position = "none",
                        axis.title=element_blank())+
  ggplot(data=SHAP_df)+geom_point(aes(x=X_MAP,y=SHAP_MAP,
                                 fill=nit_pred),pch=21,alpha=0.5)+
  scale_fill_gradient(low = "white", high = "black")+
  geom_smooth(aes(x = X_MAP,y=SHAP_MAP), method = "loess", color = "black", se = FALSE)+
  geom_smooth(data=SHAP_df[which(SHAP_df$landuse=="undev"),],
              aes(x = X_MAP,y=SHAP_MAP), method = "loess", color = alpha("#A3F0FF",0.75), se = FALSE)+
  geom_smooth(data=SHAP_df[which(SHAP_df$landuse=="urban"),],
              aes(x = X_MAP,y=SHAP_MAP), method = "loess", color = alpha("#1F8DFF",0.75), se = FALSE)+
  geom_smooth(data=SHAP_df[which(SHAP_df$landuse=="agri"),],
              aes(x = X_MAP,y=SHAP_MAP), method = "loess", color = alpha("#2A45FF",0.75), se = FALSE)+
  geom_hline(yintercept = 0,linetype="dashed")+
  theme_classic()+theme(legend.position = "none",
                        axis.title=element_blank())+
  ggplot(data=SHAP_df)+geom_point(aes(x=X_sand_mean,y=SHAP_sand_mean,
                                 fill=nit_pred),pch=21,alpha=0.5)+
  scale_fill_gradient(low = "white", high = "black")+
  geom_smooth(aes(x = X_sand_mean,y=SHAP_sand_mean), method = "loess", color = "black", se = FALSE)+
  geom_smooth(data=SHAP_df[which(SHAP_df$landuse=="undev"),],
              aes(x = X_sand_mean,y=SHAP_sand_mean), method = "loess", color = alpha("#A3F0FF",0.75), se = FALSE)+
  geom_smooth(data=SHAP_df[which(SHAP_df$landuse=="urban"),],
              aes(x = X_sand_mean,y=SHAP_sand_mean), method = "loess", color = alpha("#1F8DFF",0.75), se = FALSE)+
  geom_smooth(data=SHAP_df[which(SHAP_df$landuse=="agri"),],
              aes(x = X_sand_mean,y=SHAP_sand_mean), method = "loess", color = alpha("#2A45FF",0.75), se = FALSE)+
  geom_hline(yintercept = 0,linetype="dashed")+
  theme_classic()+theme(legend.position = "none",
                        axis.title=element_blank())+
  ggplot(data=SHAP_df)+geom_point(aes(x=X_bdod_mean,y=SHAP_bdod_mean,
                                 fill=nit_pred),pch=21,alpha=0.5)+
  scale_fill_gradient(low = "white", high = "black")+
  geom_smooth(aes(x = X_bdod_mean,y=SHAP_bdod_mean), method = "loess", color = "black", se = FALSE)+
  geom_smooth(data=SHAP_df[which(SHAP_df$landuse=="undev"),],
              aes(x = X_bdod_mean,y=SHAP_bdod_mean), method = "loess", color = alpha("#A3F0FF",0.75), se = FALSE)+
  geom_smooth(data=SHAP_df[which(SHAP_df$landuse=="urban"),],
              aes(x = X_bdod_mean,y=SHAP_bdod_mean), method = "loess", color = alpha("#1F8DFF",0.75), se = FALSE)+
  geom_smooth(data=SHAP_df[which(SHAP_df$landuse=="agri"),],
              aes(x = X_bdod_mean,y=SHAP_bdod_mean), method = "loess", color = alpha("#2A45FF",0.75), se = FALSE)+
  geom_hline(yintercept = 0,linetype="dashed")+
  theme_classic()+theme(legend.position = "none",
                        axis.title=element_blank())+
  ggplot(data=SHAP_df)+geom_point(aes(x=X_forest_mean,y=SHAP_forest_mean,
                                 fill=nit_pred),pch=21,alpha=0.5)+
  scale_fill_gradient(low = "white", high = "black")+
  geom_smooth(aes(x = X_forest_mean,y=SHAP_forest_mean), method = "loess", color = "black", se = FALSE)+
  geom_smooth(data=SHAP_df[which(SHAP_df$landuse=="undev"),],
              aes(x = X_forest_mean,y=SHAP_forest_mean), method = "loess", color = alpha("#A3F0FF",0.75), se = FALSE)+
  geom_smooth(data=SHAP_df[which(SHAP_df$landuse=="urban"),],
              aes(x = X_forest_mean,y=SHAP_forest_mean), method = "loess", color = alpha("#1F8DFF",0.75), se = FALSE)+
  geom_smooth(data=SHAP_df[which(SHAP_df$landuse=="agri"),],
              aes(x = X_forest_mean,y=SHAP_forest_mean), method = "loess", color = alpha("#2A45FF",0.75), se = FALSE)+
  geom_hline(yintercept = 0,linetype="dashed")+
  theme_classic()+theme(legend.position = "none",
                        axis.title=element_blank())+
  ggplot(data=SHAP_df)+geom_point(aes(x=X_grass_mean,y=SHAP_grass_mean,
                                 fill=nit_pred),pch=21,alpha=0.5)+
  scale_fill_gradient(low = "white", high = "black")+
  geom_smooth(aes(x = X_grass_mean,y=SHAP_grass_mean), method = "loess", color = "black", se = FALSE)+
  geom_smooth(data=SHAP_df[which(SHAP_df$landuse=="undev"),],
              aes(x = X_grass_mean,y=SHAP_grass_mean), method = "loess", color = alpha("#A3F0FF",0.75), se = FALSE)+
  geom_smooth(data=SHAP_df[which(SHAP_df$landuse=="urban"),],
              aes(x = X_grass_mean,y=SHAP_grass_mean), method = "loess", color = alpha("#1F8DFF",0.75), se = FALSE)+
  geom_smooth(data=SHAP_df[which(SHAP_df$landuse=="agri"),],
              aes(x = X_grass_mean,y=SHAP_grass_mean), method = "loess", color = alpha("#2A45FF",0.75), se = FALSE)+
  geom_hline(yintercept = 0,linetype="dashed")+
  theme_classic()+theme(legend.position = "none",
                        axis.title=element_blank())+
  ggplot(data=SHAP_df)+geom_point(aes(x = X_cec_mean, y = SHAP_cec_mean,
                                 fill=nit_pred),pch=21,alpha=0.5)+
  scale_fill_gradient(low = "white", high = "black")+
  geom_smooth(aes(x = X_cec_mean,y=SHAP_cec_mean), method = "loess", color = "black", se = FALSE)+
  geom_smooth(data=SHAP_df[which(SHAP_df$landuse=="undev"),],
              aes(x = X_cec_mean,y=SHAP_cec_mean), method = "loess", color = alpha("#A3F0FF",0.75), se = FALSE)+
  geom_smooth(data=SHAP_df[which(SHAP_df$landuse=="urban"),],
              aes(x = X_cec_mean,y=SHAP_cec_mean), method = "loess", color = alpha("#1F8DFF",0.75), se = FALSE)+
  geom_smooth(data=SHAP_df[which(SHAP_df$landuse=="agri"),],
              aes(x = X_cec_mean,y=SHAP_cec_mean), method = "loess", color = alpha("#2A45FF",0.75), se = FALSE)+
  geom_hline(yintercept = 0,linetype="dashed")+
  theme_classic()+theme(legend.position = "none",
                        axis.title=element_blank())+
  plot_layout(nrow=2)


ggsave("figures/figS8/figS8_SHAP_feature_interaction.svg",
       plot = si_shap,  width = 10.0, height = 2.50)