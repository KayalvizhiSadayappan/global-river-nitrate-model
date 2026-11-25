################################################################################
# Author : Kayalvizhi Sadayappan                                               # 
#                                                                              #
# This code was used to plot Fig S3 in Climate Aridity                         #
# Amplifies Global Riverine Nitrate Beyond Human Inputs paper                  #
#                                                                              #
# It was used to compare distribution of model performance                     #
# among different landuse classes, aridity classes and continent               # 
#                                                                              #
# The 1000 model were run to get train and test site predictions.              #
# For each model, test and train metrics among different classes               #
# were calculated and their distribution was compared                          #
#                                                                              #
# The input file "BRT_train_data_7213_catchments.txt" can be                   #
# downloaded from global-river-nitrate-model/data/raw folder at                # 
# https://github.com/KayalvizhiSadayappan/global-river-nitrate-model/          # 
# and "global_river_segment_classes.txt" at                                    # 
# https://zenodo.org/records/21853129                                          #
#                                                                              #
# R version 4.3.2.was used - Package versions in comments                      #
################################################################################
library(readr) #‘2.1.5’
library(ggplot2) #‘3.5.1’
library(patchwork) #‘1.3.0’
library(hydroGOF) #‘0.6.0’
library(caret) #‘6.0.94’
library(xgboost) #‘1.7.7.1’
library(dplyr) #‘1.1.4’
library(tidyr) #‘1.3.1’

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

output_folder <- file.path("figures","figS3")
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)

#input attributes and parameters for Boosted Regression Trees
sel_att=c("forest_mean","shrub_mean","grass_mean","wetland_mean","crop_mean",
          "urban_mean","crop_nat_mean","ice_mean","barren_mean","water_mean",
          "MAP","MAT","porosity","perm_harm_m2perd","aqf_thick_m",
          "sand_mean","soc_mean","cec_mean","bdod_mean","nitrogen_mean",
          "mean_elev","mean_slope","stream_density_perkm","max_order",
          "order1_per","area_km2")
nrounds_model=370
param_model=list(booster = "gbtree", 
               objective = "reg:squarederror", 
               eval_metric="rmse",
               eta = 0.05,
               max_depth = 8,
               min_child_weight = 11,
               subsample = 0.6,
               colsample_bytree = 0.6)

#read input characteritics for the model and get their classes
nit_df=read_delim(file="data/raw/BRT_train_data_7213_catchments.txt",
                  delim = "\t", escape_double = FALSE,trim_ws = TRUE)
global_river_segment_classes <- read_delim(/data/raw/global_river_segment_classes.txt",
                                           delim = "\t", escape_double = FALSE, trim_ws = TRUE)
nit_df[c("popden", "aridity_class", "continent")] = global_river_segment_classes[match(nit_df$COMID,global_river_segment_classes$COMID),
                                                c("popden_2020_per_km2", "aridity_class", "continent")]
nit_df$continent[which(nit_df$COMID == "23029954")] = "Europe"
nit_df = classify_landuse(nit_df)

sel_ids=c("COMID","landuse_class","aridity_class","continent")
test_pred_lt=vector(mode="list",length=1000)
train_pred_lt=vector(mode="list",length=1000)

model_input <- nit_df[,sel_att]; model_target <- log10(nit_df$mean_nit)
model_runs <- 1000
for(i in 1:model_runs){
  
  #sample training versus testign sites for ith model run
  set.seed(i)
  training.samples <- sample(1:nrow(model_input), round(0.8*nrow(model_input)), replace=FALSE)
  train_data <- model_input[training.samples,]
  test_data <- model_input[-training.samples,]
  train_label <- model_target[training.samples]
  test_label <- model_target[-training.samples]
  
  #process model input data
  process <- caret::preProcess(train_data, method = c("center", "scale"))
  train_data <- predict(process,train_data)
  test_data <- predict(process,test_data)

  #prepare data for XGBoost model
  dtrain <- xgb.DMatrix(data = data.matrix(train_data), label= data.matrix(train_label))
  dtest <- xgb.DMatrix(data =  data.matrix(test_data), label= data.matrix(test_label))
  
  #train the model 
  set.seed(i+model_runs)
  model <- xgb.train(params = param_model, nrounds = nrounds_model, data = dtrain)
  nit_pred <- predict(model,dtest)

  #get predictions for training and testing sites for ith model run in normal scale
  train_pred <- predict(model,dtrain)
  test_pred <- predict(model,dtest)
  
  test_pred_lt[[i]]=nit_df[-training.samples,sel_ids]
  test_pred_lt[[i]]$seed=i
  test_pred_lt[[i]]$nit_pred=10^test_pred
  test_pred_lt[[i]]$nit_org=nit_df$mean_nit[-training.samples]
  test_pred_lt[[i]]$type="test"
  train_pred_lt[[i]]=nit_df[training.samples,sel_ids]
  train_pred_lt[[i]]$seed=i
  train_pred_lt[[i]]$nit_pred=10^train_pred
  train_pred_lt[[i]]$nit_org=nit_df$mean_nit[training.samples]
  train_pred_lt[[i]]$type="train"
  
  print(i)
}

pred_lt=rbind(do.call(rbind,train_pred_lt),do.call(rbind,test_pred_lt))

#calculate model performance among classes of different landuse 
model_metrics_landuse <- pred_lt %>%
  group_by(landuse_class, seed) %>%
  summarise(
    train_nse = NSE(nit_pred[type == "train"], nit_org[type == "train"]),
    test_nse  = NSE(nit_pred[type == "test"],  nit_org[type == "test"]),
    train_kge = KGE(nit_pred[type == "train"], nit_org[type == "train"]),
    test_kge  = KGE(nit_pred[type == "test"],  nit_org[type == "test"]),
    train_rmse = rmse(nit_pred[type == "train"], nit_org[type == "train"]),
    test_rmse  = rmse(nit_pred[type == "test"],  nit_org[type == "test"]),
    train_pbias = pbias(nit_pred[type == "train"], nit_org[type == "train"]),
    test_pbias  = pbias(nit_pred[type == "test"],  nit_org[type == "test"]),
    train_n = length(nit_pred[type == "train"]),
    test_n = length(nit_pred[type == "test"]),
    .groups = "drop"
  ) %>%
  rename(class = landuse_class)

#calculate model performance among different aridity classes  
#Due to very few number of data points, in most cases the metrics couldn't be
#calculated for hyper-arid class. We used cases where there were
#enough points for hyper-arid class while plotting distributions 
model_metrics_AI <- pred_lt %>%
  group_by(aridity_class, seed) %>%
  summarise(
    train_nse = NSE(nit_pred[type == "train"], nit_org[type == "train"]),
    test_nse  = NSE(nit_pred[type == "test"],  nit_org[type == "test"]),
    train_kge = KGE(nit_pred[type == "train"], nit_org[type == "train"]),
    test_kge  = KGE(nit_pred[type == "test"],  nit_org[type == "test"]),
    train_rmse = rmse(nit_pred[type == "train"], nit_org[type == "train"]),
    test_rmse  = rmse(nit_pred[type == "test"],  nit_org[type == "test"]),
    train_pbias = pbias(nit_pred[type == "train"], nit_org[type == "train"]),
    test_pbias  = pbias(nit_pred[type == "test"],  nit_org[type == "test"]),
    train_n = length(nit_pred[type == "train"]),
    test_n = length(nit_pred[type == "test"]),
    .groups = "drop"
  ) %>%
  rename(class = aridity_class)

#calculate model performance among sites in different continents
#Due to very few number of data points, the metrics couldn't be
#calculated for Africa in most cases. We used cases where there were
#enough points for Africa while plotting distributions 
model_metrics_cont <- pred_lt %>%
  group_by(continent, seed) %>%
  summarise(
    train_nse = NSE(nit_pred[type == "train"], nit_org[type == "train"]),
    test_nse  = NSE(nit_pred[type == "test"],  nit_org[type == "test"]),
    train_kge = KGE(nit_pred[type == "train"], nit_org[type == "train"]),
    test_kge  = KGE(nit_pred[type == "test"],  nit_org[type == "test"]),
    train_rmse = rmse(nit_pred[type == "train"], nit_org[type == "train"]),
    test_rmse  = rmse(nit_pred[type == "test"],  nit_org[type == "test"]),
    train_pbias = pbias(nit_pred[type == "train"], nit_org[type == "train"]),
    test_pbias  = pbias(nit_pred[type == "test"],  nit_org[type == "test"]),
    train_n = length(nit_pred[type == "train"]),
    test_n = length(nit_pred[type == "test"]),
    .groups = "drop"
  ) %>%
  rename(class = continent)

#prepare data for plotting
model_metrics_landuse <- model_metrics_landuse %>%
  mutate(class = case_match(
    class,
    "1. undev" ~ "undev",
    "2. mixed" ~ "mixed",
    "3. urban" ~ "urban",
    "4. agri" ~ "agri",
    .default = class 
  ))

metrics_long_landuse <- model_metrics_landuse %>%
  pivot_longer(
    cols = c(train_nse, test_nse,
             train_kge, test_kge,
             train_rmse, test_rmse,
             train_pbias, test_pbias),
    names_to = "metric",
    values_to = "value"
  ) %>%
  tidyr::separate(metric, into = c("type", "metric_name"), sep = "_") %>%
  mutate(
    type = factor(type, levels = c("train", "test")),
    class=factor(class,levels = c("undev","mixed","urban","agri")))

model_metrics_AI <- model_metrics_AI %>%
  mutate(class = case_match(
    class,
    "1. Humid" ~ "H",
    "2. Sub-humid" ~ "SH",
    "3. Semi-arid" ~ "SA",
    "4. Arid" ~ "A",
    "5. Hyper-arid" ~ "HA",
    .default = class 
  ))

metrics_long_AI <- model_metrics_AI %>%
  pivot_longer(
    cols = c(train_nse, test_nse,
             train_kge, test_kge,
             train_rmse, test_rmse,
             train_pbias, test_pbias),
    names_to = "metric",
    values_to = "value"
  ) %>%
  tidyr::separate(metric, into = c("type", "metric_name"), sep = "_") %>%
  mutate(
    type = factor(type, levels = c("train", "test")),
    class=factor(class,levels = c("H","SH","SA","A","HA")))

model_metrics_cont <- model_metrics_cont %>%
  mutate(class = case_match(
    class,
    "North America" ~ "NA",
    "South America" ~ "SA",
    "Europe" ~ "EU",
    "Africa" ~ "AF",
    "Asia" ~ "AS",
    "Oceania" ~ "OC",
    .default = class 
  ))

metrics_long_cont <- model_metrics_cont %>%
  pivot_longer(
    cols = c(train_nse, test_nse,
             train_kge, test_kge,
             train_rmse, test_rmse,
             train_pbias, test_pbias),
    names_to = "metric",
    values_to = "value"
  ) %>%
  tidyr::separate(metric, into = c("type", "metric_name"), sep = "_") %>%
  mutate(
    type = factor(type, levels = c("train", "test")))


nit_df= nit_df %>% 
  mutate(aridity_class = factor(case_match(
    aridity_class,
    "1. Humid" ~ "H",
    "2. Sub-humid" ~ "SH",
    "3. Semi-arid" ~ "SA",
    "4. Arid" ~ "A",
    "5. Hyper-arid" ~ "HA",
    .default = aridity_class 
  ))) %>% 
  mutate(aridity_class = factor(aridity_class,levels = c("H","SH","SA","A","HA")))


nit_df= nit_df %>% 
  mutate(landuse_class = factor(case_match(
    landuse_class,
    "1. undev" ~ "undev",
    "2. mixed" ~ "mixed",
    "3. urban" ~ "urban",
    "4. agri" ~ "agri",
    .default = landuse_class 
  ))) %>% 
  mutate(landuse_class = factor(landuse_class,
                                levels = c("undev","mixed","urban","agri")))
nit_df= nit_df %>% 
  mutate(continent = factor(case_match(
    continent,
    "North America" ~ "NA",
    "South America" ~ "SA",
    "Europe" ~ "EU",
    "Africa" ~ "AF",
    "Asia" ~ "AS",
    "Oceania" ~ "OC",
    .default = continent 
  ))) #%>% 
  #mutate(continent = factor(continent,
   #                               levels = c("NA","SA","EU","AF","AS","OC")))

#plot the distribution of model performance among different aridity classes
AI_plot=ggplot(nit_df %>% filter(!is.na(aridity_class)),
               aes(x = aridity_class )) +
  geom_bar() + scale_y_log10()+
  geom_text(
    stat = "count",size=2.5,
    aes(label = after_stat(count)),
    vjust = -0.2
  ) +
  theme_classic()+theme(axis.title =element_blank(),
                        legend.position = "none")+
  ggplot(
    data = metrics_long_AI %>%
      filter(metric_name == "nse", !is.na(class)),
    aes(x = class, y = value, color = type)
  ) +scale_color_manual(values=c("grey","black"))+
  geom_hline(aes(yintercept = 0.5),col="grey",linetype="dashed")+
  geom_boxplot(outlier.shape = NA,
               position = position_dodge(width = 0.7)) +
  coord_cartesian(ylim = c(-2, 1)) +
  theme_classic() +theme(axis.title =element_blank(),
                         legend.position = "none")+
  ggplot(
    data = metrics_long_AI %>%
      filter(metric_name == "kge", !is.na(class)),
    aes(x = class, y = value, color = type)
  ) +scale_color_manual(values=c("grey","black"))+
  geom_hline(aes(yintercept = 0.5),col="grey",linetype="dashed")+
  geom_boxplot(outlier.shape = NA,
               position = position_dodge(width = 0.7)) +
  coord_cartesian(ylim = c(-2, 1)) +
  theme_classic() +theme(axis.title =element_blank(),
                         legend.position = "none")+ 
  ggplot(
    data = metrics_long_AI %>%
      filter(metric_name == "rmse", !is.na(class)),
    aes(x = class, y = value, color = type)
  ) +scale_color_manual(values=c("grey","black"))+
  geom_hline(aes(yintercept = 0),col="grey",linetype="dashed")+
  geom_boxplot(outlier.shape = NA,
               position = position_dodge(width = 0.7)) +
  coord_cartesian(ylim = c(0, 2.5)) +
  theme_classic() +theme(axis.title =element_blank(),
                         legend.position = "none")+
  ggplot(
    data = metrics_long_AI %>%
      filter(metric_name == "pbias", !is.na(class)),
    aes(x = class, y = value, color = type)
  ) +scale_color_manual(values=c("grey","black"))+
  geom_hline(aes(yintercept = 20),col="grey",linetype="dashed")+
  geom_hline(aes(yintercept = -20),col="grey",linetype="dashed")+
  geom_boxplot(outlier.shape = NA,
               position = position_dodge(width = 0.7)) +
  coord_cartesian(ylim = c(-100, 100)) +
  theme_classic() +theme(axis.title =element_blank(),
                         legend.position = "none")+plot_layout(nrow=1)



#plot the distribution of model performance among different continents
cont_plot=ggplot(nit_df %>% filter(!is.na(continent)),
                 aes(x = continent )) +
  geom_bar() + scale_y_log10()+
  geom_text(
    stat = "count",size=2.5,
    aes(label = after_stat(count)),
    vjust = -0.2
  ) +
  theme_classic()+theme(axis.title =element_blank(),
                        legend.position = "none")+
  ggplot(
    data = metrics_long_cont %>%
      filter(metric_name == "nse", !is.na(class)),
    aes(x = class, y = value, color = type)
  ) +scale_color_manual(values=c("grey","black"))+
  geom_hline(aes(yintercept = 0.5),col="grey",linetype="dashed")+
  geom_boxplot(outlier.shape = NA,
               position = position_dodge(width = 0.7)) +
  coord_cartesian(ylim = c(-2, 1)) +
  theme_classic() +theme(axis.title =element_blank(),
                         legend.position = "none")+
  ggplot(
    data = metrics_long_cont %>%
      filter(metric_name == "kge", !is.na(class)),
    aes(x = class, y = value, color = type)
  ) +scale_color_manual(values=c("grey","black"))+
  geom_hline(aes(yintercept = 0.5),col="grey",linetype="dashed")+
  geom_boxplot(outlier.shape = NA,
               position = position_dodge(width = 0.7)) +
  coord_cartesian(ylim = c(-2, 1)) +
  theme_classic() +theme(axis.title =element_blank(),
                         legend.position = "none")+
  ggplot(
    data = metrics_long_cont %>%
      filter(metric_name == "rmse", !is.na(class)),
    aes(x = class, y = value, color = type)
  ) +scale_color_manual(values=c("grey","black"))+
  geom_hline(aes(yintercept = 0),col="grey",linetype="dashed")+
  geom_boxplot(outlier.shape = NA,
               position = position_dodge(width = 0.7)) +
  coord_cartesian(ylim = c(0, 2.5)) +
  theme_classic() +theme(axis.title =element_blank(),
                         legend.position = "none")+
  ggplot(
    data = metrics_long_cont %>%
      filter(metric_name == "pbias", !is.na(class)),
    aes(x = class, y = value, color = type)
  ) +scale_color_manual(values=c("grey","black"))+
  geom_hline(aes(yintercept = 20),col="grey",linetype="dashed")+
  geom_hline(aes(yintercept = -20),col="grey",linetype="dashed")+
  geom_boxplot(outlier.shape = NA,
               position = position_dodge(width = 0.7)) +
  coord_cartesian(ylim = c(-100, 100)) +
  theme_classic() +theme(axis.title =element_blank(),
                         legend.position = "none")+
  plot_layout(nrow=1)

landuse_plot=ggplot(nit_df %>% filter(!is.na(landuse_class)),
                    aes(x = landuse_class)) +
  geom_bar() +
  geom_text(
    stat = "count",size=2.5,
    aes(label = after_stat(count)),
    vjust = -0.2
  ) +scale_y_log10()+
  theme_classic()+theme(axis.title =element_blank(),
                        legend.position = "none")+
  ggplot(
    data = metrics_long_landuse %>%
      filter(metric_name == "nse", !is.na(class)),
    aes(x = class, y = value, color = type)
  ) +scale_color_manual(values=c("grey","black"))+
  geom_hline(aes(yintercept = 0.5),col="grey",linetype="dashed")+
  geom_boxplot(outlier.shape = NA,
               position = position_dodge(width = 0.7)) +
  coord_cartesian(ylim = c(-2, 1)) +
  theme_classic() +theme(axis.title =element_blank(),
                         legend.position = "none")+
  ggplot(
    data = metrics_long_landuse %>%
      filter(metric_name == "kge", !is.na(class)),
    aes(x = class, y = value, color = type)
  ) +scale_color_manual(values=c("grey","black"))+
  geom_hline(aes(yintercept = 0.5),col="grey",linetype="dashed")+
  geom_boxplot(outlier.shape = NA,
               position = position_dodge(width = 0.7)) +
  coord_cartesian(ylim = c(-2, 1)) +
  theme_classic() +theme(axis.title =element_blank(),
                         legend.position = "none")+
  ggplot(
    data = metrics_long_landuse %>%
      filter(metric_name == "rmse", !is.na(class)),
    aes(x = class, y = value, color = type)
  ) +scale_color_manual(values=c("grey","black"))+
  geom_hline(aes(yintercept = 0),col="grey",linetype="dashed")+
  geom_boxplot(outlier.shape = NA,
               position = position_dodge(width = 0.7)) +
  coord_cartesian(ylim = c(0, 2.5)) +
  theme_classic() +theme(axis.title =element_blank(),
                         legend.position = "none")+
  ggplot(
    data = metrics_long_landuse %>%
      filter(metric_name == "pbias", !is.na(class)),
    aes(x = class, y = value, color = type)
  ) +scale_color_manual(values=c("grey","black"))+
  geom_hline(aes(yintercept = 20),col="grey",linetype="dashed")+
  geom_hline(aes(yintercept = -20),col="grey",linetype="dashed")+
  geom_boxplot(outlier.shape = NA,
               position = position_dodge(width = 0.7)) +
  coord_cartesian(ylim = c(-100, 100)) +
  theme_classic() +theme(axis.title =element_blank(),
                         legend.position = "none")+plot_layout(nrow=1)

ggsave("figures/figS3/fig_model_perf_landuse.svg",
       plot = landuse_plot,dpi=1000,width=10.0,height=2.50)

ggsave("figures/figS3/fig_model_perf_AI.svg",
       plot = AI_plot,dpi=1000,width=10.0,height=2.50)

ggsave("figures/figS3/fig_model_perf_cont.svg",
       plot = cont_plot,dpi=1000,width=10.0,height=2.50)


