################################################################################
# Author : Kayalvizhi Sadayappan                                               # 
#                                                                              #
# This code was used to plot Fig S4 in Climate Aridity                         #
# Amplifies Global Riverine Nitrate Beyond Human Inputs paper                  # 
#                                                                              #
# We used subbasins identified using first 2 digits of MERIT Hydro             #
# COMIDs to separate the segments with data into 4 folds in 1000               #
# ways. Model was tested on each fold, while training on rest folds            #
# resulting in 4*1000 training and 4*1000 testing metrics.                     #
# The plotted figure compares their distribution.                              #
#                                                                              #
# The input file "BRT_train_data_7213_catchments.txt" can be                   #
# downloaded from global-river-nitrate-model/data/raw folder at                # 
# https://github.com/KayalvizhiSadayappan/global-river-nitrate-model/          # 
#                                                                              #
# R version 4.3.2.was used - Package versions in comments                      #
################################################################################
library(readr)#‘2.1.5’
library(data.table) #‘1.16.4’
library(dplyr)#‘1.1.4’
library(xgboost)#‘1.7.7.1’
library(caret) #‘6.0.94’
library(hydroGOF) #‘0.6.0’
library(patchwork) #‘1.3.0’
library(ggplot2) #‘3.5.1’

#function to calculate model performance for n folds, when trained on (n-1) folds 
#and tested on left-out fold
get_fold_performance=function(dt,nit_df){
  
  cand=nit_df[,sel_att];target=log10(nit_df$mean_nit)
  
  #assign fold 
  nit_df$fold=dt$group[match(nit_df$subbasin,dt$subbasin)]
  
  total_iter=length(unique(nit_df$fold))
  
  test_results=data.frame(fold_left=seq(1,total_iter,1),NSE=NA,KGE=NA,RMSE=NA,PBIAS=NA)
  train_results=test_results;
  for(i in 1:total_iter){
    
    #ith fold was used as testing data and rest folds used as training data
    training.samples <- which(nit_df$fold!=i)
    train_data=nit_df[training.samples,sel_att]
    test_data=nit_df[-training.samples,sel_att]
    
    train_label=target[training.samples]
    test_label=target[-training.samples]
    
    #preprocess data by centering and scaling
    process=caret::preProcess(train_data, method = c("center", "scale"))
    train_data=predict(process,train_data)
    test_data=predict(process,test_data)
    
    
    dtrain <- xgb.DMatrix(data = data.matrix(train_data), label= data.matrix(train_label))
    dtest <- xgb.DMatrix(data =  data.matrix(test_data), label= data.matrix(test_label))
    
    #train and get model performance metrics
    set.seed(1)
    model=xgb.train(params=param_model,nrounds=nrounds_model,data=dtrain)
    nit_pred=predict(model,dtest)
    test_results$NSE[i]=NSE(nit_pred,test_label)
    test_results$KGE[i]=KGE(nit_pred,test_label)
    test_results$RMSE[i]=rmse(nit_pred,test_label)
    test_results$PBIAS[i]=pbias(nit_pred,test_label)
    
    train_pred=predict(model,dtrain)
    train_results$NSE[i]=NSE(train_pred,train_label)
    train_results$KGE[i]=KGE(train_pred,train_label)
    train_results$RMSE[i]=rmse(train_pred,train_label)
    train_results$PBIAS[i]=pbias(train_pred,train_label)
  }

  res=rbind(test_results,train_results)
  res$type=c(rep("test",total_iter),rep("train",total_iter))
  return(res)
}

root_folder <- "global-river-nitrate-model"
setwd(root_folder)

output_folder <- file.path("figures","figS4")
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)

#input attributes and parameters for Boosted Regression Trees
sel_att=c("forest_mean","shrub_mean","grass_mean","wetland_mean","crop_mean",
          "urban_mean","crop_nat_mean","ice_mean","barren_mean","water_mean",
          "MAP","MAT","porosity","perm_harm_m2perd","aqf_thick_m",
          "sand_mean","soc_mean","cec_mean","bdod_mean","nitrogen_mean",
          "mean_elev","mean_slope","stream_density_perkm","max_order","order1_per",
          "area_km2")
nrounds_model=370
param_model=list(booster = "gbtree", 
               objective = "reg:squarederror", 
               eval_metric="rmse",
               eta = 0.05,
               max_depth = 8,
               min_child_weight = 11,
               subsample = 0.6,
               colsample_bytree = 0.6)

#The first two digits in COMIDs indicate subbasins in MERIT Hydro, which are 
#hydrologicaly disconnected. They were used for spatial blocking on COMIDs with data
nit_df=read_delim(file="data/raw/BRT_train_data_7213_catchments.txt",
                  delim = "\t", escape_double = FALSE,trim_ws = TRUE)
nit_df$subbasin = nit_df$COMID %/% 1E6

# get number of sites in each subbasin
summary_blocks <- nit_df %>% group_by(subbasin) %>%
  summarize(count=n())
setDT(summary_blocks)

#get 20000 random sampling of 4 folds and a measure of their size balance across 4 folds
sb_org <- summary_blocks[, .(n = sum(count)), by = subbasin]
n_iter<-20000
n_iter_lt=vector(mode="list",length=n_iter)
score=rep(NA,n_iter)
n_group=rep(NA,n_iter)
set.seed(123)
for (j in 1:n_iter) {
  #randomly shuffles the subbasins before assigning group or fold
  sb <- sb_org[sample(.N)]
  sb[, group := NA_integer_]
  
  #assign each subbasin to the block of minimum size 
  totals <- numeric(4)
  for (i in seq_len(nrow(sb))) {
    g <- sample(which(totals == min(totals)), 1)  # random tie-breaking
    sb$group[i] <- g
    totals[g] <- totals[g] + sb$n[i]
  }
  x=sb[, .(total_n = sum(n)), by = group][order(group)]
  
  #calculate score for each sample based on variability in fold sizes
  score[j]=sd(x$total_n) / mean(x$total_n)
  n_group[j]=length(unique(sb$group))
  n_iter_lt[[j]]=sb
}

#select 1000 samples of 4 folds with best size balance across 4 folds
ind=order(score)
ind=ind[which(n_group[ind]==4)]
ind=ind[1:1000]

#get training and testing results across folds for 1000 samples
res_list=vector(mode="list",length=1000)
start_time=Sys.time()
for (i in 1:1000){
  dt=n_iter_lt[[ind[i]]]
  res=get_fold_performance(dt,nit_df)
  res$iter=ind[i]
  cat(i," done in ",Sys.time()-start_time,"!\n")
  res_list[[i]]=res
}

#consolidate results from 1000 samples of folds
res_df=do.call(rbind,res_list)

#get the data ready for plotting
res_long <- tidyr::pivot_longer(
  res_df,
  cols = c(NSE, KGE, RMSE, PBIAS),
  names_to = "metric",
  values_to = "value"
)
res_long$type <- factor(
  res_long$type,
  levels = c("train", "test")
)

#make the plot showing model performance using spatial blocking
si=ggplot()+geom_boxplot(data=res_long[which(res_long$metric=="NSE"),],
                         aes(x=type,y=value),outlier.shape=NA)+
  coord_cartesian(ylim=c(0,1))+theme_classic()+theme(axis.title = element_blank())+
  ggplot()+geom_boxplot(data=res_long[which(res_long$metric=="KGE"),],
                        aes(x=type,y=value),outlier.shape=NA)+
  coord_cartesian(ylim=c(0,1))+theme_classic()+theme(axis.title = element_blank())+
  ggplot()+geom_boxplot(data=res_long[which(res_long$metric=="RMSE"),],
                        aes(x=type,y=value),outlier.shape=NA)+
  coord_cartesian(ylim=c(0,0.55))+theme_classic()+theme(axis.title = element_blank())+
  ggplot()+geom_boxplot(data=res_long[which(res_long$metric=="PBIAS"),],
                        aes(x=type,y=value),outlier.shape=NA)+
  coord_cartesian(ylim=c(-60,60))+theme_classic()+theme(axis.title = element_blank())+
  plot_layout(nrow=1)

ggsave("figures/figS4/figS4_spatial_block.svg",si,width=7.5,height=2.5)
