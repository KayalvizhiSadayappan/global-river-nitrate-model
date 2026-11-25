################################################################################
# Author : Kayalvizhi Sadayappan                                               # 
#                                                                              #
# This code was used to plot Fig S2 in Climate Aridity                         #
# Amplifies Global Riverine Nitrate Beyond Human Inputs paper                  #
#                                                                              #
# It was used to compare distribution of input characteristics                 #
# of sites used to train model and all global site characteristics             # 
#                                                                              #
# The input files "BRT_train_data_7213_catchments.txt" can be                  #
# downloaded from global-river-nitrate-model/data/raw folder at                # 
# https://github.com/KayalvizhiSadayappan/global-river-nitrate-model/          # 
# and "BRT_input_data_global.txt" at https://zenodo.org/records/21853129       #
#                                                                              #
# R version 4.3.2.was used - Package versions in comments                      #
################################################################################
library(readr) #‘2.1.5’
library(ggplot2) #‘3.5.1’
library(patchwork) #‘1.3.0’

root_folder <- "global-river-nitrate-model"
setwd(root_folder)

output_folder <- file.path("figures","figS2")
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)

nit_df=read_delim(file="data/raw/BRT_train_data_7213_catchments.txt",
                  delim = "\t", escape_double = FALSE,trim_ws = TRUE)
global_river_segment_classes <- read_delim("data/raw/BRT_input_data_global.txt",
                                           delim = "\t", escape_double = FALSE, trim_ws = TRUE)

#plot distribution of select charactersitics among sites used to train the model and
#global sites - Fig S2
p1=ggplot()+geom_bar(data=nit_df,aes(x=as.factor(max_order)),fill="black")+
  #geom_vline(xintercept = median(nit_df$max_order),linetype="dashed",col="black")+
  theme_classic()#+
#theme(axis.title=element_blank(),axis.text=element_blank())
p2=ggplot()+geom_bar(data=stream_char_cons,aes(x=as.factor(max_order)),fill="grey50")+
  #geom_vline(xintercept = median(stream_char_cons$max_order),linetype="dashed",col="grey50")+
  theme_classic()#+
#theme(axis.title=element_blank(),axis.text=element_blank())
p3=ggplot()+geom_density(data=nit_df,aes(x=area_km2),col="black")+
  geom_vline(xintercept = median(nit_df$area_km2),linetype="dashed",col="black")+
  geom_density(data=stream_char_cons,aes(x=area_km2),col="grey50")+
  geom_vline(xintercept = median(stream_char_cons$area_km2,na.rm=T),linetype="dashed",col="grey50")+
  scale_x_log10()+theme_classic()#+
#theme(axis.title=element_blank(),axis.text=element_blank())
p4=ggplot()+geom_density(data=nit_df,aes(x=crop_mean),col="black")+
  geom_vline(xintercept = median(nit_df$crop_mean),linetype="dashed",col="black")+
  geom_density(data=stream_char_cons,aes(x=crop_mean),col="grey50")+
  geom_vline(xintercept = median(stream_char_cons$crop_mean,na.rm=T),linetype="dashed",col="grey50")+
  scale_x_log10()+theme_classic()#+
#theme(axis.title=element_blank(),axis.text=element_blank())
p5=ggplot()+geom_density(data=nit_df,aes(x=crop_nat_mean),col="black")+
  geom_vline(xintercept = median(nit_df$crop_nat_mean),linetype="dashed",col="black")+
  geom_density(data=stream_char_cons,aes(x=crop_nat_mean),col="grey50")+
  geom_vline(xintercept = median(stream_char_cons$crop_nat_mean,na.rm=T),linetype="dashed",col="grey50")+
  scale_x_log10()+theme_classic()#+
#theme(axis.title=element_blank(),axis.text=element_blank())
p6=ggplot()+geom_density(data=nit_df,aes(x=urban_mean),col="black")+
  geom_vline(xintercept = median(nit_df$urban_mean),linetype="dashed",col="black")+
  geom_density(data=stream_char_cons,aes(x=urban_mean),col="grey50")+
  geom_vline(xintercept = median(stream_char_cons$urban_mean,na.rm=T),linetype="dashed",col="grey50")+
  scale_x_log10()+theme_classic()#+
#theme(axis.title=element_blank(),axis.text=element_blank())
p7=ggplot()+geom_density(data=nit_df,aes(x=MAP),col="black")+
  geom_vline(xintercept = median(nit_df$MAP),linetype="dashed",col="black")+
  geom_density(data=stream_char_cons,aes(x=MAP),col="grey50")+
  geom_vline(xintercept = median(stream_char_cons$MAT,na.rm=T),linetype="dashed",col="grey50")+
  theme_classic()#+
theme(axis.title=element_blank(),axis.text=element_blank())
p8=ggplot()+geom_density(data=nit_df,aes(x=MAT),col="black")+
  geom_vline(xintercept = median(nit_df$MAT),linetype="dashed",col="black")+
  geom_density(data=stream_char_cons,aes(x=MAT),col="grey50")+
  geom_vline(xintercept = median(stream_char_cons$MAT,na.rm=T),linetype="dashed",col="grey50")+
  theme_classic()#+
#theme(axis.title=element_blank(),axis.text=element_blank())
p9=ggplot()+geom_density(data=nit_df,aes(x=sand_mean),col="black")+
  geom_vline(xintercept = median(nit_df$sand_mean),linetype="dashed",col="black")+
  geom_density(data=stream_char_cons,aes(x=sand_mean),col="grey50")+
  geom_vline(xintercept = median(stream_char_cons$sand_mean,na.rm=T),linetype="dashed",col="grey50")+
  theme_classic()#+
# theme(axis.title=element_blank(),axis.text=element_blank())

layout <- "
ABCCDDEE
FFGGHHII
"
combined <- p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 +
  p9+
  plot_layout(design = layout)


ggsave(plot=combined,file="figures/figS2/figS2_char_dist.svg",width=6.0,height=2.50)

