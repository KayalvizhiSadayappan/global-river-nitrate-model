################################################################################
# Author : Kayalvizhi Sadayappan                                               # 
#                                                                              #
# This code was used to plot fig S9 in Climate Aridity                        #
# Amplifies Global Riverine Nitrate Beyond Human Inputs paper                  # 
#                                                                              #
# Inputs: input files are from https://zenodo.org/records/21853129             #
#                                                                              #
# R version 4.3.2.was used - Package versions in comments                      #
################################################################################
library(sf)# ‘1.0.16’
library(readr)# ‘2.1.5’
library(dplyr)# ‘1.1.4’
library(tidyr)# ‘1.3.1’
library(ggplot2)# ‘3.5.1’
library(patchwork)# ‘1.3.0’
library(rnaturalearth)#‘1.1.0’

root_folder <- "global-river-nitrate-model"
setwd(root_folder)

output_folder <- file.path("figures","figS9")
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)

#read in predicted nitrate concentration and classes 
stream_nit_df <- read_delim(file="results/predicted_nitrate_concentrations_global.txt",
                            delim = "\t", escape_double = FALSE,trim_ws = TRUE)
global_river_segment_classes <- read_delim("data/raw/global_river_segment_classes.txt",
                                           delim = "\t", escape_double = FALSE, trim_ws = TRUE)
stream_nit_df <- stream_nit_df %>%
  left_join(global_river_segment_classes[,c("COMID","Q_mmyr","landuse_class")],
            by = "COMID") 

#free up memory
rm(global_river_segment_classes)


#scatter plots of aridity index vs predicted nitrate concentration - Fig 2B
si_q=ggplot(stream_nit_df,aes(x=Q_mmyr,y=mean_nit))+
  geom_point(color="grey",size=0.25,alpha=0.2)+
  geom_smooth(method = "lm", formula = y ~ x,col="black",linewidth=2) +
  geom_smooth(data=stream_nit_df[which(stream_nit_df$landuse_class=="3. urban"),],
              aes(x=Q_mmyr,y=mean_nit),method = "lm", formula = y ~ x,col="#1F8DFF",linewidth=2) +
  geom_smooth(data=stream_nit_df[which(stream_nit_df$landuse_class=="4. agri"),],
              aes(x=Q_mmyr,y=mean_nit),method = "lm", formula = y ~ x,col="#2A45FF",linewidth=2) +
  geom_smooth(data=stream_nit_df[which(stream_nit_df$landuse_class=="1. undev"),],
              aes(x=Q_mmyr,y=mean_nit),method = "lm", formula = y ~ x,col="#63c8ff",linewidth=2) +
  theme_classic()+scale_x_log10(limits=c(0.001,7000))+
  scale_y_log10(limits=c(0.005,12.5))

si_q_agri=ggplot(stream_nit_df[which(stream_nit_df$landuse_class=="4. agri"),],
                  aes(x=Q_mmyr,y=mean_nit))+
  geom_point(color="grey",size=0.25,alpha=0.2)+
  geom_smooth(method = "lm", formula = y ~ x,col="#2A45FF",linewidth=2) +
  theme_classic()+scale_x_log10(limits=c(0.001,7000))+
  scale_y_log10(limits=c(0.005,12.5))

si_q_undev=ggplot(stream_nit_df[which(stream_nit_df$landuse_class=="1. undev"),],
                   aes(x=Q_mmyr,y=mean_nit))+
  geom_point(color="grey",size=0.25,alpha=0.2)+
  geom_smooth(method = "lm", formula = y ~ x,col="#63c8ff",linewidth=2) +
  theme_classic()+scale_x_log10(limits=c(0.001,7000))+
  scale_y_log10(limits=c(0.005,12.5))

si_q_urban=ggplot(stream_nit_df[which(stream_nit_df$landuse_class=="3. urban"),],
                   aes(x=Q_mmyr,y=mean_nit))+
  geom_point(color="grey",size=0.25,alpha=0.2)+
  geom_smooth(method = "lm", formula = y ~ x,col="#1F8DFF",linewidth=2) +
  theme_classic()+scale_x_log10(limits=c(0.001,7000))+
  scale_y_log10(limits=c(0.005,12.5))

#figure saved in png format as svg file for this can be very large
ggsave("figures/figS9/figS9_Q_mean_nit_scatter.png",
       plot = si_q+
         theme(legend.position = "none",axis.text = element_blank(),axis.title = element_blank(),
               panel.background = element_rect(fill = "transparent", color = NA),
               plot.background  = element_rect(fill = "transparent", color = NA)),
       dpi=1000,width=5.0,height=5.0, bg = "transparent")
ggsave("figures/figS9/figS9_Q_mean_nit_scatter_undev.svg",
       plot = si_q_undev+theme(legend.position = "none",axis.text = element_blank(),axis.title = element_blank(),
                                panel.background = element_rect(fill = "transparent", color = NA),
                                plot.background  = element_rect(fill = "transparent", color = NA)),
       dpi=1000,width=1.25,height=1.25, bg = "transparent")
ggsave("figures/figS9/figS9_Q_mean_nit_scatter_urban.png",
       plot = si_q_urban+theme(legend.position = "none",axis.text = element_blank(),axis.title = element_blank(),
                                panel.background = element_rect(fill = "transparent", color = NA),
                                plot.background  = element_rect(fill = "transparent", color = NA)),
       dpi=1000,width=1.25,height=1.25, bg = "transparent")
ggsave("figures/figS9/figS9_Q_mean_nit_scatter_agri.png",
       plot = si_q_agri+theme(legend.position = "none",axis.text = element_blank(),axis.title = element_blank(),
                               panel.background = element_rect(fill = "transparent", color = NA),
                               plot.background  = element_rect(fill = "transparent", color = NA)),
       dpi=1000,width=1.25,height=1.25, bg = "transparent")

