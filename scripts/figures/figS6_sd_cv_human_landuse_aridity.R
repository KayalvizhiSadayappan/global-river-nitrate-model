################################################################################
# Author : Kayalvizhi Sadayappan                                               # 
#                                                                              #
# This code was used to plot fig S6 in Climate Aridity                         #
# Amplifies Global Riverine Nitrate Beyond Human Inputs paper                  # 
#                                                                              #
# Inputs: 1) MERIT Hydro river network can be downloaded from                  #
# https://www.reachhydro.org/home/params/merit-basins (download                #
# MERIT-Basins hydrography (based on MERIT-Hydro v0.7/v1.0))                   #
# 2) Download GRQA_data_v1.3.zip from                                          # 
# https://zenodo.org/records/7056647                                           #
# 3) Downloaded BRT_train_data_7213_catchments.txt from                        # 
# https://github.com/KayalvizhiSadayappan/global-river-nitrate-model/          # 
# 4) Other input files are from                                                #
#  https://zenodo.org/records/21853129                                         #
#                                                                              #
# R version 4.3.2.was used - Package versions in comments                      #
# Plotting global maps needed high memory and upto 240GB was allocated         #
# other plots don't require such immense memory                                #
################################################################################

library(sf)# ‘1.0.16’
library(readr)# ‘2.1.5’
library(dplyr)# ‘1.1.4’
library(tidyr)# ‘1.3.1’
library(ggplot2)# ‘3.5.1’
library(patchwork)# ‘1.3.0’
library(rnaturalearth)#‘1.1.0’
library(ggridges) #‘0.5.6’

#set root folder (user specific) as working directory
root_folder <- "/global-river-nitrate-model/"
setwd(root_folder)

#create output folder for saving figures
output_folder <- file.path("figures","figS6")
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)

#read in predicted nitrate concentrations, assign them to river segments by COMID
stream_nit_df <- read_delim(file="results/predicted_nitrate_concentrations_global.txt",
                         delim = "\t", escape_double = FALSE,trim_ws = TRUE)
global_river_segment_classes <- read_delim("data/raw/global_river_segment_classes.txt",
                                           delim = "\t", escape_double = FALSE, trim_ws = TRUE)
stream_nit_df <- stream_nit_df %>%
  left_join(global_river_segment_classes[,c("COMID","human_landuse","aridity_index")], by = "COMID")

#plot distribution of standard deviation and coefficient of variability in nitrate
#concentrations as function of human landuse and aridity - Fig S6
si=ggplot(stream_nit_df[stream_nit_df$aridity_index<10,], 
       aes(x = human_landuse, y = aridity_index, z = sd_nit)) +
  stat_summary_hex(
    aes(fill = cut(after_stat(value),
                   breaks = c(0, 0.2, 0.3, 0.4, 1.0),
                   labels = c("0-0.2", "0.2-0.3", "0.3-0.4", "0.4-1"))),
    fun = median,
    bins = 25,
    colour = NA
  ) +
  scale_fill_manual(
    values = c("#9BEBFF", "#5CC8FF","#2A35FF", "#0A0A3C"),
    drop = FALSE,
    na.value = "grey90"
  ) +
  scale_x_continuous(breaks = seq(0, 100, 25)) +
  scale_y_continuous(breaks = seq(0, 10, 2.5)) +
  labs(title = "SD NO3-N, %", x = "Human landuse, %", y = "Aridity Index", fill = NULL) +
  theme_minimal(base_size = 9) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "grey92"),
    plot.title = element_text(hjust = 0.5,size = 15)
  ) +
ggplot(stream_nit_df[stream_nit_df$aridity_index<10,], 
       aes(x = human_landuse, y = aridity_index, z = cv_nit)) +
  stat_summary_hex(
    aes(fill = cut(after_stat(value),
                   breaks = c(16, 20, 24, 28, 32),
                   labels = c("16-20", "20-24", "24-28", "28-32"))),
    fun = median,
    bins = 25,
    colour = NA
  ) +
  scale_fill_manual(
    values = c("16-20" = "#9BEBFF",
               "20-24" = "#5CC8FF",
               "24-28" = "#2A35FF",
               "28-32" = "#0A0A3C"),
    drop = FALSE,
    na.value = "grey90"
  ) +
  scale_x_continuous(breaks = seq(0, 100, 25)) +
  scale_y_continuous(breaks = seq(0, 10, 2.5)) +
  labs(title = "CV NO3-N, %", x = "Human landuse, %", y = "Aridity Index", fill = NULL) +
  theme_minimal(base_size = 9) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "grey92"),
    plot.title = element_text(hjust = 0.5,size = 15)
  )
ggsave(plot=si,file="figures/figS6/figS6_sd_cv_dist.svg",width=6.0,height=3.50)

