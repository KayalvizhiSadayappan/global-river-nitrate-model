################################################################################
# Author : Kayalvizhi Sadayappan                                               # 
#                                                                              #
# This code was used to plot fig S12 in Climate Aridity                        #
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

output_folder <- file.path("figures","figS12")
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)

#read in predicted nitrate concentration and classes 
stream_nit_df <- read_delim(file="results/predicted_nitrate_concentrations_global.txt",
                            delim = "\t", escape_double = FALSE,trim_ws = TRUE)
global_river_segment_classes <- read_delim("data/raw/global_river_segment_classes.txt",
                                           delim = "\t", escape_double = FALSE, trim_ws = TRUE)
stream_nit_df <- stream_nit_df %>%
  left_join(global_river_segment_classes[,c("COMID","area_km2",
                                            "pop_2020","aridity_class")],
            by = "COMID") 

#free up memory
rm(global_river_segment_classes)

#assign range of concentrations to segments   
stream_nit_df <- stream_nit_df %>%
  mutate(
    mean_nit_levels = case_when(
      mean_nit <= 0.50 ~ "1. 0-0.5",
      mean_nit <= 1 ~ "2. 0.5-1",
      mean_nit <= 2.5 ~ "3. 1-2.5",
      mean_nit > 2.5 ~ "4. >2.5",
      TRUE ~ NA_character_
    )
  )

#get population and area under different concentration ranges in different aridity
#classes 
pie_df_AI_nit <- stream_nit_df[!is.na(stream_nit_df$aridity_class),] %>%
  dplyr::group_by(aridity_class,mean_nit_levels) %>%
  dplyr::summarise(pop = sum(pop_2020, na.rm = TRUE) / 1e9,
                   area = sum(area_km2,na.rm=TRUE)/1E6) %>%
  dplyr::mutate(per_pop = pop*100/sum(pop),
                per_area = area*100/sum(area))

#plot pie plots showing percent population under different concentration
#ranges in different aridity classes  Fig S12A
si_arid_pop=ggplot(pie_df_AI_nit[which(pie_df_AI_nit$aridity_class=="1. Humid"),], aes(x = "", y = pop, fill = mean_nit_levels)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#63C8FF", "#1F8DFF", "#2A45FF", "#0B1033")) +
  theme_void() + theme(legend.position = "none")+
  ggplot(pie_df_AI_nit[which(pie_df_AI_nit$aridity_class=="2. Sub-humid"),], aes(x = "", y = pop, fill = mean_nit_levels)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#63C8FF", "#1F8DFF", "#2A45FF", "#0B1033")) +
  theme_void() + theme(legend.position = "none")+
  ggplot(pie_df_AI_nit[which(pie_df_AI_nit$aridity_class=="3. Semi-arid"),], aes(x = "", y = pop, fill = mean_nit_levels)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#63C8FF", "#1F8DFF", "#2A45FF", "#0B1033")) +
  theme_void() + theme(legend.position = "none")+
  ggplot(pie_df_AI_nit[which(pie_df_AI_nit$aridity_class=="4. Arid"),], aes(x = "", y = pop, fill = mean_nit_levels)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#63C8FF", "#1F8DFF", "#2A45FF", "#0B1033")) +
  theme_void() + theme(legend.position = "none")+
  ggplot(pie_df_AI_nit[which(pie_df_AI_nit$aridity_class=="5. Hyper-arid"),], aes(x = "", y = pop, fill = mean_nit_levels)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#63C8FF", "#1F8DFF", "#2A45FF", "#0B1033")) +
  theme_void() + theme(legend.position = "none")+plot_layout(nrow=1)

#plot pie plots showing percent area under different concentration
#ranges in different aridity classes  Fig S12B
si_arid_area=ggplot(pie_df_AI_nit[which(pie_df_AI_nit$aridity_class=="1. Humid"),], aes(x = "", y = area, fill = mean_nit_levels)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#63C8FF", "#1F8DFF", "#2A45FF", "#0B1033")) +
  theme_void() + theme(legend.position = "none")+
  ggplot(pie_df_AI_nit[which(pie_df_AI_nit$aridity_class=="2. Sub-humid"),], aes(x = "", y = area, fill = mean_nit_levels)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#63C8FF", "#1F8DFF", "#2A45FF", "#0B1033")) +
  theme_void() + theme(legend.position = "none")+
  ggplot(pie_df_AI_nit[which(pie_df_AI_nit$aridity_class=="3. Semi-arid"),], aes(x = "", y = area, fill = mean_nit_levels)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#63C8FF", "#1F8DFF", "#2A45FF", "#0B1033")) +
  theme_void() + theme(legend.position = "none")+
  ggplot(pie_df_AI_nit[which(pie_df_AI_nit$aridity_class=="4. Arid"),], aes(x = "", y = area, fill = mean_nit_levels)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#63C8FF", "#1F8DFF", "#2A45FF", "#0B1033")) +
  theme_void() + theme(legend.position = "none")+
  ggplot(pie_df_AI_nit[which(pie_df_AI_nit$aridity_class=="5. Hyper-arid"),], aes(x = "", y = area, fill = mean_nit_levels)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#63C8FF", "#1F8DFF", "#2A45FF", "#0B1033")) +
  theme_void() + theme(legend.position = "none")+plot_layout(nrow=1)

ggsave("figures/figS12/figS12A_pop_area_dist.svg",
       si_arid_pop/si_arid_area,width=5.6,height=2.50)
