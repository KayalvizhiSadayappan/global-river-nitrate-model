################################################################################
# Author : Kayalvizhi Sadayappan                                               # 
#                                                                              #
# This code was used to plot fig S5 in Climate Aridity                         #
# Amplifies Global Riverine Nitrate Beyond Human Inputs paper                  # 
#                                                                              #
# Inputs: input files are from https://zenodo.org/records/21853129             #
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

root_folder <- "global-river-nitrate-model"
setwd(root_folder)

output_folder <- file.path("figures","figS5")
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)

# read in global river network data split over 8 MERIT files and combine them
global_river_lt <- list()
parent_folder = "data/raw/MERIT-Hydro_v0.7_v1.0/MERIT_Hydro_v07_Basins_v01/pfaf_level_01/"
for (sel_merit in seq(1,8,1)){
  MERIT_network <-  st_read(paste(parent_folder,"riv_pfaf_",sel_merit,
                                  "_MERIT_Hydro_v07_Basins_v01.shp",sep=""))
  global_river_lt[[sel_merit]] = MERIT_network
}
global_river = do.call(rbind,global_river_lt)
rm(global_river_lt)

#read in predicted standard deviation in nitrate concentrations, assign them to 
#river segments by COMID
stream_nit_df <- read_delim(file="results/predicted_nitrate_concentrations_global.txt",
                            delim = "\t", escape_double = FALSE,trim_ws = TRUE)
global_river_segment_classes <- read_delim("data/raw/global_river_segment_classes.txt",
                                           delim = "\t", escape_double = FALSE, trim_ws = TRUE)
rivers_joined <- global_river %>%
  left_join(stream_nit_df[,c("COMID","sd_nit")], by = "COMID")

#plot global river network of mean nitrate concentration 
# and their distribution - Fig. S5
rivers_joined$quartile_group <- cut(
  rivers_joined$sd_nit,
  breaks = c(0,0.025,0.050,0.100,0.200,3.2),
  include.lowest = TRUE,right = TRUE,
  labels = c("0-0.025", "0.025-0.050", "0.050-0.100", "0.100-0.200", "0.200-3.100"))
river_plot_mean= ggplot(data = rivers_joined) +
  geom_sf(aes(color = quartile_group),lwd = 0.05) +
  scale_color_manual(values=c("#a0e2ff","#63c8ff", "#1f8dff", "#2a45ff", "#0b1033"))+
  theme_classic()+theme(legend.position = "none", axis.line = element_blank(),
                        axis.text = element_blank(),
                        axis.ticks = element_blank())
#global maps are too large to be saved as svg
ggsave("figures/figS5/figS5_map.png", plot = river_plot_mean,
       dpi = 1000, width = 6.50, height = 3.50)

#assign continents to segments
stream_nit_df$continent = global_river_segment_classes$continent[match(stream_nit_df$COMID,
                                                                       global_river_segment_classes$COMID)]
stream_nit_df <- stream_nit_df %>%
  mutate(class = case_match(
    continent,
    "North America" ~ "NA",
    "South America" ~ "SA",
    "Europe" ~ "EU",
    "Africa" ~ "AF",
    "Asia" ~ "AS",
    "Oceania" ~ "OC",
    .default = continent 
  )) %>% 
  mutate(class = factor(class,
                        levels = c("NA","SA","AS","OC","AF","EU")))

# density distribution - global and continent-wise
density_plot_global_mean=ggplot(stream_nit_df,
                                aes(x=sd_nit,fill="transparent"))+
  geom_density(fill="transparent")+
  scale_x_log10()+
  geom_vline(xintercept=median(stream_nit_df$sd_nit),lty="dashed")+
  theme_classic()+
  theme(legend.position = "none")
density_plot_cont_mean=ggplot(stream_nit_df[!is.na(stream_nit_df$class),],
                              aes(x=sd_nit,y=class,
                                  fill="transparent"))+
  geom_density_ridges2(fill="transparent")+
  scale_x_log10()+
  stat_density_ridges(fill="transparent",quantile_lines = TRUE,quantile_fun = median,linetype=2)+
  theme_classic()+
  theme(panel.border = element_blank())
ggsave("figures/figS5/figS5_global_dist.svg", plot = density_plot_global_mean,
       width=1.6,height=0.7)
ggsave("figures/figS5/figS5_cont_dist.svg", plot = density_plot_cont_mean,
       width=1.6,height=0.7)

#median values on density distribution - global and continent-wise
median(stream_nit_df$sd_nit)
stream_nit_df[!is.na(stream_nit_df$class),] %>%
  group_by(class) %>%
  summarise(median_val = median(sd_nit))

