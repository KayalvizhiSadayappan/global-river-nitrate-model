################################################################################
# Author : Kayalvizhi Sadayappan                                               # 
#                                                                              #
# This code was used to plot fig 1 in Climate Aridity                          #
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
output_folder <- file.path("figures","fig1")
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)

# read in global river network data split over 8 MERIT files and combine them
global_river_lt <- list()
parent_folder = "data/MERIT-Hydro_v0.7_v1.0/MERIT_Hydro_v07_Basins_v01/pfaf_level_01/"
for (sel_merit in seq(1,8,1)){
  MERIT_network <-  st_read(paste(parent_folder,"riv_pfaf_",sel_merit,
                              "_MERIT_Hydro_v07_Basins_v01.shp",sep=""))
  global_river_lt[[sel_merit]] = MERIT_network
}
global_river = do.call(rbind,global_river_lt)
rm(global_river_lt)

# read in global subcatchment boundary data split over 8 MERIT files and combine them
global_basin_lt<- list()
target_crs=st_crs(global_river)              
for (sel_merit in seq(1,8,1)){
  global_basin_lt[[sel_merit]]=st_read(paste(parent_folder,"cat_pfaf_",sel_merit,"_MERIT_Hydro_v07_Basins_v01.shp",sep=""))
}
global_basin=do.call(rbind,global_basin_lt)
st_crs(global_basin)=target_crs;rm(global_basin_lt)

#read in predicted nitrate concentrations, assign them to river segments by COMID
stream_nit_df <- read_delim(file="data/predicted_nitrate_concentrations_global.txt",
                         delim = "\t", escape_double = FALSE,trim_ws = TRUE)
global_river_segment_classes <- read_delim("~/data/raw/global_river_segment_classes.txt",
                                           delim = "\t", escape_double = FALSE, trim_ws = TRUE)
rivers_joined <- global_river %>%
  left_join(stream_nit_df[,c("COMID","mean_nit","cv_nit")], by = "COMID")

#plot global river network of mean nitrate concentration 
# and their distribution - Fig. 1A
rivers_joined$quartile_group <- cut(
  rivers_joined$mean_nit,
  breaks = c(0,0.25,0.5,1,2.5,100),
  include.lowest = TRUE,right = TRUE,
  labels = c("0-0.25", "0.25-0.5", "0.5-1.0", "1.0-2.5", ">2.5"))
river_plot_mean= ggplot(data = rivers_joined) +
  geom_sf(aes(color = quartile_group),lwd = 0.05) +
  scale_color_manual(values=c("#a0e2ff","#63c8ff", "#1f8dff", "#2a45ff", "#0b1033"))+
  theme_classic()+theme(legend.position = "none", axis.line = element_blank(),
                        axis.text = element_blank(),
                        axis.ticks = element_blank())
#global maps are too large to be saved as svg
ggsave("figures/fig1/fig1A_map.png", plot = river_plot_mean,
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
                         aes(x=mean_nit,fill="transparent"))+geom_density(fill="transparent")+
  scale_x_log10()+
  geom_vline(xintercept=median(stream_nit_df$mean_nit),lty="dashed")+
  theme_classic()+
  theme(legend.position = "none")
density_plot_cont_mean=ggplot(stream_nit_df[!is.na(stream_nit_df$class),],
                    aes(x=mean_nit,y=class,
                        fill="transparent"))+geom_density_ridges2(fill="transparent")+
  scale_x_log10()+
  stat_density_ridges(fill="transparent",quantile_lines = TRUE,quantile_fun = median,linetype=2)+
  theme_classic()+
  theme(panel.border = element_blank())
ggsave("figures/fig1/fig1A_global_dist.svg", plot = density_plot_global_mean,
       width=1.6,height=0.7)
ggsave("figures/fig1/fig1A_cont_dist.svg", plot = density_plot_cont_mean,
       width=1.6,height=0.7)

#median values on density distribution - global and continent-wise
median(stream_nit_df$mean_nit)
stream_nit_df[!is.na(stream_nit_df$class),] %>%
  group_by(class) %>%
  summarise(median_val = median(mean_nit))

#plot global river network of coefficient of variability in 
#mean nitrate concentration and their distribution - Fig. 1B
rivers_joined$quartile_group <- cut(
  rivers_joined$cv_nit,
  breaks = c(2-0.1,20,23,26,29,132),
  include.lowest = TRUE,right = TRUE,
  labels = c("2-20", "20-23", "23-26", "26-29", "29-132"))
river_plot_cv= ggplot(data = rivers_joined) +
  geom_sf(aes(color = quartile_group),lwd = 0.05) +
  scale_color_manual(values=c("#a0e2ff","#63c8ff", "#1f8dff", "#2a45ff", "#0b1033"))+
  theme_classic()+theme(legend.position = "none", axis.line = element_blank(),
                        axis.text = element_blank(),
                        axis.ticks = element_blank())
ggsave("/scratch/kps5778/fig1B_map.png", plot = river_plot_cv,
       dpi = 1000, width = 6.50, height = 3.50)


# density distribution - global and continent-wise
density_plot_global_cv=ggplot(stream_nit_df,
                                aes(x=cv_nit,fill="transparent"))+geom_density(fill="transparent")+
  geom_vline(xintercept=median(stream_nit_df$cv_nit),lty="dashed")+
  theme_classic()+
  theme(legend.position = "none")
density_plot_cont_cv=ggplot(stream_nit_df[!is.na(stream_nit_df$class),],
                              aes(x=cv_nit,y=class,
                                  fill="transparent"))+
  geom_density_ridges2(fill="transparent")+
  stat_density_ridges(fill="transparent",quantile_lines = TRUE,quantile_fun = median,linetype=2)+
  theme_classic()+
  theme(panel.border = element_blank())
ggsave("figures/fig1/fig1B_global_dist.svg", plot = density_plot_global_cv,
       width=1.6,height=0.7)
ggsave("figures/fig1/fig1B_cont_dist.svg", plot = density_plot_cont_cv,
       width=1.6,height=0.7)

#median values on density distribution - global and continent-wise
median(stream_nit_df$cv_nit)
stream_nit_df[!is.na(stream_nit_df$class),] %>%
  group_by(class) %>%
  summarise(median_val = median(cv_nit))

#read and plot distribution of model performance of 1000 model ensemble - Fig 1C
model_perf <- read_delim("results/BRT_model_performance.txt", 
                                    delim = "\t", escape_double = FALSE, 
                                    trim_ws = TRUE)
model_metrics_df <- model_perf %>%
  pivot_longer(
    cols = -model_run,
    names_to = c("identifier", "Metric"),
    names_sep = "_",
    values_to = "Value"
  ) %>%
  mutate(
    identifier = recode(identifier, "train" = "1_train", "test" = "2_test")
  )

bias_plot=ggplot()+geom_boxplot(data=model_metrics_df[which(model_metrics_df$Metric=="PBIAS"),],
                                aes(x=identifier,y=Value,col=identifier),outlier.size = 0.25)+
  scale_color_manual(values=c("grey","black"))+theme_classic()+
  coord_cartesian(ylim=c(-25,5))+
  theme(axis.title=element_blank(),axis.line.x = element_blank(),
        axis.ticks.x=element_blank(),axis.text.x = element_blank(),
        axis.text= element_text(family = "Arial"),legend.position = "none")
rmse_plot=ggplot()+geom_boxplot(data=model_metrics_df[which(model_metrics_df$Metric=="RMSE"),],
                                aes(x=identifier,y=Value,col=identifier),outlier.size = 0.25)+
  scale_color_manual(values=c("grey","black"))+theme_classic()+
  coord_cartesian(ylim=c(0.0,1.4))+
  theme(axis.title=element_blank(),axis.line.x = element_blank(),
        axis.ticks.x=element_blank(),axis.text.x = element_blank(),
        axis.text= element_text(family = "Arial"),legend.position = "none")
nse_plot=ggplot()+geom_boxplot(data=model_metrics_df[which(model_metrics_df$Metric%in% c("NSE")),],
                               aes(x=identifier,y=Value,col=identifier),outlier.size = 0.25)+
  scale_color_manual(values=c("grey","black"))+theme_classic()+
  coord_cartesian(ylim=c(0.0,0.94))+
  theme(axis.title=element_blank(),axis.line.x = element_blank(),
        axis.ticks.x=element_blank(),axis.text.x = element_blank(),
        axis.text= element_text(family = "Arial"),legend.position = "none")
kge_plot=ggplot()+geom_boxplot(data=model_metrics_df[which(model_metrics_df$Metric%in% c("KGE")),],
                               aes(x=identifier,y=Value,col=identifier),outlier.size = 0.25)+
  scale_color_manual(values=c("grey","black"))+theme_classic()+
  coord_cartesian(ylim=c(0.0,0.94))+
  theme(axis.title=element_blank(),axis.line.x = element_blank(),
        axis.ticks.x=element_blank(),axis.text.x = element_blank(),
        axis.text= element_text(family = "Arial"),legend.position = "none")

perf_plot=nse_plot + kge_plot + bias_plot + rmse_plot +
  plot_layout(nrow=1)
ggsave("figures/fig1/fig1C_model_perf.svg", 
       plot = perf_plot,dpi=1000,width=2.0,height=1)

#plot observed nitrate concentrations in world map - Fig 1D
nit_df  <- read_delim(file="data/raw/BRT_train_data_7213_catchments.txt",
                  delim = "\t", escape_double = FALSE,trim_ws = TRUE)

#get location of sites from GRQA database
NO3N_GRQA <- read_delim("data/raw/GRQA/GRQA_data_v1.3/NO3N_GRQA.csv", 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)
site_loc = NO3N_GRQA %>%
  filter(site_id %in% nit_df$site_id) %>%
  group_by(site_id) %>%
  summarise(lon_wgs84=unique(lon_wgs84),lat_wgs84=unique(lat_wgs84))
site_loc$mean_nit = nit_df$mean_nit[match(site_loc$site_id,nit_df$site_id)]

sites_sf  <- st_as_sf(site_loc, coords = c("lon_wgs84", "lat_wgs84"), crs = 4326)
sites_sf$quartile_group <- cut(
  sites_sf$mean_nit,
  breaks = c(0,0.25,0.5,1,2.5,100),
  include.lowest = TRUE,right = TRUE,
  labels = c("0-0.25", "0.25-0.5", "0.5-1.0", "1.0-2.5", ">2.5"))
map_sites = ggplot()+
  geom_sf(data = world_map[which(world_map$continent!="Antarctica" &
                                   world_map$admin!="Greenland"),],color="grey",fill="transparent")+
  geom_sf(data = sites_sf, 
          aes(color = quartile_group),size = 0.5,pch=16) +
  scale_color_manual(values=c("#a0e2ff","#63c8ff", "#1f8dff", "#2a45ff", "#0b1033"))+
  theme_classic()+theme(legend.position = "none",axis.line=element_blank(),axis.text = element_blank(),
                        axis.ticks=element_blank())
ggsave("figures/fig1/fig1D_global_obs_nit.svg", plot = map_sites, width=6.50,height=3.50)

#global map of human land use - Fig 1E
global_basin_join <- left_join(global_basin,
                               global_river_segment_classes[,c("COMID","human_landuse")], by = "COMID")
global_basin_join$quartile_group <- cut(
  global_basin_join$human_landuse, breaks = c(0,0.00001,2,20,80,100),
  include.lowest = TRUE,right = TRUE,
  labels = c("Q1", "Q2", "Q3", "Q4", "Q5"))
fig_lu=ggplot(global_basin_join) +
  geom_sf(aes(fill = quartile_group), color = NA) +
  scale_fill_manual(values=c("#a0e2ff","#63c8ff", "#1f8dff", "#2a45ff", "#0b1033"))+
  theme_classic()+theme(legend.position = "none", axis.line = element_blank(),
                        axis.text = element_blank(),
                        axis.ticks = element_blank())
ggsave("figures/fig1/fig1E_map.png", plot = fig_lu,
       dpi = 1000, width = 6.50, height = 3.50)

#map of aridity classes - Fig 1F
global_basin_join <- left_join(global_basin,global_river_segment_classes[,c("COMID","aridity_class")],
                               by = "COMID")
fig_AI=ggplot(global_basin_join) +
  geom_sf(aes(fill = aridity_class), color = NA) +
  scale_fill_manual(values=c("#a0e2ff","#63c8ff", "#1f8dff", "#2a45ff", "#0b1033"))+
  theme_classic()+theme(legend.position = "none", axis.line = element_blank(),
                        axis.text = element_blank(),
                        axis.ticks = element_blank())
ggsave("figures/fig1/fig1F_map.png", plot = fig_AI,
       dpi = 1000, width = 6.50, height = 3.50)

#density distributions of CV in different landuse and aridity classes - Fig 1G
stream_nit_df = left_join(stream_nit_df, global_river_segment_classes[c("COMID","landuse_class","aridity_class")],
                          by = "COMID")
density_plot_landuse=ggplot(stream_nit_df[!is.na(stream_nit_df$landuse_class),],
                            aes(x=cv_nit,y=landuse_class,color = landuse_class,
                                fill="transparent"))+
  geom_density_ridges2(fill="transparent")+
  scale_color_manual(values=c("#63c8ff", "#1f8dff", "#2a45ff", "#0b1033"))+
  stat_density_ridges(fill="transparent",quantile_lines = TRUE,quantile_fun = median,linetype=2)+
  theme_classic()+
  coord_cartesian(xlim=c(0,50))+
  theme(panel.border = element_blank())

density_plot_aridity=ggplot(stream_nit_df[!is.na(stream_nit_df$aridity_class),],
                            aes(x=cv_nit,y=aridity_class,color=aridity_class,
                                fill="transparent"))+
  geom_density_ridges2(fill="transparent")+
  scale_color_manual(values=c("#a0e2ff","#63c8ff", "#1f8dff", "#2a45ff", "#0b1033"))+
  stat_density_ridges(fill="transparent",quantile_lines = TRUE,quantile_fun = median,linetype=2)+
  theme_classic()+
  coord_cartesian(xlim=c(0,50))+
  theme(panel.border = element_blank())
ggsave("figures/fig1/fig1G_landuse_dist.svg", plot = density_plot_landuse,
       width=1.6,height=0.7)
ggsave("figures/fig1/fig1G_aridity_dist.svg", plot = density_plot_aridity,
       width=1.6,height=0.7)

#median values on density distribution - global and continent-wise
stream_nit_df[!is.na(stream_nit_df$landuse_class),] %>%
  group_by(landuse_class) %>%
  summarise(median_val = median(cv_nit))
stream_nit_df[!is.na(stream_nit_df$aridity_class),] %>%
  group_by(aridity_class) %>%
  summarise(median_val = median(cv_nit))
