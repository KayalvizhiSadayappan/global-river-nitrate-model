################################################################################
# Author : Kayalvizhi Sadayappan                                               # 
#                                                                              #
# This code was used to plot fig 3 in Climate Aridity                          #
# Amplifies Global Riverine Nitrate Beyond Human Inputs paper                  # 
#                                                                              #
# Inputs: MERIT Hydro river catchment shapefile can be downloaded from         #
# https://www.reachhydro.org/home/params/merit-basins (download                #
# MERIT-Basins hydrography (based on MERIT-Hydro v0.7/v1.0)) and other         #
# input files are from https://zenodo.org/records/21853129                     #
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

output_folder <- file.path("figures","fig3")
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)

world_map <- ne_countries(scale = "medium", returnclass = "sf")

parent_folder = "data/raw/MERIT-Hydro_v0.7_v1.0/MERIT_Hydro_v07_Basins_v01/pfaf_level_01/"
# read in global subcatchment boundary data split over 8 MERIT files and combine them
global_basin_lt<- list()
for (sel_merit in seq(1,8,1)){
  global_basin_lt[[sel_merit]] = st_read(paste(parent_folder,"cat_pfaf_",sel_merit,"_MERIT_Hydro_v07_Basins_v01.shp",sep=""))
}
global_basin=do.call(rbind,global_basin_lt)
merit_riv <- st_read(paste(parent_folder,"riv_pfaf_8_MERIT_Hydro_v07_Basins_v01.shp",sep=""))
target_crs <- st_crs(merit_riv); rm(merit_riv)
st_crs(global_basin)=target_crs;rm(global_basin_lt)

#read in predicted nitrate concentration and classes 
stream_nit_df <- read_delim(file="results/predicted_nitrate_concentrations_global.txt",
                            delim = "\t", escape_double = FALSE,trim_ws = TRUE)
global_river_segment_classes <- read_delim("data/raw/global_river_segment_classes.txt",
                                           delim = "\t", escape_double = FALSE, trim_ws = TRUE)
stream_char_cons <- read_delim(file="data/raw/BRT_input_data_global.txt",
                               delim = "\t", escape_double = FALSE,trim_ws = TRUE)
basin_joined <- global_basin %>%
  left_join(stream_nit_df[,c("COMID","mean_nit")], by = "COMID") %>%
  left_join(global_river_segment_classes[,c("COMID","area_km2","pop_2020","rural_urban_class",
                                            "income_class","aridity_class","landuse_class")],
            by = "COMID") %>%
  left_join(stream_char_cons[,c("COMID","max_order")], by = "COMID")

#free up memory
rm(global_basin,stream_nit_df,global_river_segment_classes,stream_char_cons)

#combine all segments of order exceeding 3 into single bin  
basin_joined <- basin_joined %>%
  mutate(
    order_class = case_when(
      max_order <= 1 ~ "1. 1",
      max_order <= 2 ~ "2. 2",
      max_order <= 3 ~ "3. 3",
      max_order > 3 ~ "5. 4 or higher",
      TRUE ~ NA_character_
    )
  )
#assign range of concentrations to segments   
basin_joined <- basin_joined %>%
  mutate(
    mean_nit_levels = case_when(
      mean_nit <= 0.50 ~ "1. 0-0.5",
      mean_nit <= 1 ~ "2. 0.5-1",
      mean_nit <= 2.5 ~ "3. 1-2.5",
      mean_nit > 2.5 ~ "4. >2.5",
      TRUE ~ NA_character_
    )
  )

#indices of segments with concentrations exceeding fixed thresholds  
ind_2_5=which(basin_joined$mean_nit>2.5)
ind_1=which(basin_joined$mean_nit>1)
ind_0_5=which(basin_joined$mean_nit>0.5)

#global map showing the distribution of population density in areas where river
#nitrate concentration exceeds 0.5 mg/L - Fig 3A
fig3_A=ggplot(basin_joined[ind_0_5,]) +
  geom_sf(data = world_map[which(world_map$continent!="Antarctica" &
                                   world_map$admin!="Greenland"),],color="grey",fill="transparent")+
  geom_sf(aes(fill = rural_urban_class,color =rural_urban_class),linewidth=0.01) +
  scale_fill_manual(values=c("#63C8FF", "#1F8DFF", "#2A45FF","#0B1033"),#,
                    #"#A3F0FF", "#63C8FF", "#1F8DFF", "#2A45FF"),
                    na.value = "grey")+
  scale_color_manual(values=c("#63C8FF", "#1F8DFF", "#2A45FF","#0B1033"),#,
                     #"#A3F0FF", "#63C8FF", "#1F8DFF", "#2A45FF", "#0B1033"),
                     na.value = "grey")+
  theme_classic()+theme(axis.line=element_blank(),axis.text = element_blank(),
                        axis.ticks=element_blank(),legend.position = "none")
ggsave("figures/fig3/fig3A_global_dist_0_5.png",
       plot = fig3_A,dpi=1000,width=5.00,height=2.50)

#global map showing the distribution of population density in areas where river
#nitrate concentration exceeds 1.0 mg/L - Fig 3B
fig3_B=ggplot(basin_joined[ind_1,]) +
  geom_sf(data = world_map[which(world_map$continent!="Antarctica" &
                                   world_map$admin!="Greenland"),],color="grey",fill="transparent")+
  geom_sf(aes(fill = rural_urban_class,color =rural_urban_class),linewidth=0.01) +
  scale_fill_manual(values=c( "#63C8FF", "#1F8DFF", "#2A45FF","#0B1033"),#,
                    #"#A3F0FF", "#63C8FF", "#1F8DFF", "#2A45FF", "#0B1033"),
                    na.value = "grey")+
  scale_color_manual(values=c( "#63C8FF", "#1F8DFF", "#2A45FF","#0B1033"),#,
                     #"#A3F0FF", "#63C8FF", "#1F8DFF", "#2A45FF", "#0B1033"),
                     na.value = "grey")+
  theme_classic()+theme(axis.line=element_blank(),axis.text = element_blank(),
                        axis.ticks=element_blank(),legend.position = "none")
ggsave("figures/fig3/fig3B_global_dist_1_0.png",
       plot = fig3_B,dpi=1000,width=5.00,height=2.50)

#global map showing the distribution of population density in areas where river
#nitrate concentration exceeds 2.5 mg/L - Fig 3C
fig3_C=ggplot(basin_joined[ind_2_5,]) +
  geom_sf(data = world_map[which(world_map$continent!="Antarctica" &
                                   world_map$admin!="Greenland"),],color="grey",fill="transparent")+
  geom_sf(aes(fill = rural_urban_class,color =rural_urban_class),linewidth=0.01) +
  scale_fill_manual(values=c("#63C8FF", "#1F8DFF", "#2A45FF","#0B1033"),#,
                    #"#A3F0FF", "#63C8FF", "#1F8DFF", "#2A45FF", "#0B1033"),
                    na.value = "grey")+
  scale_color_manual(values=c("#63C8FF", "#1F8DFF", "#2A45FF","#0B1033"),#,
                     #"#A3F0FF", "#63C8FF", "#1F8DFF", "#2A45FF", "#0B1033"),
                     na.value = "grey")+
  theme_classic()+theme(axis.line=element_blank(),axis.text = element_blank(),
                        axis.ticks=element_blank(),legend.position = "none")
ggsave("figures/fig3/fig3C_global_dist_2_5.png",
       plot = fig3_C,dpi=1000,width=5.00,height=2.50)

#Pie chart showing percent global population living near river with different
#nitrate concentrations - Fig 3D
pie_df <- basin_joined %>%
  st_drop_geometry() %>%
  dplyr::group_by(mean_nit_levels) %>%
  dplyr::summarise(pop = sum(pop_2020, na.rm = TRUE) / 1e9,
                   area = sum(area_km2,na.rm=TRUE)/1E6)
#numbers on the plot
pie_df = pie_df %>% 
  mutate(per_pop = pop*100/sum(pop),
         per_area = area*100/sum(area))

si_pie_pop=ggplot(pie_df, aes(x = "", y = pop, fill = mean_nit_levels)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#63C8FF", "#1F8DFF", "#2A45FF", "#0B1033")) +
  theme_void() + theme(legend.position = "none")
ggsave("figures/fig3/fig3D_pie_pop.svg",
       si_pie_pop,width=5,height=5)

#Pie chart showing percent area near river with different
#nitrate concentrations - Fig 3E
si_pie_area=ggplot(pie_df, aes(x = "", y = area, fill = mean_nit_levels)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#63C8FF", "#1F8DFF", "#2A45FF", "#0B1033")) +
  theme_void() + theme(legend.position = "none")
ggsave("figures/fig3/fig3E_pie_area.svg",
       si_pie_area,width=5,height=5)

#Pie chart showing percent population under different rural-urban and income classes 
#considering only rivers with nitrate concentrations exceeding 2.5 mg/L - Fig 3F
pie_df_2_5_popden <- basin_joined[!is.na(basin_joined$rural_urban_class),]  %>%
  st_drop_geometry() %>%
  filter(mean_nit_levels == "4. >2.5") %>%
  dplyr::group_by(rural_urban_class) %>%
  dplyr::summarise(pop = sum(pop_2020, na.rm = TRUE) / 1e9) %>% 
  mutate(per_pop = pop*100/sum(pop))

si_pie_pop_2_5_popden=ggplot(pie_df_2_5_popden, 
                             aes(x = "", y = pop, fill = rural_urban_class)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#63C8FF", "#1F8DFF", "#2A45FF", "#0B1033")) +
  theme_void() + theme(legend.position = "none")
ggsave("figures/fig3/fig3F_pie_pop_popden.svg",
       si_pie_pop_2_5_popden,width=5,height=5)

pie_df_2_5_income <- basin_joined[!is.na(basin_joined$income_class),]  %>%
  st_drop_geometry() %>%
  filter(mean_nit_levels == "4. >2.5") %>%
  dplyr::group_by(income_class) %>%
  dplyr::summarise(pop = sum(pop_2020, na.rm = TRUE) / 1e9) %>% 
  mutate(per_pop = pop*100/sum(pop))

si_pie_pop_2_5_income=ggplot(pie_df_2_5_income, 
                             aes(x = "", y = pop, fill = income_class)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#63C8FF", "#1F8DFF", "#2A45FF", "#0B1033")) +
  theme_void() + theme(legend.position = "none")
ggsave("figures/fig3/fig3F_pie_pop_income.svg",
       si_pie_pop_2_5_income,width=5,height=5)

#Pie chart showing percent area under different rural-urban, landuse,
#aridity and income classes considering only rivers with nitrate 
#concentrations exceeding 0.5 mg/L - Fig 3G
pie_df_0_5_popden <- basin_joined[!is.na(basin_joined$rural_urban_class),]  %>%
  st_drop_geometry() %>%
  filter(mean_nit_levels != "1. 0-0.5") %>%
  dplyr::group_by(rural_urban_class) %>%
  dplyr::summarise(area = sum(area_km2, na.rm = TRUE) / 1e9) %>% 
  mutate(per_area = area*100/sum(area))
pie_df_0_5_landuse <- basin_joined[!is.na(basin_joined$landuse_class),]  %>%
  st_drop_geometry() %>%
  filter(mean_nit_levels != "1. 0-0.5") %>%
  dplyr::group_by(landuse_class) %>%
  dplyr::summarise(area = sum(area_km2, na.rm = TRUE) / 1e9) %>% 
  mutate(per_area = area*100/sum(area))
pie_df_0_5_aridity <- basin_joined[!is.na(basin_joined$aridity_class),]  %>%
  st_drop_geometry() %>%
  filter(mean_nit_levels != "1. 0-0.5") %>%
  dplyr::group_by(aridity_class) %>%
  dplyr::summarise(area = sum(area_km2, na.rm = TRUE) / 1e9) %>% 
  mutate(per_area = area*100/sum(area))
pie_df_0_5_order <- basin_joined[!is.na(basin_joined$order_class),]  %>%
  st_drop_geometry() %>%
  filter(mean_nit_levels != "1. 0-0.5") %>%
  dplyr::group_by(order_class) %>%
  dplyr::summarise(area = sum(area_km2, na.rm = TRUE) / 1e9) %>% 
  mutate(per_area = area*100/sum(area))

si_pie_area_0_5_popden=ggplot(pie_df_0_5_popden, 
                             aes(x = "", y = area, fill = rural_urban_class)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#63C8FF", "#1F8DFF", "#2A45FF", "#0B1033")) +
  theme_void() + theme(legend.position = "none")
ggsave("figures/fig3/fig3G_pie_area_popden.svg",
       si_pie_area_0_5_popden,width=5,height=5)

si_pie_area_0_5_landuse=ggplot(pie_df_0_5_landuse, 
                              aes(x = "", y = area, fill = landuse_class)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#63C8FF", "#1F8DFF", "#2A45FF", "#0B1033")) +
  theme_void() + theme(legend.position = "none")
ggsave("figures/fig3/fig3G_pie_area_landuse.svg",
       si_pie_area_0_5_landuse,width=5,height=5)

si_pie_area_0_5_aridity=ggplot(pie_df_0_5_aridity, 
                               aes(x = "", y = area, fill = aridity_class)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#A0E2FF", "#63C8FF", "#1F8DFF", "#2A45FF", "#0B1033")) +
  theme_void() + theme(legend.position = "none")
ggsave("figures/fig3/fig3G_pie_area_aridity.svg",
       si_pie_area_0_5_aridity,width=5,height=5)

si_pie_area_0_5_order=ggplot(pie_df_0_5_order, 
                               aes(x = "", y = area, fill = order_class)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c( "#63C8FF", "#1F8DFF", "#2A45FF", "#0B1033")) +
  theme_void() + theme(legend.position = "none")
ggsave("figures/fig3/fig3G_pie_area_order.svg",
       si_pie_area_0_5_order,width=5,height=5)
