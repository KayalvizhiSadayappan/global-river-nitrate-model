################################################################################
# Author : Kayalvizhi Sadayappan                                               # 
#                                                                              #
# This code was used to plot fig 2 in Climate Aridity                          #
# Amplifies Global Riverine Nitrate Beyond Human Inputs paper                  # 
#                                                                              #
# Inputs: Input files are from https://zenodo.org/records/21853129             #
#                                                                              #
# R version 4.3.2.was used - Package versions in comments                      #
################################################################################

library(readr)# ‘2.1.5’
library(dplyr)# ‘1.1.4’
library(tidyr)# ‘1.3.1’
library(ggplot2)# ‘3.5.1’
library(patchwork)# ‘1.3.0’
library(ggridges) #‘0.5.6’

root_folder <- "global-river-nitrate-model"
setwd(root_folder)

output_folder <- file.path("figures","fig2")
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)

#read in predicted nitrate concentrations, assign them to river segments by COMID
stream_nit_df <- read_delim(file="results/predicted_nitrate_concentrations_global.txt",
                            delim = "\t", escape_double = FALSE,trim_ws = TRUE)
global_river_segment_classes <- read_delim("data/raw/global_river_segment_classes.txt",
                                           delim = "\t", escape_double = FALSE, trim_ws = TRUE)

ind=match(stream_nit_df$COMID,global_river_segment_classes$COMID)
stream_nit_df$AI = global_river_segment_classes$aridity_index[ind]
stream_nit_df$landuse_class = global_river_segment_classes$landuse_class[ind]
stream_nit_df$aridity_class = global_river_segment_classes$aridity_class[ind]
stream_nit_df$human_landuse = global_river_segment_classes$human_landuse[ind]

#create a grid with landuse along x-axis and aridity along y-axis
grid_x_breaks <- seq(0,100,5)
grid_y_breaks <- seq(0,20,1)
data_df_grid <- stream_nit_df[which(stream_nit_df$AI<=20),] %>%
  mutate(
    grid_x = cut(human_landuse,
                 breaks = grid_x_breaks,
                 include.lowest = TRUE),
    grid_y = cut(AI,
                 breaks = grid_y_breaks,
                 include.lowest = TRUE),
    grid_x_id = cut(
      human_landuse,
      breaks = grid_x_breaks,
      include.lowest = TRUE,
      labels = FALSE
    ),
    grid_y_id = cut(
      AI,
      breaks = grid_y_breaks,
      include.lowest = TRUE,
      labels = FALSE
    )
  )

#calculate median of predicted nitrate values in different subgrids and classify
#them into bins
data_summary <- data_df_grid %>%
  group_by(grid_x_id, grid_y_id) %>%
  summarize(mean_nit_median = median(mean_nit, na.rm = TRUE),
            .groups = "drop")
data_summary <- data_summary %>%
  mutate(
    x_mid = (grid_x_breaks[grid_x_id] + grid_x_breaks[grid_x_id + 1]) / 2,
    y_mid = (grid_y_breaks[grid_y_id] + grid_y_breaks[grid_y_id + 1]) / 2
  )
data_summary <- data_summary %>%
  mutate(
    mean_nit_bin = cut(mean_nit_median,
                       breaks = c(0.0,1.057,1.44,1.643,1.838,2.9),
                       include.lowest = TRUE)
  )

#plot the median value of predicted mean nitrate in different grids - Fig 2A
dx <- diff(grid_x_breaks)[1]
dy <- diff(grid_y_breaks)[1]
heatmap <- ggplot(data_summary, aes(x = x_mid, y = y_mid, fill = mean_nit_bin)) +
  geom_raster() +
  coord_cartesian(
    xlim = c(min(data_summary$x_mid) - dx/2, max(data_summary$x_mid) + dx/2),
    ylim = c(min(data_summary$y_mid) - dy/2, max(data_summary$y_mid) + dy/2),
    expand = FALSE
  ) +
  scale_fill_manual(
    values = c("#A0E2FF", "#63C8FF", "#1F8DFF", "#2A45FF", "#0B1033"),
    na.value = "grey",
    name = NULL
  ) +theme_classic()+
  xlab("Human land use, %")+ylab("Aridity Index")+
  theme(
    legend.position = "none",
    axis.text = element_blank(),axis.title = element_blank())

ggsave("figures/fig2/fig2A_landuse_AI_heatmap.svg",
       plot = heatmap,width=5,height=5)

#scatter plots of aridity index vs predicted nitrate concentration - Fig 2B
si_ai=ggplot(stream_nit_df,aes(x=AI,y=mean_nit))+
  geom_point(color="grey",size=0.25,alpha=0.2)+
  geom_smooth(method = "lm", formula = y ~ x,col="black",linewidth=2) +
  geom_smooth(data=stream_nit_df[which(stream_nit_df$landuse=="3. urban"),],
              aes(x=AI,y=mean_nit),method = "lm", formula = y ~ x,col="#1F8DFF",linewidth=2) +
  geom_smooth(data=stream_nit_df[which(stream_nit_df$landuse=="4. agri"),],
              aes(x=AI,y=mean_nit),method = "lm", formula = y ~ x,col="#2A45FF",linewidth=2) +
  geom_smooth(data=stream_nit_df[which(stream_nit_df$landuse=="1. undev"),],
              aes(x=AI,y=mean_nit),method = "lm", formula = y ~ x,col="#63c8ff",linewidth=2) +
  theme_classic()+scale_x_log10(limits=c(0.05,5200))+
  scale_y_log10(limits=c(0.005,12.5))

si_ai_agri=ggplot(stream_nit_df[which(stream_nit_df$landuse=="4. agri"),],
                  aes(x=AI,y=mean_nit))+
  geom_point(color="grey",size=0.25,alpha=0.2)+
  geom_smooth(method = "lm", formula = y ~ x,col="#2A45FF",linewidth=2) +
  theme_classic()+scale_x_log10(limits=c(0.05,5200))+
  scale_y_log10(limits=c(0.005,12.5))

si_ai_undev=ggplot(stream_nit_df[which(stream_nit_df$landuse=="1. undev"),],
                   aes(x=AI,y=mean_nit))+
  geom_point(color="grey",size=0.25,alpha=0.2)+
  geom_smooth(method = "lm", formula = y ~ x,col="#63c8ff",linewidth=2) +
  theme_classic()+scale_x_log10(limits=c(0.05,5200))+
  scale_y_log10(limits=c(0.005,12.5))

si_ai_urban=ggplot(stream_nit_df[which(stream_nit_df$landuse=="3. urban"),],
                   aes(x=AI,y=mean_nit))+
  geom_point(color="grey",size=0.25,alpha=0.2)+
  geom_smooth(method = "lm", formula = y ~ x,col="#1F8DFF",linewidth=2) +
  theme_classic()+scale_x_log10(limits=c(0.05,5200))+
  scale_y_log10(limits=c(0.005,12.5))

#figure saved in png format as svg file for this can be very large
ggsave("figures/fig2/fig2B_AI_mean_nit_scatter.svg",
       plot = si_ai+
         theme(legend.position = "none",axis.text = element_blank(),axis.title = element_blank(),
                          panel.background = element_rect(fill = "transparent", color = NA),
                          plot.background  = element_rect(fill = "transparent", color = NA)),
       dpi=1000,width=5.0,height=5.0, bg = "transparent")
ggsave("figures/fig2/fig2B_AI_mean_nit_scatter_undev.svg",
       plot = si_ai_undev+theme(legend.position = "none",axis.text = element_blank(),axis.title = element_blank(),
                                panel.background = element_rect(fill = "transparent", color = NA),
                                plot.background  = element_rect(fill = "transparent", color = NA)),
       dpi=1000,width=1.25,height=1.25, bg = "transparent")
ggsave("figures/fig2/fig2B_AI_mean_nit_scatter_urban.svg",
       plot = si_ai_urban+theme(legend.position = "none",axis.text = element_blank(),axis.title = element_blank(),
                                panel.background = element_rect(fill = "transparent", color = NA),
                                plot.background  = element_rect(fill = "transparent", color = NA)),
       dpi=1000,width=1.25,height=1.25, bg = "transparent")
ggsave("figures/fig2/fig2B_AI_mean_nit_scatter_agri.svg",
       plot = si_ai_agri+theme(legend.position = "none",axis.text = element_blank(),axis.title = element_blank(),
                                panel.background = element_rect(fill = "transparent", color = NA),
                                plot.background  = element_rect(fill = "transparent", color = NA)),
       dpi=1000,width=1.25,height=1.25, bg = "transparent")


#density distributions of mean nitrate in undevelped sites under 
#different aridity classes - Fig 1C
density_plot_undev=ggplot(stream_nit_df[which(stream_nit_df$landuse_class =="1. undev" &
                                                !is.na(stream_nit_df$aridity_class)),],
                          aes(x=mean_nit,y=aridity_class,color = aridity_class,
                              fill="transparent"))+
  geom_density_ridges2(fill="transparent")+
  scale_color_manual(values=c("#a0e2ff","#63c8ff", "#1f8dff", "#2a45ff", "#0b1033"))+
  stat_density_ridges(fill="transparent",quantile_lines = TRUE,quantile_fun = median,linetype=2)+
  theme_classic()+
  coord_cartesian(xlim=c(0,2))+
  theme(panel.border = element_blank())
stream_nit_df[which(stream_nit_df$landuse_class =="1. undev" &
                      !is.na(stream_nit_df$aridity_class)),] %>%
  group_by(aridity_class) %>%
  summarize(median_value = median(mean_nit))

ggsave("figures/fig2/fig2C_density_dist_AI_undev.svg",
       plot = density_plot_undev,width=4.8,height=4.8, bg = "transparent")

#plot importance of most important features - Fig 2D
BRT_mean_importance <- read_delim("results/BRT_mean_importance.txt", 
                                  delim = "\t", escape_double = FALSE, 
                                  trim_ws = TRUE)

mean_importance_df <- BRT_mean_importance %>%
  arrange(desc(Gain_mean))

gain_plot <- ggplot(mean_importance_df[1:6,], aes(x = reorder(Feature, Gain_mean), y = Gain_mean)) +
  geom_bar(stat = "identity", fill = "grey") +
  geom_errorbar(aes(ymin = Gain_mean - Gain_sd, ymax = Gain_mean + Gain_sd),
                width = 0) + 
  coord_flip() + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.24)) +  # NO padding, lower limit zero
  theme_classic()+
  theme(axis.title=element_blank(),axis.line.y = element_blank(),
        axis.ticks.y=element_blank(),axis.text = element_blank(),legend.position = "none")

ggsave("figures/fig2/fig2D_gain_plot.svg",
       plot = gain_plot,width=4,height=4.8, bg = "transparent")

#scatter plots of human landuse vs predicted nitrate concentration - Fig 2E
si_lu=ggplot(stream_nit_df,aes(x=human_landuse,y=mean_nit))+
  geom_point(color="grey",size=0.25,alpha=0.2)+
  geom_smooth(method = "lm", formula = y ~ x,col="black",linewidth=2) +
  geom_smooth(data=stream_nit_df[which(stream_nit_df$landuse=="3. urban"),],
              aes(x=crop_mean + urban_mean + crop_nat_mean,y=mean_nit),method = "lm", formula = y ~ x,col="#1F8DFF",linewidth=2) +
  geom_smooth(data=stream_nit_df[which(stream_nit_df$landuse=="4. agri"),],
              aes(x=crop_mean + urban_mean + crop_nat_mean,y=mean_nit),method = "lm", formula = y ~ x,col="#2A45FF",linewidth=2) +
  geom_smooth(data=stream_nit_df[which(stream_nit_df$landuse=="1. undev"),],
              aes(x=crop_mean + urban_mean + crop_nat_mean,y=mean_nit),method = "lm", formula = y ~ x,col="#63c8ff",linewidth=2) +
  theme_classic()+scale_x_log10(limits=c(5E-6,100))+
  scale_y_log10(limits=c(0.005,12.5))

si_lu_undev=ggplot(stream_nit_df[which(stream_nit_df$landuse=="1. undev"),],
                   aes(x=human_landuse,y=mean_nit))+
  geom_point(color="grey",size=0.25,alpha=0.2)+
  geom_smooth(method = "lm", formula = y ~ x,col="#63c8ff",linewidth=2) +
  theme_classic()+scale_x_log10(limits=c(5E-6,100))+
  scale_y_log10(limits=c(0.005,12.5))

si_lu_urban=ggplot(stream_nit_df[which(stream_nit_df$landuse=="3. urban"),],
                   aes(x=human_landuse,y=mean_nit))+
  geom_point(color="grey",size=0.25,alpha=0.2)+
  geom_smooth(method = "lm", formula = y ~ x,col="#1F8DFF",linewidth=2) +
  theme_classic()+scale_x_log10(limits=c(5E-6,100))+
  scale_y_log10(limits=c(0.005,12.5))

si_lu_agri=ggplot(stream_nit_df[which(stream_nit_df$landuse=="4. agri"),],
                  aes(x=human_landuse,y=mean_nit))+
  geom_point(color="grey",size=0.25,alpha=0.2)+
  geom_smooth(method = "lm", formula = y ~ x,col="#2A45FF",linewidth=2) +
  theme_classic()+scale_x_log10(limits=c(5E-6,100))+
  scale_y_log10(limits=c(0.005,12.5))

#figure saved in png format as svg file for this can be very large
ggsave("figures/fig2/fig2E_landuse_mean_nit_scatter.svg",
       plot = si_lu+
         theme(legend.position = "none",axis.text = element_blank(),axis.title = element_blank(),
               panel.background = element_rect(fill = "transparent", color = NA),
               plot.background  = element_rect(fill = "transparent", color = NA)),
       dpi=1000,width=5.0,height=5.0, bg = "transparent")
ggsave("figures/fig2/fig2E_landuse_mean_nit_scatter_undev.svg",
       plot = si_lu_undev+theme(legend.position = "none",axis.text = element_blank(),axis.title = element_blank(),
                                panel.background = element_rect(fill = "transparent", color = NA),
                                plot.background  = element_rect(fill = "transparent", color = NA)),
       dpi=1000,width=1.25,height=1.25, bg = "transparent")
ggsave("figures/fig2/fig2E_landuse_mean_nit_scatter_urban.svg",
       plot = si_lu_urban+theme(legend.position = "none",axis.text = element_blank(),axis.title = element_blank(),
                                panel.background = element_rect(fill = "transparent", color = NA),
                                plot.background  = element_rect(fill = "transparent", color = NA)),
       dpi=1000,width=1.25,height=1.25, bg = "transparent")
ggsave("figures/fig2/fig2E_landuse_mean_nit_scatter_agri.svg",
       plot = si_lu_agri+theme(legend.position = "none",axis.text = element_blank(),axis.title = element_blank(),
                               panel.background = element_rect(fill = "transparent", color = NA),
                               plot.background  = element_rect(fill = "transparent", color = NA)),
       dpi=1000,width=1.25,height=1.25, bg = "transparent")


#density distributions of mean nitrate in urban sites under 
#different aridity classes - Fig 2F
density_plot_urban=ggplot(stream_nit_df[which(stream_nit_df$landuse_class =="3. urban"),],
                          aes(x=mean_nit,y=aridity_class,color = aridity_class,
                              fill="transparent"))+
  geom_density_ridges2(fill="transparent")+
  scale_color_manual(values=c("#a0e2ff","#63c8ff", "#1f8dff", "#2a45ff", "#0b1033"))+
  stat_density_ridges(fill="transparent",quantile_lines = TRUE,quantile_fun = median,linetype=2)+
  theme_classic()+
  coord_cartesian(xlim=c(0,4))+
  theme(panel.border = element_blank())
stream_nit_df[which(stream_nit_df$landuse_class =="3. urban" &
                      !is.na(stream_nit_df$aridity_class)),] %>%
  group_by(aridity_class) %>%
  summarize(median_value = median(mean_nit))

ggsave("figures/fig2/fig2F_density_dist_AI_urban.svg",
       plot = density_plot_urban,width=4,height=4.8, bg = "transparent")

#density distributions of mean nitrate in agricultural sites under 
#different aridity classes - Fig 2G
density_plot_agri=ggplot(stream_nit_df[which(stream_nit_df$landuse_class =="4. agri"),],
                          aes(x=mean_nit,y=aridity_class,color = aridity_class,
                              fill="transparent"))+
  geom_density_ridges2(fill="transparent")+
  scale_color_manual(values=c("#a0e2ff","#63c8ff", "#1f8dff", "#2a45ff", "#0b1033"))+
  stat_density_ridges(fill="transparent",quantile_lines = TRUE,quantile_fun = median,linetype=2)+
  theme_classic()+
  coord_cartesian(xlim=c(0,4))+
  theme(panel.border = element_blank())

stream_nit_df[which(stream_nit_df$landuse_class =="4. agri" &
                      !is.na(stream_nit_df$aridity_class)),] %>%
  group_by(aridity_class) %>%
  summarize(median_value = median(mean_nit))

ggsave("figures/fig2/fig2G_density_dist_AI_agri.svg",
       plot = density_plot_agri,width=4,height=4.8, bg = "transparent")
