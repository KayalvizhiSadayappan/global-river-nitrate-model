################################################################################
# Author : Kayalvizhi Sadayappan                                               # 
#                                                                              #
# This code was used to plot fig S8 in Climate Aridity                         #
# Amplifies Global Riverine Nitrate Beyond Human Inputs paper                  # 
#                                                                              #
# Inputs: Download "SHAP_interaction_model_338.txt" from                       #
# https://zenodo.org/records/7056647                                           # 
#                                                                              #
# R version 4.3.2.was used - Package versions in comments                      #
################################################################################
library(ggplot2) #‘3.5.1’
library(patchwork) #‘1.3.0’

root_folder <- "global-river-nitrate-model"
setwd(root_folder)

output_folder <- file.path("figures","figS8")
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)

# read interaction of different features 
interaction_strength <- read.table("results/SHAP_interaction_model_338.txt", header = TRUE, row.names = 1)

# get mean interaction of different features 
mean_interaction_strength <- data.frame(att=colnames(interaction_strength),
                                        interaction=colMeans(interaction_strength,na.rm=T))
mean_interaction_strength$att <- factor(mean_interaction_strength$att, levels = rev(mean_interaction_strength$att))

# plot mean interaction of different features - Fig S8A
si_mean_int=ggplot(data=mean_interaction_strength)+geom_col(aes(y=att,x=interaction),fill="grey")+
  theme_classic()+theme(axis.title=element_blank(),
                        axis.text.y = element_blank(),
                        axis.line.y=element_blank(),
                        axis.ticks.y = element_blank())

#get feature-wise interaction of features 
interaction_strength[upper.tri(interaction_strength)] <- NA
diag(interaction_strength) <- NA
df <- as.data.frame(as.table(as.matrix(interaction_strength)))
colnames(df) <- c("Feature1", "Feature2", "Strength")
df$quartile <- cut(
  df$Strength,
  breaks = c(0,0.0005,0.001,0.0015,0.0025,0.011),
  include.lowest = TRUE,right = TRUE,
  labels = c("0.0000-0.0005", "0.0005-0.0010", "0.0010-0.0015", "0.0015-0.0025", "0.0025-0.0100"))

# plot heatmap showing feature interaction with other features - Fig S8B
si_int_br=ggplot(df, aes(Feature1, Feature2, fill = quartile)) +
  geom_tile() +
  scale_fill_manual(values=c("#A3F0FF", "#63C8FF", "#1F8DFF", "#2A45FF", "#0B1033"),
                    na.value = "transparent") +theme_classic()+
  scale_y_discrete(limits = rev)+
  theme(axis.title=element_blank(),axis.text = element_blank(),
        axis.line=element_blank(),axis.ticks = element_blank(),
        legend.position = "none")

ggsave("figures/figS8/figS8_SHAP_feature_interaction.svg",
       plot = si_mean_int + si_int_br, width = 10.0, height = 2.50)