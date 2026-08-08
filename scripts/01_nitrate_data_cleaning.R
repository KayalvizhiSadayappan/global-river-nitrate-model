# ---------------------------------------------------------------
# Script: 01_nitrate_data_cleaning.R
# Purpose: Clean GRQA water quality data and calculate long term mean nitrate per site
# Author: Kayalvizhi Sadayappan
# Date: 2026-08-07
# Version: 1.0.1
#
# Inputs:
#   - Folder: data/raw/GRQA/
#   - Files:GRQA_meta/NO3N_GRQA_dup_obs.csv  
#           GRQA_data_v1.3/NO3N_GRQA.csv
#           country_continent_list.txt, site_id_country_correction.txt
#   - Source: Download GRQA_data_v1.3.zip and GRQA_meta.zip from https://zenodo.org/records/7056647
#             country_continent_list.txt and site_id_country_correction.txt are in the repository
#
# Outputs:
#   - Folder: data/processed/
#   - Files: NO3N_GRQA_daily_cleaned.txt (contains daily observations for all sites after cleaning)
#            NO3N_sites_GRQA.txt  (contains mean nitrate concentrations for all sites)
#
# Dependencies:
#   - R packages: dplyr, readr, lubridate
# Runtime: The code ran on 1 CPU with 32 GB of memory and completed in approximately 7.5 minutes.
# ---------------------------------------------------------------

#load libraries
library(readr)
library(lubridate)
library(dplyr)

#select root folder (user specific)
root_folder <- "/global-river-nitrate-model/"

#set working directory
setwd(root_folder)

start_time=Sys.time()

#load raw data
NO3N_GRQA <- read_delim("data/raw/GRQA/GRQA_data_v1.3/NO3N_GRQA.csv", 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)
NO3N_GRQA_dup_obs <- read_delim("data/raw/GRQA/GRQA_meta/NO3N_GRQA_dup_obs.csv", 
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)
country_continent_list <- read_delim("data/raw/GRQA/country_continent_list.txt",
                                     delim = "\t",escape_double = FALSE,trim_ws = TRUE)
site_country_cor <- read_delim("data/raw/GRQA/site_id_country_correction.txt", 
                               delim = "\t",col_types = cols(site_id = col_character()), escape_double = FALSE,trim_ws = TRUE)

#ensure processed subfolder exists
if (!dir.exists("data/processed")) { 
  dir.create("data/processed", recursive = TRUE)
  }

#GLORICH and GEMSTAT drainage areas are missing in GRQA data but are in the following units according to their source data
NO3N_GRQA$upstream_basin_area_unit[which(NO3N_GRQA$source=="GLORICH")]="m2"
NO3N_GRQA$upstream_basin_area_unit[which(NO3N_GRQA$source=="GEMSTAT")]="km2"

#conversion of drainage area into common unit of km2
NO3N_GRQA$area_km2=NA
NO3N_GRQA$area_km2[which(NO3N_GRQA$upstream_basin_area_unit=="km2")]=NO3N_GRQA$upstream_basin_area[which(NO3N_GRQA$upstream_basin_area_unit=="km2")]
NO3N_GRQA$area_km2[which(NO3N_GRQA$upstream_basin_area_unit=="m2")]=NO3N_GRQA$upstream_basin_area[which(NO3N_GRQA$upstream_basin_area_unit=="m2")]/1E6
NO3N_GRQA$area_km2[which(NO3N_GRQA$upstream_basin_area_unit=="sq mi")]=NO3N_GRQA$upstream_basin_area[which(NO3N_GRQA$upstream_basin_area_unit=="sq mi")]*2.59  

#Dates with time zone was converted to UTC time zone date. When time zone was missing, it was automatically considered as UTC
NO3N_GRQA$obs_date_UTC=as.Date(NA)
NO3N_GRQA$obs_date_UTC[is.na(NO3N_GRQA$obs_time_zone)]=NO3N_GRQA$obs_date[is.na(NO3N_GRQA$obs_time_zone)]
NO3N_GRQA$obs_date_UTC[which(NO3N_GRQA$obs_time_zone %in% c("UTC","DST"))]=
  NO3N_GRQA$obs_date[which(NO3N_GRQA$obs_time_zone %in% c("UTC","DST"))]
timezone_df=data.frame(tz=c("AKDT","AKST","CDT","CST","MST","PST", "PDT","MDT","EST","EDT","HST","AST","IDLE","ZP-11","AWST"),
                       offset=c(-8,-9,-5,-6,-7,-8,-7,-6,-5,-4,-10,-4,12,11,8))
for(i in 1:nrow(timezone_df)){
  temp=NO3N_GRQA[which(NO3N_GRQA$obs_time_zone==timezone_df$tz[i]),]
  local_time=lubridate::ymd_hms(paste(temp$obs_date,temp$obs_time))
  NO3N_GRQA$obs_date_UTC[which(NO3N_GRQA$obs_time_zone==timezone_df$tz[i])]=as.Date(local_time - hours(timezone_df$offset[i]))
}


#combine data from same sites with different names in differnt databases
NO3N_GRQA_dup_sites <- NO3N_GRQA_dup_obs %>%
  distinct(site_id_1, site_id_2)
NO3N_GRQA_subset=NO3N_GRQA[,c("site_id","site_country","lon_wgs84",
                              "lat_wgs84","area_km2","obs_date_UTC",
                              "obs_value","source")]
ind_dup=which(NO3N_GRQA_subset$site_id%in% c(NO3N_GRQA_dup_sites$site_id_1,
                                         NO3N_GRQA_dup_sites$site_id_2))
distinct_data=NO3N_GRQA_subset[-ind_dup,]

dup_data <- lapply(1:nrow(NO3N_GRQA_dup_sites), function(i) {
  ind=which(NO3N_GRQA_subset$site_id%in% c(NO3N_GRQA_dup_sites$site_id_1[i],
                                           NO3N_GRQA_dup_sites$site_id_2[i]))
  temp_data_holder=NO3N_GRQA_subset[ind,]
  cons_df=data.frame(matrix(NA,nrow=length(unique(temp_data_holder$obs_date_UTC)),ncol=ncol(temp_data_holder)))
  colnames(cons_df)=colnames(temp_data_holder)
  cons_df$obs_date_UTC=unique(temp_data_holder$obs_date_UTC)
  cons_df$site_id=paste(unique(temp_data_holder$site_id),collapse="_")
  cons_df$source=paste(unique(temp_data_holder$source),collapse="_")
  cons_df$area_km2=unique(temp_data_holder$area_km2)
  cons_df$site_country=unique(temp_data_holder$site_country)[1]
  cons_df$lat_wgs84=unique(temp_data_holder$lat_wgs84[temp_data_holder$site_id == unique(temp_data_holder$site_id)[1]])
  cons_df$lon_wgs84=unique(temp_data_holder$lon_wgs84[temp_data_holder$site_id == unique(temp_data_holder$site_id)[1]])
  for(j in 1:nrow(cons_df)){
    temp=temp_data_holder[which(temp_data_holder$obs_date_UTC==cons_df$obs_date_UTC[j]),]
    cons_df$obs_value[j]=mean(temp$obs_value)
  }
  cons_df
})
dup_data  <- bind_rows(dup_data)
NO3N_GRQA_subset=rbind(distinct_data,dup_data)

#outlier removal for all samples - log-transformed nitrate values that were beyone
#3 times the standard deviation from mean were removed as outliers
outlier_all=data.frame(site_id=unique(NO3N_GRQA_subset$site_id),log_mean=NA,log_sd=NA,
                       lower_limit=NA,upper_limit=NA,n=NA,n_out=NA)
temp_outlier_rem_list <- vector("list", length = nrow(outlier_all))
site_indices <- split(1:nrow(NO3N_GRQA_subset), NO3N_GRQA_subset$site_id)
for(i in 1:nrow(outlier_all)){
  site_id=outlier_all$site_id[i]
  temp_site_data=NO3N_GRQA_subset[site_indices[[as.character(site_id)]],]
  sample_all=log10(temp_site_data$obs_value)
  outlier_all$log_mean[i]=mean(sample_all)
  outlier_all$log_sd[i]=sd(sample_all)
  outlier_all$lower_limit[i]=outlier_all$log_mean[i]-3*outlier_all$log_sd[i]
  outlier_all$upper_limit[i]=outlier_all$log_mean[i]+3*outlier_all$log_sd[i]
  outlier_all$n[i]=length(sample_all)
  temp_site_data=temp_site_data[which(sample_all>=outlier_all$lower_limit[i]&
                                        sample_all<=outlier_all$upper_limit[i]),]
  temp_outlier_rem_list[[i]]=temp_site_data
  outlier_all$n_out[i]=length(sample_all)-nrow(temp_site_data)
  #print(i)
}
NO3N_GRQA_subset_outrem=do.call(rbind, temp_outlier_rem_list)

#Calculate mean daily concentration for days with several measurements. This was done to 
#avoid possible bias created by days with several measurements.
temp_date_cons_list <- vector("list", length = nrow(outlier_all))
site_indices <- split(1:nrow(NO3N_GRQA_subset_outrem), NO3N_GRQA_subset_outrem$site_id)
for(i in 1:nrow(outlier_all)){
  site_id=outlier_all$site_id[i]
  temp_site_data=NO3N_GRQA_subset_outrem[site_indices[[as.character(site_id)]],]
  if (nrow(temp_site_data)==length(unique(temp_site_data$obs_date_UTC))){
    temp_date_cons_list[[i]]=temp_site_data
  } else{
    temp_site_data_cons <- temp_site_data %>%
      group_by(obs_date_UTC) %>%
      summarise(site_id=unique(site_id),site_country = unique(site_country),
                lon_wgs84=unique(lon_wgs84),lat_wgs84=unique(lat_wgs84),  
                area_km2=unique(area_km2),obs_value = mean(obs_value),
    
                source = paste(unique(source), collapse = "_")) %>% ungroup()
    temp_date_cons_list[[i]] <- temp_site_data_cons %>%
      select(names(temp_site_data))
  }
  #print(i)
}
NO3N_GRQA_daily=do.call(rbind, temp_date_cons_list)

#correct for sites with wrong or no country assigned
NO3N_GRQA_daily <- NO3N_GRQA_daily %>%
  left_join(site_country_cor, by = "site_id") %>%
  mutate(
    site_country = coalesce(country, site_country)
  ) %>%
  select(-country)


#assign continent name to countries
NO3N_GRQA_daily=NO3N_GRQA_daily %>%
  left_join(country_continent_list, by = "site_country") %>%
  select(-site_country) %>%  
  rename(site_country = site_country_name)  
NO3N_GRQA_daily$site_country[which(NO3N_GRQA_daily$site_id %in% c("FRA00021","FRA00022"))]="French Guiana"
NO3N_GRQA_daily$continent[which(NO3N_GRQA_daily$site_id %in% c("FRA00021","FRA00022"))]="SouthAmerica"

#get mean nitrate concentratioon in each site
sites_GRQA <- NO3N_GRQA_daily %>%
  group_by(site_id) %>%
  summarise(lon_wgs84=first(lon_wgs84),lat_wgs84=first(lat_wgs84),
            site_country = first(site_country),continent=first(continent),
            area_km2 = first(area_km2),
            count_obs = n(),mean_obs_value = mean(obs_value),
            median_obs_value = median(obs_value),sd_obs_value = sd(obs_value)) %>%
  ungroup()   

write.table(NO3N_GRQA_daily,"data/processed/NO3N_GRQA_daily_cleaned.txt",
            sep="\t",row.names = FALSE)
write.table(sites_GRQA,"data/processed/NO3N_sites_GRQA.txt",
            sep="\t",row.names = FALSE)

run_time=Sys.time()-start_time
sprintf("Code ran in %02d:%02d:%02d", 
        floor(as.numeric(run_time, "secs")/3600),
        floor((as.numeric(run_time, "secs")%%3600)/60),
        round(as.numeric(run_time, "secs")%%60))
