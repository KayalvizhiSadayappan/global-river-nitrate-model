# ---------------------------------------------------------------
# Script: 02_identify_river_segments_with_data.R
# Purpose: For each site with at least 10 measurements, the stream network it best represents
#           are chosen. The criteria is 1) the selected river segment's upstream area 
#           should be within 20% of site's area in GRQA database 2) minimum distance
#           between site and selected river segment should be within 5
# Author: Kayalvizhi Sadayappan
# Date: 2026-01-26
# Version: 1.0.0
#
# Inputs:
#   - Folder: data/raw/MERIT_Hydro/MERIT_Hydro_v07_Basins_v01/MERIT_Hydro_v07_Basins_v01/pfaf_level_01/
#   - Files: 8 files including "riv_pfaf_1_MERIT_Hydro_v07_Basins_v01.shp", riv_pfaf_2_MERIT_Hydro_v07_Basins_v01.shp", etc
#   - Download: MERIT_Hydro_v07_Basins_v01.zip from https://drive.google.com/drive/folders/1uCQFmdxFbjwoT9OYJxw-pXaP8q_GYH1a?usp=share_link
#   - Processed data from previous script:
#       Folder: data/processed/river_nit_conc/
#       Files: NO3N_sites_GRQA.txt
#
# Outputs:
#   - Folder: data/processed/river_segments_with_data
#   - Files: nearest_MERIT_COMIDs.txt (contains nearest 15 river segments within 5 km radius from each site)
#            best_MERIT_COMID.txt  (contains COMID of river segment best represented by each site)
#
# Dependencies:
#   - R packages: dplyr, parallel, lubridate, sf
#
# Runtime: The code ran on 20 CPU each with 32 GB of memory and completed in approximately 36 minutes.
# ---------------------------------------------------------------

#load libraries
library(readr)
library(dplyr)
library(sf)
library(parallel)

#functions
assign_MERIT=function(sites_GRQA){
  sites_GRQA$MERIT=NA
  sites_GRQA$MERIT[sites_GRQA$continent=="Africa"]=1
  sites_GRQA$MERIT[sites_GRQA$continent=="Europe"]=2
  sites_GRQA$MERIT[sites_GRQA$site_country=="Turkey"]=2
  sites_GRQA$MERIT[sites_GRQA$site_country=="Russia"]=3
  sites_GRQA$MERIT[sites_GRQA$site_country=="Russia" &
                     sites_GRQA$lon_wgs84 < 60]=2
  sites_GRQA$MERIT[sites_GRQA$site_id %in% c("RUS00009","RUS00019")]=4
  sites_GRQA$MERIT[sites_GRQA$site_country %in% c("India","China","Japan","South Korea",
                                                  "Sri Lanka","Malaysia","Thailand")]=4
  sites_GRQA$MERIT[sites_GRQA$site_country=="Indonesia"]=5
  sites_GRQA$MERIT[sites_GRQA$continent=="Oceania"]=5
  sites_GRQA$MERIT[sites_GRQA$continent=="SouthAmerica"]=6
  sites_GRQA$MERIT[sites_GRQA$continent=="NorthAmerica" & 
                     sites_GRQA$lat_wgs84 < 52]=7
  sites_GRQA$MERIT[sites_GRQA$continent=="NorthAmerica" & 
                     sites_GRQA$lat_wgs84 > 52]=8
  sites_GRQA$MERIT[sites_GRQA$site_country=="Canada" & 
                     sites_GRQA$lat_wgs84 < 60 & sites_GRQA$lon_wgs84>-80]=7
  sites_GRQA_sf=st_as_sf(sites_GRQA, coords = c("lon_wgs84", "lat_wgs84"), crs = 4326)
  box_78 <- st_sfc(st_polygon(list(matrix(c(-140, 50, -110, 50, -110, 60, -140, 60, -140, 50), ncol = 2, byrow = TRUE))))
  box_78_sf <- st_sf(geometry = box_78)
  st_crs(box_78_sf) <- 4326
  ind_78=st_within(sites_GRQA_sf, box_78_sf)
  ind_78 <- lengths(ind_78) > 0  
  sites_GRQA$MERIT[ind_78]=78
  return(sites_GRQA)
}
get_nearest_river_segments=function(merit_river,site_sf){
  nearest_stream_seg=data.frame(matrix(NA,nrow=1,ncol=16))
  colnames(nearest_stream_seg)[1]="site_id"
  nearest_stream_seg$site_id=site_sf$site_id
  #identify rivers within 5 km radius
  site_buffer <- st_buffer(site_sf, dist = 5000)
  rivers_buffer <- st_intersection(merit_river,site_buffer)
  if (nrow(rivers_buffer)!=0){
    temp=as.vector(st_distance(site_sf,rivers_buffer))
    temp_riv=rivers_buffer[order(temp),c("COMID")]
    if (nrow(temp_riv)>15){temp_riv=temp_riv[1:15,]}
    nearest_stream_seg[1,(2:(nrow(temp_riv)+1))]=temp_riv$COMID
  }
  return(nearest_stream_seg)
}
select_bestfit_river_segment=function(merit_river,site_sf,nearest_stream_seg){
  summary_river_segment=data.frame(site_id=site_sf$site_id,area_GRQA=site_sf$area_km2,
                                   sel_COMID=NA,dist=NA,area=NA,area_dif=NA)
  sel_seg_COMID=nearest_stream_seg[1,2:16];
  sel_seg_COMID=sel_seg_COMID[!is.na(sel_seg_COMID)]
  sel_seg_sf=merit_river[merit_river$COMID %in% sel_seg_COMID,]
  temp=data.frame(COMID=sel_seg_sf$COMID,
                  area_dif=(site_sf$area_km2-sel_seg_sf$uparea)*100/site_sf$area_km2,
                  dist=as.vector(st_distance(site_sf, sel_seg_sf)))
  #select the segments whose upstream area is within 20% of site's drainage or upstream area
  temp=temp[which(abs(temp$area_dif)<=20),]
  if(nrow(temp)>0){
    #order the selected river segments based on area constraint in the order of distance and 
    #select the nearest river segment among them as the best choice
    temp=temp[order(temp$dist),]
    if(sum(temp$dist==min(temp$dist))>1){
      temp=temp[which(temp$dist==min(temp$dist)),]
      temp=temp[order(abs(temp$area_dif)),]
    }
    summary_river_segment$sel_COMID=temp$COMID[1]
    summary_river_segment$dist=temp$dist[1]
    summary_river_segment$area=sel_seg_sf$uparea[sel_seg_sf$COMID==temp$COMID[1]]
    summary_river_segment$area_dif=temp$area_dif[1]
  }
  return(summary_river_segment)
}

#select root folder and cores for parallel computing (user specific)
root_folder <- "/global-river-nitrate-model/"
ncores=20

#set working directory
setwd(root_folder)

start_time=Sys.time()

#folder with MERIT-Hydro data
merit_net_folder="data/raw/MERIT_Hydro/MERIT-Hydro_v0.7_v1.0/MERIT_Hydro_v07_Basins_v01/pfaf_level_01/"

#select sites with area information and minimum 10 observations
sites_GRQA <- read_delim("data/processed/river_nit_conc/NO3N_sites_GRQA.txt", 
                              delim = "\t", escape_double = FALSE,trim_ws = TRUE)
sites_GRQA=sites_GRQA[sites_GRQA$count_obs>=10,]
sites_GRQA=sites_GRQA[!is.na(sites_GRQA$area_km2),]

#global river snetwork is divided into 8 files in MERIT hydro dataset
#this function assigns each site to the MERIT file where it lies. 
sites_GRQA <- assign_MERIT(sites_GRQA)
sites_4326 <- st_as_sf(sites_GRQA, coords = c("lon_wgs84", "lat_wgs84"), crs = 4326)
sites_sf <- st_transform(sites_4326, crs = "ESRI:54009")

#identify COMID MERIT river segments within 5 km radius from site
nearest_stream_seg_merit=vector(mode="list",length=9)
current_time=Sys.time()
merit_id=c(1,2,3,4,5,6,7,8,78)
for(i in c(1,2,3,4,5,6,7,8,9)){
  current_time=Sys.time()
  sites_sf_subset=sites_sf[which(sites_sf$MERIT==merit_id[i]),]
  sites_list <- split(sites_sf_subset, seq_len(nrow(sites_sf_subset)))
  if (i != 9){
    merit_river=st_read(paste(merit_net_folder,"riv_pfaf_",i,"_MERIT_Hydro_v07_Basins_v01.shp",sep=""))
  } else if (i == 9){
    MERIT_7=st_read(paste(merit_net_folder,"riv_pfaf_",7,"_MERIT_Hydro_v07_Basins_v01.shp",sep=""))
    MERIT_8=st_read(paste(merit_net_folder,"riv_pfaf_",8,"_MERIT_Hydro_v07_Basins_v01.shp",sep=""))
    merit_river=rbind(MERIT_7, MERIT_8)
    rm(MERIT_7,MERIT_8)
  } else{cat("Check MERIT number!\n")}
  rivers_proj <- st_transform(merit_river, crs = "ESRI:54009")
  nearest_stream_seg_list <- mclapply(
    sites_list,function(site) {
      get_nearest_river_segments(rivers_proj, site)},
    mc.cores=ncores)
  print(i)
  nearest_stream_seg_merit[[i]] <- do.call(rbind, nearest_stream_seg_list)
  print(Sys.time()-current_time)
  
}
nearest_stream_seg_merit_cons=do.call(rbind,nearest_stream_seg_merit)
nearest_stream_seg_merit_cons=nearest_stream_seg_merit_cons[match(sites_sf$site_id, nearest_stream_seg_merit_cons$site_id), ]

write.table(nearest_stream_seg_merit_cons,"data/processed/river_segments_with_data/nearest_MERIT_COMIDs.txt",
            sep="\t",row.names = FALSE)

#select the stream segment best representing each site based on drainage area and distance
best_stream_seg_merit=vector(mode="list",length=9)
for(i in c(1,2,3,4,5,6,7,8,9)){
  sites_sf_subset=sites_sf[which(sites_sf$MERIT==merit_id[i]),]
  nearest_subset=nearest_stream_seg_merit_cons[which(sites_sf$MERIT==merit_id[i]),]
  if (identical(sites_sf_subset$site_id,nearest_subset$site_id)){
    sites_list <- split(sites_sf_subset, seq_len(nrow(sites_sf_subset)))
    nearest_list <- split(nearest_subset, seq_len(nrow(nearest_subset)))
    if (i != 9){
      merit_river=st_read(paste(merit_net_folder,"riv_pfaf_",i,"_MERIT_Hydro_v07_Basins_v01.shp",sep=""))
    } else if (i == 9){
      MERIT_7=st_read(paste(merit_net_folder,"riv_pfaf_",7,"_MERIT_Hydro_v07_Basins_v01.shp",sep=""))
      MERIT_8=st_read(paste(merit_net_folder,"riv_pfaf_",8,"_MERIT_Hydro_v07_Basins_v01.shp",sep=""))
      merit_river=rbind(MERIT_7, MERIT_8)
      rm(MERIT_7,MERIT_8)
    } else{cat("Check MERIT number!\n")}
    rivers_proj <- st_transform(merit_river, crs = "ESRI:54009")
    best_stream_seg_list <- mclapply(seq_along(sites_list),function(x) {
      site <- sites_list[[x]]
      nearest <- nearest_list[[x]]
      select_bestfit_river_segment(rivers_proj, site, nearest)},mc.cores = ncores)
    print(i)
    best_stream_seg_merit[[i]] <- do.call(rbind, best_stream_seg_list)
  }
}
best_stream_seg_merit_cons=do.call(rbind,best_stream_seg_merit)
best_stream_seg_merit_cons=best_stream_seg_merit_cons[match(sites_sf$site_id, best_stream_seg_merit_cons$site_id), ]
best_stream_seg_merit_cons=best_stream_seg_merit_cons[!is.na(best_stream_seg_merit_cons$sel_COMID),]

best_stream_seg_merit_cons$MERIT=as.numeric(best_stream_seg_merit_cons$sel_COMID) %/% 1E7
write.table(best_stream_seg_merit_cons,"data/processed/river_segments_with_data/best_MERIT_COMID.txt",
            sep="\t",row.names = FALSE)

run_time=Sys.time()-start_time
sprintf("Code ran in %02d:%02d:%02d", 
        floor(as.numeric(run_time, "secs")/3600),
        floor((as.numeric(run_time, "secs")%%3600)/60),
        round(as.numeric(run_time, "secs")%%60))

