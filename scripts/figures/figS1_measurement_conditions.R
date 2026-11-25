################################################################################
# Author : Kayalvizhi Sadayappan                                               # 
#                                                                              #
# This code was used to plot fig S1 in Climate Aridity                         #
# Amplifies Global Riverine Nitrate Beyond Human Inputs paper                  # 
#                                                                              #
# Inputs: 1) download discharge data from GRADES-hydroDL:                      #
# https://www.reachhydro.org/home/records/grades-hydrodl                       #
# 2) Download GRQA_data_v1.3.zip from https://zenodo.org/records/7056647 and   # 
# get "NO3N_GRQA_daily_cleaned.txt" by running "01_nitrate_data_cleaning.R"    #
# from https://github.com/KayalvizhiSadayappan/global-river-nitrate-model/     # 
#                                                                              #
# R version 4.3.2.was used - Package versions in comments                      #
################################################################################
library(readr) # ‘2.1.5’
library(sf)    # ‘1.0.16’
library(ncdf4) # ‘1.22’
library(purrr) # ‘1.0.2’
library(dplyr) # ‘1.1.4’
library(tidyr) # ‘1.3.1’

root_folder <- "global-river-nitrate-model"
setwd(root_folder)

output_folder <- file.path("figures","figS1")
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)

# combine daily discharge data for COMIDs with nitrate date
nit_df=read_delim("data/raw/BRT_train_data_7213_catchments.txt", 
                  delim = "\t", escape_double = FALSE, trim_ws = TRUE)
comids=data.frame(COMID=nit_df$COMID);

comids$MERIT=comids$COMID %/%1E7
merit_dt=data.frame(date=seq(as.Date("1980-1-1"),as.Date("2024-12-31"),by="day"))
merit_dt$yr=as.numeric(format(merit_dt$date,"%Y"))
folder_name="data/raw/Discharge_GRADES_hydroDL/"
file_name="GRADES-hydroDL_V2.0_pfaf_0"
yr_seq=seq(1980,2024,1);
Q_df_list=list()
for(sel_merit in seq(1,8,1)){
  COMID=unique(comids$COMID[which(comids$MERIT==sel_merit)])
  Q_df=data.frame(matrix(NA,nrow=nrow(merit_dt),
                         ncol=length(COMID)+1))
  colnames(Q_df)=c("date",COMID)
  Q_df$date=merit_dt$date
  yr=as.numeric(format(merit_dt$date,"%Y"))
  for(k in yr_seq){
    nc_data <- nc_open(paste(folder_name,k,"/",file_name,sel_merit,"_",k,".nc",sep=""))
    data_array <- ncvar_get(nc_data, "Qout")
    var_info = nc_data$var[["Qout"]]
    data_array_ids=var_info$dim[[1]]$vals
    ind=match(COMID, data_array_ids)
    Q_df[which(yr==k),2:ncol(Q_df)]=t(data_array[ind,])
    nc_close(nc_data)
    rm(nc_data,data_array,var_info,data_array_ids)
    cat("Year ",k, " done!\n")
  }
  Q_df_list[[sel_merit]]=Q_df
  cat("MERIT ",sel_merit, " done!\n")
}

#combine data from different MERIT into one data.frame
Q_df_comb <- reduce(Q_df_list, left_join, by = "date")

Q_long <- Q_df_comb %>%
  pivot_longer(
    -date,
    names_to = "COMID",
    values_to = "discharge"
  ) %>%
  mutate(COMID = as.numeric(COMID))
site_Q <- Q_long %>%
  left_join(nit_df[,c("site_id","COMID")], by = "COMID")

site_Q_wide <- site_Q %>%
  select(date, site_id, discharge) %>%
  pivot_wider(
    names_from = site_id,
    values_from = discharge
  )
site_Q_wide <- site_Q_wide[, c("date", nit_df$site_id)]

write.table(site_Q_wide,"data/processed/discharge_80_24_global_nit.txt",
            sep="\t",row.names=F)

# identify flow and temporal conditions of measurements used for getting 
# mean nitrate concentration in 7213 sites used to train the model
NO3N_GRQA_daily = read_delim("data/processed/NO3N_GRQA_daily_cleaned.txt", 
           delim = "\t", escape_double = FALSE, 
           trim_ws = TRUE)
NO3N_GRQA_sel <- NO3N_GRQA_daily[NO3N_GRQA_daily$site_id %in% nit_df$site_id,]
NO3N_GRQA_sel$month <- format(NO3N_GRQA_sel$obs_date_UTC,"%m")
NO3N_GRQA_sel$year <- as.numeric(format(NO3N_GRQA_sel$obs_date_UTC,"%Y"))

NO3N_GRQA_sel <- NO3N_GRQA_sel %>% mutate(
  season = case_when(
    as.numeric(month) %in% c(12,1,2) ~ "Winter",
    as.numeric(month) %in% c(3,4,5) ~ "Spring",
    as.numeric(month) %in% c(6,7,8) ~ "Summer",
    as.numeric(month) %in% c(9,10,11) ~ "Fall",
    TRUE ~ NA_character_
  )
)

#get count of measurements per site
site_count <- NO3N_GRQA_sel %>% 
  group_by(site_id) %>%
  summarise(count=n())

site_Q_long <- site_Q_wide %>%
  pivot_longer(
    cols = -date,               
    names_to = "site_id",       
    values_to = "discharge"     
  )

#get site-wise flow percentiles at increments of 10
site_Q_per <- site_Q_long %>% 
  group_by(site_id) %>%
  summarize(p0=min(discharge,na.rm=T),
            p10=quantile(discharge,0.1,na.rm=T),
            p20=quantile(discharge,0.2,na.rm=T),
            p30=quantile(discharge,0.3,na.rm=T),
            p40=quantile(discharge,0.4,na.rm=T),
            p50=quantile(discharge,0.5,na.rm=T),
            p60=quantile(discharge,0.6,na.rm=T),
            p70=quantile(discharge,0.7,na.rm=T),
            p80=quantile(discharge,0.8,na.rm=T),
            p90=quantile(discharge,0.9,na.rm=T),
            p100=max(discharge,na.rm=T))

#get flow percentile range on the days of measurement 
Q_nit <- left_join(NO3N_GRQA_sel[,c("site_id","obs_date_UTC","obs_value")],
                   site_Q_long,by=c("site_id","obs_date_UTC"="date"))
Q_nit <- left_join(Q_nit,site_Q_per,by="site_id")
Q_nit <- Q_nit %>%
  mutate(
    flow_per = case_when(
      discharge <= p10 ~ "0-10",
      discharge <= p20 ~ "10-20",
      discharge <= p30 ~ "20-30",
      discharge <= p40 ~ "30-40",
      discharge <= p50 ~ "40-50",
      discharge <= p60 ~ "50-60",
      discharge <= p70 ~ "60-70",
      discharge <= p80 ~ "70-80",
      discharge <= p90 ~ "80-90",
      discharge <= p100 ~ "90-100",
      TRUE ~ NA_character_
    )
  )


#get site-wise flow percentiles
site_Q_4flow_per <- site_Q_long %>% 
  group_by(site_id) %>%
  summarize(p0=min(discharge,na.rm=T),
            p25=quantile(discharge,0.25,na.rm=T),
            p50=quantile(discharge,0.5,na.rm=T),
            p75=quantile(discharge,0.75,na.rm=T),
            p100=max(discharge,na.rm=T))

#assign flow percentiles to days when nitrate was measured
Q_nit_4flow <- left_join(NO3N_GRQA_sel[,c("site_id","obs_date_UTC","obs_value")],
                         site_Q_long,by=c("site_id","obs_date_UTC"="date"))
Q_nit_4flow <- left_join(Q_nit_4flow,site_Q_4flow_per,by="site_id")
Q_nit_4flow <- Q_nit_4flow %>%
  mutate(
    flow_per = case_when(
      discharge <= p25 ~ "0-25",
      discharge <= p50 ~ "25-50",
      discharge <= p75 ~ "50-75",
      discharge <= p100 ~ "75-100",
      TRUE ~ NA_character_
    )
  )

#get max proportion of season and flow percentile when most data was measured
#considering there are 4 seaons and 4 flow percentile ranges considered, this
#ratio should ideally be close to 0.25
seas_balance <- NO3N_GRQA_sel %>%
  count(site_id, season) %>%
  group_by(site_id) %>%
  mutate(prop = n / sum(n)) %>%
  summarise(max_prop = max(prop))
flow_balance <- Q_nit_4flow %>%
  filter(!is.na(discharge)) %>%
  count(site_id, flow_per) %>%
  group_by(site_id) %>%
  mutate(prop = n / sum(n)) %>%
  summarise(max_prop = max(prop))


# plot the distribution of number of measurements and information on year, month
# flow percentile when measurement was taken. The final row shows ow uniformly
# the measurements were taken over seasons and flow ranges
plot_si=ggplot(site_count)+geom_histogram(aes(x=count),color="white")+
  scale_x_log10()+geom_vline(xintercept = median(site_count$count), linetype = 2)+
  theme_classic()+theme(axis.title = element_blank())+
  ggplot(NO3N_GRQA_sel, aes(x = year)) +
  geom_histogram(color="white")+geom_vline(xintercept = median(NO3N_GRQA_sel$year), linetype = 2)+
  theme_classic()+theme(axis.title = element_blank())+
  ggplot(NO3N_GRQA_sel, aes(x = month)) +
  geom_bar()+theme_classic()+theme(axis.title = element_blank())+
  ggplot(Q_nit[!is.na(Q_nit$discharge),], aes(x = flow_per)) +
  geom_bar()+theme_classic()+theme(axis.title = element_blank())+
  ggplot(seas_balance, aes(x = max_prop)) +
  geom_histogram(color = "white") +
  geom_vline(xintercept = median(seas_balance$max_prop, na.rm = TRUE), linetype = 2)+
  theme_classic()+theme(axis.title = element_blank())+
  ggplot(flow_balance, aes(x = max_prop)) +
  geom_histogram(color = "white") +
  geom_vline(xintercept = median(flow_balance$max_prop, na.rm = TRUE), linetype = 2)+
  theme_classic()+theme(axis.title = element_blank())+
  plot_layout(nrow=3,ncol=2)

ggsave("figures/figS1/figS1_daily_nit_data_dist.svg",
       plot = plot_si,dpi=1000,width=10.0,height=2.50)