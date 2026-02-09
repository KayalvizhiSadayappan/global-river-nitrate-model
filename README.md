
# Global River Nitrate Model
<!-- Project Title -->
<p align="justify"> A global scale approach for estimating long term mean nitrate concentrations across the global river network using land use, climate, soil and other catchment attributes </p> 

<h1>Overview</h1>

<p align="justify"> Nitrate continues to be a persistent global problem, causing eutrophication, endangering aquatic life and human health, and driving higher drinking water treatment costs. Human nitrogen input is considered as the main driver of high nitrate concentrations. Catchment factors including climate, soil and aquifer properties are known to moderate the effect of human nitrogen input; yet there is a lack of global framework explaining these interactions. While rivers in North America and Europe are relatively well monitored, rivers in other continents have huge gaps in their monitoring network. This work aims to 1) generate a global map of river nitrate concentrations and 2) propose a framework explaining how and which catchment characteristics interact with nitrogen input to moderate river nitrate concentrations. For this purpose, an ensemble of 1000 machine learning (boosted regression tree) models was trained on data from 7213 sites (Global River Quality Archive, GRQA) representing 6601 river segments to predict nitrate concentration across the global river network (Multi-Error-Removed Improved Terrain Hydrography, MERIT-Hydro). The predicted nitrate concentrations were used to develop the framework and additionally assess the global population exposed to high nitrate concentrations. </p>

<h1>Repository Structure</h1>

- `data/` - Input datasets
  - `raw/` - Unprocessed input data (from original databases or repositories)
  - `processed/` - Processed data created by scripts for model training and prediction
- `models` - Models are saved here
- `results` - Predicted river nitrate concentrations
- `scripts` - Scripts that clean raw data, trains model on it and predict nitrate concentrations
  - `01_nitrate_data_cleaning` - cleans raw data and calculate mean river nitrate concentrations in GRQA sites
  - `02_identify_river_segments_with_data` - identifies the river segments on which the GRQA sites are located
  - `03_train_BRT_model` - trains 1000 machine learning models to predict mean nitrate concentrations
  - `04_predict_global_nitrate` - predicts nitrate concentration in 2895896 segments in the global river network

<h1>Requirements</h1>

The scripts run in R (version 4.3.2) and requires the following packages:
- readr (2.1.5)
- dplyr (1.1.4)
- lubridate (1.9.4)
- sf (1.0.16)
- parallel (4.3.2)
- caret (6.0.94)
- xgboost (1.7.7.1)
- hydroGOF (0.6.0)

<h1>Data</h1>

Nitrate concentration data: Global River Quality Archive (GRQA)<sup>1<sup>  

Global river network: Multi-Error-Removed Improved Terrain Hydrography (MERIT Hydro)<sup>2<sup> 

Catchment characteristics:
  - Terra and Aqua combined Moderate Resolution Imaging Spectroradiometer (MODIS) Land Cover Type  MODIS Land Cover Type Product (MCD12Q1, version 6.1)<sup>3<sup> 
  - Climatologies at High Resolution for the Earth’s Land Surface Areas (CHELSA version 2.1)<sup>4<sup> 
  - Global Multi-resolution Terrain Elevation Data 2010 (GMTED2010)<sup>5<sup> 
  - SoilGrids version 2.0<sup>6<sup> 
  - Global Geo-processed Data of Aquifer Properties by 0.5° Grid, Country and Water Basins<sup>7<sup> 

<h1>Notes</h1>

<p align="justify"> The data for training the models are provided in the file "BRT_train_data_7213_catchments.txt" in the folder "data/processed". The "BRT_input_global_subset.txt" file in the same folder contains catchment characteristics for 5000 river segments as an example dataset to demonstrate prediction of river nitrate concentration in these segments. </p>

<h1>Citation</h1>

1. H. Virro, G. Amatulli, A. Kmoch, L. Shen, E. Uuemaa, GRQA: global river water quality archive. Earth System Science Data Discussions 2021, 1-30 (2021)
2. D. Yamazaki et al., MERIT Hydro: A high‐resolution global hydrography map based on latest topography dataset. Water Resour Res 55, 5053-5073 (2019)
3. M. Friedl, D. Sulla-Menashe, MODIS/Terra+ Aqua land cover type yearly L3 Global 0.05 Deg CMG V061. NASA EOSDIS Land Processes Distributed Active Archive Center (DAAC) data set, MCD12C11. 061 (2022)
4. D. N. Karger et al., Climatologies at high resolution for the earth’s land surface areas. Scientific data 4, 1-20 (2017)
5. J. J. Danielson, D. B. Gesch, "Global multi-resolution terrain elevation data 2010 (GMTED2010),"  (US Geological Survey, 2011)
6. L. Poggio et al., SoilGrids 2.0: producing soil information for the globe with quantified spatial uncertainty. Soil 7, 217-240 (2021)
7. H. Niazi et al., "Global Geo-processed Data of Aquifer Properties by 0.5 Grid, Country and Water Basins,"  (MultiSector Dynamics-Living, Intuitive, Value-adding, Environment, 2024)









