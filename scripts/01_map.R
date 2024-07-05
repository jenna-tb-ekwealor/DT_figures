library(rgbif)
library(maps)
library(ggplot2)
library(sf)
library(concaveman)
library(dplyr)
library(countrycode)
library(raster)
library(ncdf4)
library(terra)

# the next three lines find the script location and set that as the working directory. copy these lines into all scripts for this project. 
library(rstudioapi)
wd_script_location <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(wd_script_location)


# Aridity layer -----------------------------------------------------------
#--- Download the files from the TerraClimate website ---#
# tutorial: https://www.painblogr.org/2020-12-15-climate-change.html
# Variables:
# aet (Actual Evapotranspiration, monthly total), units = mm
# def (Climate Water Deficit, monthly total), units = mm
# pet (Potential evapotranspiration, monthly total), units = mm
# ppt (Precipitation, monthly total), units = mm
# q (Runoff, monthly total), units = mm
# soil (Soil Moisture, total column - at end of month), units = mm
# srad (Downward surface shortwave radiation), units = W/m2
# swe (Snow water equivalent - at end of month), units = mm
# tmax (Max Temperature, average for month), units = C
# tmin (Min Temperature, average for month), units = C
# vap (Vapor pressure, average for month), units  = kPa
# ws (Wind speed, average for month), units = m/s
# vpd (Vapor Pressure Deficit, average for month), units = kpa
# PDSI (Palmer Drought Severity Index, at end of month), units = unitless

# Precipitation
download.file(url = 'http://thredds.northwestknowledge.net:8080/thredds/fileServer/TERRACLIMATE_ALL/data/TerraClimate_ppt_2019.nc',
              destfile = '../data/ppt.nc')

# Evapotranspiration
download.file(url = 'http://thredds.northwestknowledge.net:8080/thredds/fileServer/TERRACLIMATE_ALL/data/TerraClimate_pet_2019.nc',
              destfile = '../data/pet.nc')

# Minimum monthly temperature
download.file(url = 'http://thredds.northwestknowledge.net:8080/thredds/fileServer/TERRACLIMATE_ALL/data/TerraClimate_tmin_2019.nc',
              destfile = '../data/tmin.nc')


#--- Import the downloaded files ---#
# Precipitation
ppt <- raster::stack(x = '../data/ppt.nc')

# Evapotranspiration
pet <- raster::stack(x = '../data/pet.nc')

# Minimum monthly temperature
tmin <- raster::stack(x = '../data/tmin.nc')

# #--- Inspect ---#
# # Precipitation
# plot(ppt)
# # Evapotranspiration
# plot(pet)


#--- Raster maths ---#
# Precipitation
ppt_mean <- raster::calc(ppt, # RasterStack object
                 fun = mean, # Function to apply across the layers
                 na.rm = TRUE)

# Evapotranspiration
pet_mean <- raster::calc(pet,
                 fun = mean, 
                 na.rm = TRUE)


# Minimum monthly temperature
tmin_mean <- raster::calc(tmin,
                         fun = mean, 
                         na.rm = TRUE)



# #--- Inspect ---#
# # Precipitation
# plot(main = 'Precipitation',
#      ppt_mean)
# 
# # Evapotranspiration
# plot(main = 'Evapotranspiration',
#      pet_mean)
# 
# # Minimum monthly temperature
# plot(main = 'Minimum monthly temperature',
#      tmin_mean)

#--- Calculate aridity index ---#
# Precipitation (ppt) / Evapotranspiration (pet)
aridity_index <- overlay(x = ppt_mean, # Raster object 1
                         y = pet_mean, # Raster object 2
                         fun = function(x, y){return(x / y)}) # Function to apply


#--- Convert raster to a matrix ---#
aridity_index_matrix <- rasterToPoints(aridity_index)


#--- Convert to the matrix to a dataframe ---#
aridity_index_df <- as.data.frame(aridity_index_matrix)



#--- Recode aridity index into categories --#
aridity_index_df <- aridity_index_df %>% 
  # Recode
  mutate(category = case_when(
    is.infinite(layer) ~ 'Humid',
    layer >= 0.65 ~ 'Humid', 
    layer >= 0.5 & layer < 0.65 ~ 'Dry sub-humid', 
    layer >= 0.2 & layer < 0.5 ~ 'Semi-arid', 
    layer >= 0.05 & layer < 0.2 ~ 'Arid', 
    layer < 0.05 ~ 'Hyper-arid' 
  )) %>% 
  # Convert to ordered factor
  mutate(category = factor(category,
                           levels = c('Hyper-arid', 'Arid', 'Semi-arid', 'Dry sub-humid', 'Humid'),
                           ordered = TRUE))

# # SAVE aridity index DATASET SO DON'T HAVE TO DOWNLOAD EVERY TIME
# write.csv(aridity_index_df, file = "../output/global_aridity_index.csv") # too big


#--- Set a colour palette ---#
colours <- c('#ea6402', '#fd7e23', '#fd9c55', '#feb988', 'lightgrey')
# colours <- c('#3c7274', '#4d9395', '#66adb0', '#87bfc1', 'lightgrey')



# remove big large datasets
# rm(aridity_index)
rm(aridity_index_matrix)
rm(pet)
rm(ppt)
rm(tmin)
# rm(pet_mean)
# rm(ppt_mean)


#--- Plot the data ---#
ggplot() +
  geom_raster(data = aridity_index_df,
              aes(y = y,
                  x = x,
                  fill = category)) +
  scale_fill_manual(values = colours, name = "") +
  theme_minimal() +
  theme(axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = c(0.13, 0.5))  -> aridity_map
aridity_map

pdf(file = "../output/ardity_map.pdf",
    width = 5.5,
    height = 3.5)
aridity_map
dev.off()
