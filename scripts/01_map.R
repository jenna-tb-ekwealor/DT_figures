library(rgbif)
library(maps)
library(ggplot2)
library(sf)
library(st)
library(concaveman)
library(dplyr)
library(countrycode)
library(raster)
library(ncdf4)
library(terra)
library(ggmap)
library(countrycode)
library(tidyterra)

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


# uncomment out all below to download and process large data files, or proceed and upload my saved versions
# options(timeout=1000)
# 
# # Precipitation
# download.file(url = 'http://thredds.northwestknowledge.net:8080/thredds/fileServer/TERRACLIMATE_ALL/data/TerraClimate_ppt_2019.nc',
#               destfile = '../data/ppt.nc')
# 
# # Evapotranspiration
# download.file(url = 'http://thredds.northwestknowledge.net:8080/thredds/fileServer/TERRACLIMATE_ALL/data/TerraClimate_pet_2019.nc',
#               destfile = '../data/pet.nc')
# 
# # Minimum monthly temperature
# download.file(url = 'http://thredds.northwestknowledge.net:8080/thredds/fileServer/TERRACLIMATE_ALL/data/TerraClimate_tmin_2019.nc',
#               destfile = '../data/tmin.nc')
# 
# 
# #--- Import the downloaded files ---#
# # Precipitation
# ppt <- raster::stack(x = '../data/ppt.nc')
# 
# # Evapotranspiration
# pet <- raster::stack(x = '../data/pet.nc')
# 
# # Minimum monthly temperature
# tmin <- raster::stack(x = '../data/tmin.nc')
# 
# # #--- Inspect ---#
# # # Precipitation
# # plot(ppt)
# # # Evapotranspiration
# # plot(pet)
# 
# 
# #--- Raster maths ---#
# # Precipitation
# ppt_mean <- raster::calc(ppt, # RasterStack object
#                  fun = mean, # Function to apply across the layers
#                  na.rm = TRUE)
# 
# # Evapotranspiration
# pet_mean <- raster::calc(pet,
#                  fun = mean, 
#                  na.rm = TRUE)
# 
# 
# # Minimum monthly temperature
# tmin_mean <- raster::calc(tmin,
#                          fun = mean, 
#                          na.rm = TRUE)
# 
# 
# 
# 
# 
# 
# #--- Calculate aridity index ---#
# # Precipitation (ppt) / Evapotranspiration (pet)
# aridity_index <- overlay(x = ppt_mean, # Raster object 1
#                          y = pet_mean, # Raster object 2
#                          fun = function(x, y){return(x / y)}) # Function to apply
# 
# crs(aridity_index)
# 
# 
# 
# #--- Convert raster to a matrix ---#
# aridity_index_matrix <- rasterToPoints(aridity_index)
# 
# 
# #--- Convert to the matrix to a dataframe ---#
# aridity_index_df <- as.data.frame(aridity_index_matrix)
# 
# 
# 
# #--- Recode aridity index into categories --#
# aridity_index_df <- aridity_index_df %>% 
#   # Recode
#   mutate(category = case_when(
#     is.infinite(layer) ~ 'Humid',
#     layer >= 0.65 ~ 'Humid', 
#     layer >= 0.5 & layer < 0.65 ~ 'Dry sub-humid', 
#     layer >= 0.2 & layer < 0.5 ~ 'Semi-arid', 
#     layer >= 0.05 & layer < 0.2 ~ 'Arid', 
#     layer < 0.05 ~ 'Hyper-arid' 
#   )) %>% 
#   # Convert to ordered factor
#   mutate(category = factor(category,
#                            levels = c('Hyper-arid', 'Arid', 'Semi-arid', 'Dry sub-humid', 'Humid'),
#                            ordered = TRUE))
# 
# # # SAVE aridity index DATASET SO DON'T HAVE TO DOWNLOAD EVERY TIME
# write.csv(aridity_index_df, file = "../output/global_aridity_index.csv", row.names = F) # too big

aridity_index_df <- read.csv(file = "../output/global_aridity_index.csv")
aridity_index_df$category <- factor(aridity_index_df$category, levels = c("Hyper-arid", "Arid", "Semi-arid", "Dry sub-humid", "Humid"))


#--- Set a colour palette ---#
aridity_colours <- c("Hyper-arid" = '#ea6402', "Arid" = '#fd7e23', "Semi-arid" = '#fd9c55', "Dry sub-humid" = '#feb988', "Humid" = 'lightgrey')


# remove big large datasets
# rm(aridity_index)
# rm(aridity_index_matrix)
# rm(pet)
# rm(ppt)
# rm(tmin)
# rm(pet_mean)
# rm(ppt_mean)

#--- Plot the data ---#
ggplot() +
  geom_raster(data = aridity_index_df,
              aes(y = y,
                  x = x,
                  fill = category)) +
  scale_fill_manual(values = aridity_colours, name = "Aridity",
                    labels = c("Hyper-arid" = '', "Arid" = '', "Semi-arid" = '', "Dry sub-humid" = '', "Humid" = '')
  ) +
  theme_minimal() +
  theme(axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = c(0.05, 0.5))  -> aridity_map
aridity_map

pdf(file = "../output/ardity_map.pdf",
    width = 5.5,
    height = 3.5)
aridity_map
dev.off()


#--- Load DT taxa data ---#
# Marks_et.al_Appendix_S1 2021
marks <- read.csv("../data/Marks_et.al_Appendix_S1.csv", header = T)
# fix colnames
colnames(marks) <- as.character(unlist(marks[1,]))
marks = marks[-1, ]

nonplants <- read.csv("../data/nonplants.csv", header = T)

# add nonplants and marks df
DT_taxa <- full_join(marks, nonplants)

# retrieve DT species from rgbif
DT_taxa$Genus_species <- paste0(DT_taxa$Genus, " ", DT_taxa$Species)
Genus <- unique(DT_taxa$Genus)

# code below to search rbif but takes a while, so load rdata instead
# create a retrieve backbone function to apply to each genus
retrieve_backbone <- function(Genus){
  rgbif::name_backbone(name = Genus)
}

# UNCOMMENT BELOW TO RUN BACKBONE SEARCH, LOAD occ_search BELOW INSTEAD IF ALREADY DONE
# retrieve the backbone of each genus
backbones <- lapply(Genus, retrieve_backbone)

# add names
names(backbones) <- Genus

# get usagekeys
getkeys <- function(Genus){
  Genus[["usageKey"]]
}
usageKeys <- lapply(backbones, getkeys)

# organize output
usageKeys <- unlist(usageKeys) %>% as.data.frame() %>% tibble::rownames_to_column(., "Genus") %>% dplyr::rename(., usageKey = .)

# some manual cleaning, remove these Genus
usageKeys %>% dplyr::filter(Genus != "Lindernia (previously Ilysanthes)") %>% 
  dplyr::filter(Genus != "Cyperus (previously Kyllinga)") -> usageKeys


# remove rows with duplicate usagekey
usageKeys = usageKeys[!duplicated(usageKeys$usageKey),]


# create function for retrieving occurrences per Genus
get_occ <- function(Genus){
  rgbif::occ_search(taxonKey = Genus, limit = 1000, curlopts = list(verbose = TRUE))
}

occ_search <- lapply(usageKeys$usageKey, get_occ)

# add names
names(occ_search) <- usageKeys$Genus

# save occ search since it's big and took forever
saveRDS(occ_search, file="../data/occ_search.RData")

# load instead of running 
occ_search <- readRDS("../data/occ_search.RData")

occ_search_unlist <- lapply(occ_search, `[[`, "data")

# pull data from all elements of occ list 
occ_data_all <- purrr::list_rbind(occ_search_unlist)

occ_data_all %>% dplyr::select(key, decimalLatitude, decimalLongitude, kingdom, phylum, order, family, genus, species, taxonKey, taxonRank, continent, stateProvince) -> occ_data_all

###############################
# rasterize occurence points
###############################

# arrange df by genus
occ_data_all <- occ_data_all %>% dplyr::arrange(genus)

# subset only spatial and genera data
xy <-  occ_data_all %>% dplyr::select(decimalLongitude, decimalLatitude, genus) 
colnames(xy) <- c("lon", "lat", "genus")

# generate genera list
genera_list <- unique(xy$genus)

v <- terra::vect(xy)
r <- terra::rast(nrows = 200, ncols = 200, crs = "EPSG:4326", ext = terra::ext(v))


genera_raster <- terra::rasterize(v, r, fun = "count", by = "genus")
names(genera_raster) <- genera_list

richness_raster <- terra::rasterize(v, r, "genus", function(x, ...) length(unique(na.omit(x))))
terra::plot(richness_raster)

names(richness_raster) <- "Genera"

#--- Convert the matrix to a dataframe ---#
richness_raster_df <- terra::as.data.frame(richness_raster, xy = T)

# save a copy of the richness raster df
write.csv(richness_raster_df, file = "../output/global_plant_richness.csv", row.names = F) 

# read in richness raster
richness_raster_df <- read.csv("../output/global_plant_richness.csv", header = T)

# create blank world raster
world_raster_df <- aridity_index_df %>% dplyr::select(-category, -layer)

# plot 
richness_map <- ggplot() + 
  geom_raster(data = world_raster_df,
              aes(y = y,
                  x = x),
              fill = "lightgray") +
  geom_raster(data = richness_raster_df,
              aes(y = y,
                  x = x,
              fill = Genera)) +
  theme_minimal() +
  theme(axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = c(0.08, 0.5)) +
  scale_fill_grass_c(
    palette = "grass",
    direction=-1,
    guide = guide_legend(reverse = T)) +
  labs(fill = "Genera")

richness_map

pdf(file = "../output/richness_map.pdf",
    width = 5.5,
    height = 3.5)
richness_map
dev.off()

minmax(richness_raster, compute=FALSE)
