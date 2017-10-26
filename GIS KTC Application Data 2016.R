## ----------------------
## GIS data with CC 2016
## ----------------------

## --------------------------------
## Load shapefile by Package rgdal
## Also could use package tmap instead
## --------------------------------

library(rgdal)

# Define directory of GIS dataset (Thai Administration)
# Generally GIS dataset consist of file .prj, .dbf, .shx and .shp 
dataDir <- "/Users/Danny/Documents/R Project/THA_adm"

# Define layer dataset, without extension
layerName <- "THA_adm1"  # Load Thai Province layer from shape (.shp)
thaiAdmin <- readOGR(dsn = dataDir, layer = layerName)  # Load data into R

# Check CRS if WGS84 ok
summary(thaiAdmin)

# Read zipcode - province mapping
# library(readxl)
# zipAdmin <- read_excel("/Users/Danny/Documents/R Project/THA_adm/province.xlsx", sheet = 4)
# zipAdmin$Zip_Code <- as.character(zipAdmin$Zip_Code)  # Convert zip to charactor
# detach(package:readxl)

## --------------
## Read KTC data
## --------------
setwd("/Users/Danny/Documents/R Project/KTC Dataset")
load(file = "qry_ALLProduct_KPI.rdata")

# Map zip with zipAdmin
library(dplyr)
# dat <- left_join(dat, zipAdmin, by = c("ZipCode" = "Zip_Code"))

# Count no of finalized CC by province
KTC <- dat %>%
  # filter(Source_Code == "OGS") %>%
  group_by(Region_Eng, Province_Eng) %>%
  summarise(n = n()) %>%
  filter(Province_Eng != "Bangkok Metropolis") %>%  # Exclude Bangkok from data
  mutate(rank = rank(n)) %>%  # create ranking field
  mutate(topName = ifelse(rank >= n() - 10, Province_Eng, ""))  # Top 10 


## --------------------
## Manipulate map data 
## with package tmap
## --------------------
library(tmaptools)
library(tmap)

# Load shape file
thaiAdmin <- read_shape(file = "/Users/Danny/Documents/R Project/THA_adm/THA_adm1.shp")

# Add no of finalized to thaiAdmin@data
KTC <- rename(KTC, NAME_1 = Province_Eng)  # Rename to match with thaiAdmin_map
thaiAdmin_KTC <- thaiAdmin  # Use thaiAdmin_map for mapping
thaiAdmin_KTC@data <- left_join(thaiAdmin_KTC@data, KTC)  # Map data to thaiAdmin_map

## -------------------------------
## 1. Plot with qtm (quick plot)
## -------------------------------

qtm(thaiAdmin_KTC)

qtm(thaiAdmin_KTC, fill = "n", fill.palette = "div",  title = "CC+PL Finalized",
    text = "NAME_1", text.size = "AREA", root = 1)

## -------------------------------
## 2. Plot with standard plot
## -------------------------------

tm_shape(thaiAdmin_KTC) +  # Define shape files used
  tm_fill(col = "n", alpha = 1, palette = "div", title = "CC+PL Finalized") +  # Define attribute to be filled
  tm_borders() + # Add border
  tm_text(text = "topName", size = "AREA")  # Show top name with varible text size proportion to polygon area

tm_shape(thaiAdmin_KTC) +  # Define shape files used
  tm_fill(col = "n", alpha = 1, title = "CC+PL Finalized") +  # Define attribute to be filled
  tm_borders() +
  tm_facets(by = "Region_Eng")  # Facet by Region

## -----------------------
## 3. Interactive map
## -----------------------

# Create tmap data
mapThai <- tm_shape(thaiAdmin_KTC) +  # Define shape files used
  tm_fill(col = "n", alpha = 1, palette = "div", title = "CC+PL Finalized") +  # Define attribute to be filled
  tm_borders() + # Add border
  tm_text(text = "topName", size = "AREA")  # Show top name with varible text size proportion to polygon area


# Change from plot mode to view (interactive)
tmap_mode("view")

# Start interactive mode
mapThai
