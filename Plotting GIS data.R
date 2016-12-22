## -----------------------
## Ploting GIS data in R
## -----------------------

# rgdal : R library for GDAL(Geospatial Abstraction Library)
library(rgdal)

# dsn : data source name (directory that stored GIS data)
# layer : shape files = consist of many files extension (.dbf, .prj, .shp) all in same names
# .shp = polygon information
# .dbf = data (attributes) of each polygon
setwd("/Users/Danny/Documents/R Project/Creating-maps-in-R-master")
lnd <- readOGR(dsn = "data", layer = "london_sport")

# R-GIS object structure
# lnd@polygon = object of plotting polygon
# lnd@data = object of attribute (data.frame)
str(lnd)

# Plot GIS data with base plot
plot(lnd, col = "gray")

# Filter GIS data by attribute
sel <- lnd$Partic_Per > 25
plot(lnd[sel, ])

# Plot both all area with highlight the filter area
# Use stag plotting
plot(lnd, col = "lightgray")
sel <- lnd$Partic_Per > 25
plot(lnd[sel, ], add = TRUE, col = "turquoise")

# Gis data infomation
summary(lnd)
# Key : info
# Object class : SpatialPolygonDataFrame
# Is Projected : Projected on Earth co-ordinate
# proj4string : CRS (Coordinate Referecen System)

# CRS (Coordinate Referecen System)
# list CRS
CRS_list <- make_EPSG()
# WGS 84 is the most used system
CRS_list[grepl("WGS 84$", CRS_list$note), ]
# transform CRS system
lnd84 <- sp::spTransform(lnd, CRS("+init=epsg:4326"))
saveRDS(object = lnd84, file = "/Users/Danny/Documents/R Project/Creating-maps-in-R-master/data/lnd84.Rds")

## --------------------------
## Join with Attribute data
## --------------------------
# Load new attribute data
crime_data <- read.csv("/Users/Danny/Documents/R Project/Creating-maps-in-R-master/data/mps-recordedcrime-borough.csv", stringsAsFactors = FALSE)

# Filter only needed data
crime_theft <- crime_data[crime_data$CrimeType == "Theft & Handling", ]

# Aggregrate data by joining key (Borough = Name)
crime_ag <- aggregate(x = crime_theft["CrimeCount"], by = crime_theft["Borough"], FUN = sum)

# Check if no crime data area to be join with area map
lnd@data[!(lnd$name %in% crime_ag$Borough), ]

# Check if no area map in the crime data
crime_ag$Borough[!(crime_ag$Borough %in% lnd$name)]
crime_ag[is.null(crime_ag$Borough),]

# Left join with dplyr
library(dplyr)
crime_ag <- rename(crime_ag, name = Borough)  # Rename for joining "Borouth" to "name
lnd@data <- left_join(lnd@data, crime_ag)

# Use tmap : Temperatue map for plottion
library(tmap)
qtm(lnd, "CrimeCount", fill.palette = "div", text = "name", text.size = 0.75)

# Show area name for top attribute
lnd$top_label <- ifelse(lnd$CrimeCount > mean(lnd$CrimeCount[!is.na(lnd$CrimeCount)]), lnd$name, "")
qtm(lnd, "CrimeCount", fill.palette = "div", text = "top_label", text.size = 0.75)

## -----------------------
## Join with spatial data
## -----------------------
stations <- readOGR(dsn = "/Users/Danny/Documents/R Project/Creating-maps-in-R-master/data", layer = "lnd-stns")

# Check the CRS and projected area
proj4string(stations)
proj4string(lnd)
bbox(stations)
bbox(lnd)

# Reproject to new CRS data from lnd
stationRe <- spTransform(stations, CRS(proj4string(lnd)))
stations <- stationRe
rm(stationRe)
plot(lnd)

# Super impos points data on area plot data 
points(stations)

# Spatial intersection
station_bkp <- stations
stations <- station_bkp[lnd, ]
plot(stations)

## ----------------------------
## Aggregrate by spatial data
## ----------------------------

# Count obs. in each area , FUN = length
# stations["CODE"] : sent list of attribute, could be any attribute since it's just count obs. by each polygon
stations_agg <- aggregate(x = stations["CODE"], by = lnd, FUN = length)
head(stations_agg@data, n = 10)

# map to lnd shape
lnd$n_points <- stations_agg$CODE


# Mean of "NUMBER" by each polygon
lnd_n <- aggregate(x = stations["NUMBER"], by = lnd, FUN = mean)

# Create quandrant plot 
brks <- quantile(lnd_n$NUMBER)  # Define break points of each quantile
labs <- gray.colors(n = 4)  # Define color by each quantile

# Create group by cut by quantiles, and define labels = labs colors
q <- cut(lnd_n$NUMBER, brks, labels = labs, include.lowest = TRUE)
summary(q)

# Convert factor -> charactor for color parameter
qc <- as.character(q)

# Plot with specificed color in each polygon
plot(lnd, col = qc)

# Add legend of each quantiles
legend(legend = paste0("Q", 1:4), fill = levels(q), "topright")
areas <- sapply(lnd_n@polygons, function(x) x@area)

# Extract station type and plot in the maps
levels(stations$LEGEND)
sel <- grepl("A Road Sing|Rapid", stations$LEGEND)
sym <- as.integer(stations$LEGEND[sel])
points(stations[sel, ], pch = sym)
legend(legend = c("A Road", "RTS"), "bottomright", pch = unique(sym))


## --------------------------
## More beautiful plot
## --------------------------

# Library tmap
library(tmap)
qtm(shp = lnd, fill = "Partic_Per", fill.palette = "-Blues")

# Side by side plot 2 attribute
qtm(shp = lnd, fill = c("Partic_Per", "Pop_2001"), fill.palette = c("Blues"), nrow = 2)

# faceting map
tm_shape(lnd) +
  tm_fill("Pop_2001", thres.poly = 0) +
  tm_facets("name", free.coords=TRUE, drop.units=TRUE) +
  tm_layout(legend.show = FALSE, title.position = c("center", "center"), title.size = 20)

# Library ggmaps
library(ggplot2)

# Nomal point plotting with ggplot
p <- ggplot(data = lnd@data, aes(Partic_Per, Pop_2001))
p + geom_point()
p + geom_point(aes(colour = Partic_Per, size = Pop_2001)) + geom_text(size = 2, aes(label = name))

# Combine plotting with ggmap
# ggplot2 cannot plot Spatial data directly need to be convert ot data.frame

library(rgeos)
lnd_f <- fortify(lnd)

# Since fortify data.fraem will loose the @data
# use left join to retrived all data with join key = row.names of lnd
lnd$id <- row.names(lnd)
lnd_f <- dplyr::left_join(lnd_f, lnd@data)

# Ploting with ggplot2
map <- ggplot(lnd_f, aes(long, lat, group = group, fill = Partic_Per)) + geom_polygon() +
  coord_equal() +
  labs(x = "Easting (m)", y = "Northing (m)", fill = "% Sports\nParticipation") + 
  ggtitle("London Sports Participation")

# library Leaflet
lnd84 <- readRDS("/Users/Danny/Documents/R Project/Creating-maps-in-R-master/data/lnd84.Rds")

# Plot polygon area
leaflet() %>%
  addTiles() %>% addPolygons(data = lnd84)

# coloring leaflet data
# Create color palette

pal <- colorNumeric(
  palette = "Blues",
  domain = lnd84$Partic_Per
)

# Plot leaflet with color palette
leaflet(data = lnd84) %>%
  addTiles() %>%
  addPolygons(stroke = TRUE, smoothFactor = 0.5, fillOpacity = 0.5,
              color = ~pal(Partic_Per))