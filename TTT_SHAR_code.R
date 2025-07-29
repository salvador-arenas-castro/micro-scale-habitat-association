#############################################################
## Calculation of Torus translation test (TTT) to examine the habitat-species associations ##
## Author: Salvador Arenas-Castro ##
#############################################################

library(shar)
library(raster)
library(spatstat)
library(rgdal)
library(rgeos)
library(gstat)
library(rgl)
library(rasterVis)
library(terra)

setwd("/path/")

plot <- readOGR("./path.shp")
plot(plot)

altimetry <- readOGR("./path.shp")
plot(altimetry)

# Subset contour lines to 10m to enhance visualization
contour_plot <- altimetry[(altimetry$ELEV) %in% 
                            seq(min(altimetry$ELEV), 
                                max(altimetry$ELEV), 
                                10), ]                                                       

dem_rs <- raster("./path.asc")
plot(dem_rs)
dem_rs

contour(dem_rs, col = "black", add = TRUE)

# Plot 2D DEM with contour lines
plot(dem_rs, col = terrain.colors(10))
plot(contour_plot, add = T)

landscape_classified <- classify_habitats(raster = terra::rast(dem_rs), n = 6, style = "fisher")
plot(landscape_classified)
plot(plot, add = T)
ext(landscape_classified)
class(landscape_classified)

rt<- raster(landscape_classified)
class(rt)
rt

plot(contour_plot, add = T)

# Alphabetic order
bn <- as.factor(rt)

# Add a column with the new attribute
rat <- levels(bn)[[1]]
rat[["bn"]] <- c("River basin",
                 "Streamsides",
                 "Sunny slopes",
                 "Shady slopes",
                 "Ridges",
                 "Hilltop")
levels(bn) <- rat
class(bn)

# Plot
hab <- levelplot(bn,
                    margin=FALSE,
                    col.regions=rev(terrain.colors(6)),
                    at = seq(from = 1, to = 6, by = 1),
                    xlab="",
                    ylab="",
                    main = expression("Habitats | Pb"))

plot(hab)

cls <- data.frame(id=1:6, cover=c("River basin",
                                  "Streamsides",
                                  "Sunny slopes",
                                  "Shady slopes",
                                  "Ridges",
                                  "Hilltop"))
levels(landscape_classified) <- cls
is.factor(landscape_classified)
landscape_classified