
######################################################################
#### TREE SPECIES CLASSIFICATION WITH PHOTOGRAMMETRIC POINT CLOUD ####
#### UEF ADVANCED REMOTE SENSING COURSE                           ####
#### Lauri Korhonen 2024                                          #### 
#### MODEL SOLUTION                                               #### 
######################################################################

# Clear old stuff from memory  
rm(list = ls())

# Only run the following four installations when starting an a new computer
# Comment when not needed

# install.packages("sf")
# install.packages("terra")
#install.packages("ForestTools")
# install.packages("lidR")


# Load installed libraries into active use
require(sf)
require(terra)
require(ForestTools)
require(lidR)

# Set working directory path - all your files should be here

#setwd("C:/Users/Hp/OneDrive - University of Eastern Finland/Desktop/Jyrinvaara_drone")

# Load a set of custom functions
source("webodm_functions.r")

# Read the tree file and set coordinate reference system
trees <- read.csv("C:/Users/Hp/OneDrive - University of Eastern Finland/Desktop/Jyrinvaara_drone/jyrinvaara_trees.csv", dec=".", sep=",")
trees <- st_as_sf(trees, coords = c("X","Y"))
st_crs(trees) <- 3067

#View last rows
tail(trees)


#=================================================
# 4. Point cloud processing 
#=================================================

# Read the point cloud 
las <- lidR::readLAS("C:/Users/Hp/OneDrive - University of Eastern Finland/Desktop/Jyrinvaara_drone/Lieksa-6-13-2019-georeferenced_model.laz")

# Visualization 
plot(las, color = "RGB", legend=F)


# Show the records of the laz file
tail(las@data)

# Correct for local ellipsoid-datum difference
las$Z <- las$Z - 29.854

# Read DTM
dtm <- rast("C:/Users/Hp/OneDrive - University of Eastern Finland/Desktop/Jyrinvaara_drone/jyrinvaara_dtm.tif")

# Height normalization
las <- las - dtm

# View height histogram
hist(las$Z)
summary(las)

# Set negative elevations as zero
las$Z[las$Z < 0] <- 0

#======================================================
# 5. Crown segmentation and computation of predictors
#======================================================

# Interpolate a CHM from the las file
chm <- lidR::rasterize_canopy(las, res = 0.5, algorithm = pitfree(thresholds = c(0,5,10,15,20,25)))

# Plot CHM and field trees
plot(chm)
plot(trees, add=T, pch=20, col="red")

# Define a function for computing a height-dependent moving window size
window_size <- function(height){ 1.2 + 0.003*height^2 } 

# Compute a georeferenced data frame with tree top coordinates and heights
tops <- locate_trees(chm, lmf(window_size))

# Run marker-controlled watershed segmentation with the detected tops
crowns <- ForestTools::mcws(tops, chm, minHeight = 5, format="polygons")


# Add crown segments on top of the previous plot
plot(crowns, add=T, col="transparent")

# Join the crowns with the field-measured  trees
det_crowns <- st_join(crowns, trees)

# Remove crowns that were not linked to any tree
det_crowns <- det_crowns[! is.na(det_crowns$Species),] #was species previously

# If a crown was linked to multiple trees, keep only the largest diameter
det_crowns <- transform(det_crowns, largest = ave(diameter, treeID, FUN = is.largest)) 
det_crowns <- det_crowns[det_crowns$largest==1,]


# Calculate predictors for each detected crown
d <- polygon_metrics(las, ~ rgb_metrics(Z, R, G, B),  det_crowns)


# View the result
tail(d)

# Reset geometry from the predictor data frame to avoid duplication
d <- st_set_geometry(d, NULL)

# Merge response and predictors
d <- cbind(det_crowns, d)

# Save crown segments and trees as gpkg
st_write(d, "Crown_segments.gpkg", append=F)
st_write(trees, "Trees.gpkg", append=F)

# Save CHM as tif image
writeRaster(chm, "CHM.tif", overwrite=T)

#=================================================
# 6. Tree species classification
#=================================================

# View available predictors
names(d)

# View species proportions in 
table(d$Species)

# Linear discriminant analysis with a custom function
# Note: the variables are automatically normalized to zero mean and unit variance in the function
require(MASS)
lda <- trainLDA(d, yvar="Species", prednum=2, colmin=6, colmax=57, equalPriors=TRUE, opt="OA")

# View lda results
lda$lda

# View discriminant plot. Change legend position if needed ("bottomleft", "topright" etc.)
discrPlot(lda$pred, legpos="bottomleft")

# View predicted values
lda$pred[1]

# View error matrix
lda$errmat

# Calculate overall accuracy yourself!

#require(MASS)
# Linear discriminant analysis with a custom function
# Note: the variables are automatically normalized to zero mean and unit variance in the function
lda <- trainLDA(d, yvar="Species", prednum=2, colmin=6, colmax=57, equalPriors=TRUE, opt="OA")
lda.1 <- trainLDA(d, yvar="Species", prednum=3, colmin=6, colmax=57, equalPriors=TRUE, opt="OA")
lda.2 <- trainLDA(d, yvar="Species", prednum=4, colmin=6, colmax=57, equalPriors=TRUE, opt="OA")
lda.3 <- trainLDA(d, yvar="Species", prednum=5, colmin=6, colmax=57, equalPriors=TRUE, opt="OA")
lda.4 <- trainLDA(d, yvar="Species", prednum=6, colmin=6, colmax=57, equalPriors=TRUE, opt="OA")


# View lda results
lda$lda
lda.1$lda
lda.2$lda
lda.3$lda
lda.4$lda

# View discriminant plot, change legend position if needed ("bottomleft", "topright" etc.)
discrPlot(lda$pred, legpos="topright")
discrPlot(lda.1$pred, legpos="bottomleft")
discrPlot(lda.2$pred, legpos="bottomright")
discrPlot(lda.3$pred, legpos="bottomleft")
discrPlot(lda.4$pred, legpos="bottomleft")

# View predicted values
lda.3$pred[1]

# View error matrix
lda$errmat
lda.1$errmat
lda.2$errmat
lda.3$errmat
lda.4$errmat

#kappa coefficient
lda$kappa
lda.1$kappa
lda.2$kappa
lda.3$kappa
lda.4$kappa
# Calculate overall accuracy yourself!
#overall accuracy
colSums(lda$errmat)/sum(diag(lda$errmat))
colSums(lda.1$errmat)/sum(diag(lda.1$errmat))
colSums(lda.2$errmat)/sum(diag(lda.2$errmat))
colSums(lda.3$errmat)/sum(diag(lda.3$errmat))
colSums(lda.4$errmat)/sum(diag(lda.4$errmat))

oa(lda$errmat)
oa(lda.1$errmat)
oa(lda.2$errmat)
oa(lda.3$errmat)
oa(lda.4$errmat)


((oa(lda.1$errmat)-oa(lda$errmat))/oa(lda$errmat))*100
((oa(lda.2$errmat)-oa(lda.1$errmat))/oa(lda.1$errmat))*100
((oa(lda.3$errmat)-oa(lda.2$errmat))/oa(lda.2$errmat))*100
((oa(lda.4$errmat)-oa(lda.3$errmat))/oa(lda.3$errmat))*100



