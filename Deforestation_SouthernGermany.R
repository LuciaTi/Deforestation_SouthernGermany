##########################################################
## Deforestation of montane forests in southern Germany ##
##########################################################


## Theme Description:


## Chosen Data:
# path 193
# row 27

# Landsat Level-2 data (surface reflectance)
# Landsat 4/5 TM (1988, 2011)

# band designations Landsat 4/5 TM:
# 1 Blue
# 2 Green
# 3 Red
# 4 Near Infrared
# 5 Shortwave Infrared 1
# (6 Thermal, not included in chosen data)
# 7 Shortwave Infrared 2


## wor2king directory:
setwd("C:/Users/Ltischer/Documents/Studium/A Master/Geostatistics/Deforestation_SouthernGermany")


## Used Libraries:
library(raster)
library(RStoolbox)
library(rgdal)
library(ggplot2)
library(gridExtra)
library(rgeos)


## 1) Reading in the Data, creating Marktoberdorf-buffer, cropping and first plotting ####



########## 1. Reading in and transforming the coordinates of Marktoberdorf
# Therefore: Read in the data from 1988 first for projection! (see below)

gps_Marktoberdorf <- as.data.frame(cbind( x=10.6158265, y=47.778483, id=1))

coordinates(gps_Marktoberdorf) <- c("x", "y")

projection(gps_Marktoberdorf)
projection(p193r27_1988_cdr)

proj4string(gps_Marktoberdorf) <- CRS("+proj=longlat +datum=WGS84")
gps_Marktoberdorf <- spTransform(gps_Marktoberdorf, CRS(projection(p193r27_1988_cdr)))

gps_Marktoberdorf





########## 2. Creating a 25km-Buffer around Marktoberdorf
buf_25 <- gBuffer(gps_Marktoberdorf, width=25000, byid=TRUE) 

# prepare the polygons for plotting
buf_25_df <- fortify(buf_25, region="id") # transform the buffer-data to dataframe
buf_25_df_final <- merge(buf_25_df, buf_25@data, by="id") # add the information

# transform also coordinates back to dataframe for plotting
gps_Marktoberdorf_df <- as.data.frame(gps_Marktoberdorf) 


plot(buf_25)
plot(gps_Marktoberdorf, add=TRUE)





########## 3. Data from 1988 

p193r27_1988_xml <- readMeta("raster/LT05_p193r27_1988_07_20/LT05_L1TP_193027_19880720_20170208_01_T1.xml")
p193r27_1988_cdr <- stackMeta(p193r27_1988_xml, quantity=c("sre", "bt")) # --> no bt-bands, only sre-bands are loaded

scaleF_1988 <- getMeta(p193r27_1988_cdr, p193r27_1988_xml, what="SCALE_FACTOR")
scaleF_1988

# transform DNs back to real values
p193r27_1988_cdr <- p193r27_1988_cdr*scaleF_1988

# plot the data from 1985 and create an extent to crop
ggRGB(p193r27_1988_cdr, stretch="lin")+
  ggtitle("1988")



# create the extent of the 25km-buffer and crop to this extent
ex <- extent(buf_25)
p193r27_1988_cdr_crop <- crop(p193r27_1988_cdr, extent(buf_25))


# plot the data from 1985 and create an extent to crop
ggRGB(p193r27_1988_cdr_crop,
      stretch="lin", geom_raster=TRUE) +
  ggtitle("1988_crop") +
  theme(plot.title=element_text(size=12, colour="black", face="bold"), 
        legend.title=element_text(size=10, colour = "black", face="bold")) +
  geom_polygon(data=buf_25_df_final, 
               mapping=aes(x=buf_25_df_final$long, 
                           y=buf_25_df_final$lat,
                           group=group), 
               colour="orange", 
               fill="orange", 
               alpha=0.3) +
  geom_point(data=gps_Marktoberdorf_df,
             aes(x=gps_Marktoberdorf_df$x,y=gps_Marktoberdorf_df$y), 
             colour="red", 
             shape=3, size=3, stroke=1.5)





########## 4. Data from 2011
p193r27_2011_xml <- readMeta("raster/LT05_p193r27_2011_07_04/LT05_L1TP_193027_20110704_20161008_01_T1.xml")
p193r27_2011_cdr <- stackMeta(p193r27_2011_xml, quantity=c("sre", "bt")) # --> no bt-bands, only sre-bands are loaded

scaleF_2011 <- getMeta(p193r27_2011_cdr, p193r27_2011_xml, what="SCALE_FACTOR")
scaleF_2011

# transform DNs back to real values
p193r27_2011_cdr <- p193r27_2011_cdr*scaleF_2011 

# plot all the scene from 2011
ggRGB(p193r27_2011_cdr, stretch="lin", r=3, g=2, b=1)+
  ggtitle("2011")


# create the extent of the 25 k buffer
p193r27_2011_cdr_crop <- crop(p193r27_2011_cdr, extent(buf_25))



# plot the data from 2011
ggRGB(p193r27_2011_cdr_crop,
      stretch="lin", geom_raster=TRUE) +
  ggtitle("2011_crop") +
  theme(plot.title=element_text(size=12, colour="black", face="bold"), 
        legend.title=element_text(size=10, colour = "black", face="bold")) +
  geom_polygon(data=buf_25_df_final, 
               mapping=aes(x=buf_25_df_final$long, 
                           y=buf_25_df_final$lat,
                           group=group), 
               colour="orange", 
               fill="orange", 
               alpha=0.3) +
  geom_point(data=gps_Marktoberdorf_df,
             aes(x=gps_Marktoberdorf_df$x,y=gps_Marktoberdorf_df$y), 
             colour="red", 
             shape=3, size=3, stroke=1.5)





## 2) Check for Artefacts and remove them ####

########## 1. Data from 1988
boxplot(p193r27_1988_cdr, main="Values Bands 1-6, p193r27 from 1988")
lines(x=c(0,8), y=c(1,1), col="red", lty="dashed")
lines(x=c(0,8), y=c(0,0), col="red", lty="dashed")
summary(p193r27_1988_cdr)
## --> some single data points are outside of the range [0,1]

# remove these points from the data
p193r27_1988_cdr_new <- p193r27_1988_cdr # create a copy of the data
query_raster_p193r27_1988 <- ((p193r27_1988_cdr_new[[1:6]] < 0) | (p193r27_1988_cdr_new[[1:6]] > 1)) # create a query raster, which defines the values outside of the Intervall [0, 1]
p193r27_1988_cdr_new[query_raster_p193r27_1988] <- NA # set all these values to zero

# plot the new data again
boxplot(p193r27_1988_cdr_new, 
        main="Values Bands 1-6, p193r27_new from 1988", 
        ylim=c(-0.2,1.2))
lines(x=c(0,8), y=c(1,1), col="red", lty="dashed")
lines(x=c(0,8), y=c(0,0), col="red", lty="dashed")


# create the extract of 25-km-buffer from the data and plot it
p193r27_1988_cdr_new_crop <- crop(p193r27_1988_cdr_new, extent(buf_25))
ggRGB(p193r27_1988_cdr_new_crop, stretch="lin") +
  ggtitle("Cropped Scene from 1988\n(25 km-buffer around Marktoberdorf)") +
  theme(plot.title=element_text(size=12, colour="black", face="bold"))

# create also a mask for only the buffer area
p193r27_1988_cdr_new_crop_mask <- mask(p193r27_1988_cdr_new_crop, buf_25)
ggRGB(p193r27_1988_cdr_new_crop_mask, stretch="lin") +
  ggtitle("Cropped Scene from 1988\n(ONLY 25 km-buffer around Marktoberdorf)") +
  theme(plot.title=element_text(size=12, colour="black", face="bold"))

# save the results
writeRaster(p193r27_1988_cdr_new, "results/p193r27_1988_cdr_new.grd")
writeRaster(p193r27_1988_cdr_new_crop, "results/p193r27_1988_cdr_new_crop.grd")
writeRaster(p193r27_1988_cdr_new_crop_mask, "results/p193r27_1988_cdr_new_crop_mask.grd")





########## 2. Data from 2011
boxplot(p193r27_2011_cdr, main="Values Bands 1-6, p193r27 from 2011")
lines(x=c(0,8), y=c(1,1), col="red", lty="dashed")
lines(x=c(0,8), y=c(0,0), col="red", lty="dashed")
summary(p193r27_2011_cdr)
## --> some single data points are outside of the range [0,1]

# remove these points from the data
p193r27_2011_cdr_new <- p193r27_2011_cdr # create a copy of the data
query_raster_p193r27_2011 <- ((p193r27_2011_cdr_new[[1:6]] < 0) | (p193r27_2011_cdr_new[[1:6]] > 1)) # create a query raster, which defines the values outside of the Intervall [0, 1]
p193r27_2011_cdr_new[query_raster_p193r27_2011] <- NA # set all these values to zero

# plot the new data again
boxplot(p193r27_2011_cdr_new, 
        main="Values Bands 1-6, p193r27_new from 2011", 
        ylim=c(-0.2, 1.2))
lines(x=c(0,8), y=c(1,1), col="red", lty="dashed")
lines(x=c(0,8), y=c(0,0), col="red", lty="dashed")


# create the extract of 25-km-buffer from the data and plot it
p193r27_2011_cdr_new_crop <- crop(p193r27_2011_cdr_new, extent(buf_25))
ggRGB(p193r27_2011_cdr_new_crop, stretch="lin") +
  ggtitle("Cropped Scene from 2011\n(25 km-buffer around Marktoberdorf)") +
  theme(plot.title=element_text(size=12, colour="black", face="bold"))

# create also a mask for only the buffer area
p193r27_2011_cdr_new_crop_mask <- mask(p193r27_2011_cdr_new_crop, buf_25)
ggRGB(p193r27_2011_cdr_new_crop_mask, stretch="lin") +
  ggtitle("Cropped Scene from 2011\n(ONLY 25 km-buffer around Marktoberdorf)") +
  theme(plot.title=element_text(size=12, colour="black", face="bold"))

# save the results
writeRaster(p193r27_2011_cdr_new, "results/p193r27_2011_cdr_new.grd")
writeRaster(p193r27_2011_cdr_new_crop, "results/p193r27_2011_cdr_new_crop.grd")
writeRaster(p193r27_2011_cdr_new_crop_mask, "results/p193r27_2011_cdr_new_crop_mask.grd")



  
## 3) Identify and Remove Cloud Pixels (no big cloud contained on images --> no extra processing done.) ####

# process follows the device/description of package (https://cran.r-project.org/web/packages/RStoolbox/RStoolbox.pdf)
# only the cropped part of each scene has been cloudMasked

########## 1. Data from 1988
cloudmask_1988_crop <- cloudMask(p193r27_1988_cdr_crop, blue=1, tir=6)
ggR(cloudmask_1988_crop, geom_raster=TRUE)
# --> no clouds identified

########## 2. Data from 2011
cloudmask_2011_crop <- cloudMask(p193r27_2011_cdr_crop, blue=1, tir=6)
ggR(cloudmask_2011_crop, geom_raster=TRUE)
# --> no clouds identified




## 4) Topografic Illumination Correction #####

# load the scenes if calculated before
p193r27_1988_cdr_new_crop_mask <- brick("results/p193r27_1988_cdr_new_crop_mask.grd")
p193r27_2011_cdr_new_crop_mask <- brick("results/p193r27_2011_cdr_new_crop_mask.grd")

# load also data which is not masked yet (for adopting the resolution of the elevation data)
p193r27_1988_cdr_new_crop <- brick("results/p193r27_1988_cdr_new_crop.grd")
p193r27_2011_cdr_new_crop <- brick("results/p193r27_2011_cdr_new_crop.grd")


# download the elevation data for germany
x <- getData("ISO3") # store all the country codes as variable
x$ISO3[x$NAME == "Germany"] # query the country code for Germany
elev_germany <- getData('alt', country = "DEU") # download the elvation data

# compare the projections of elevation data and Marktoberdorf data
identical(projection(elev_germany), projection(p193r27_1988_cdr_new_crop_mask))
identical(projection(elev_germany), projection(p193r27_2011_cdr_new_crop_mask)) # --> the projections differ!

# reproject the elevation data
elev_germany_proj <- projectRaster(elev_germany, crs=crs(p193r27_1988_cdr_new_crop_mask))
identical(projection(elev_germany_proj), projection(p193r27_2011_cdr_new_crop_mask))
identical(projection(elev_germany_proj), projection(p193r27_1988_cdr_new_crop_mask)) # --> now they are identical

# crop the elevation data to scene-size
elev_germany_proj_crop <- crop(elev_germany_proj, p193r27_1988_cdr_new_crop_mask)
elev_germany_proj_crop_mask <- mask(elev_germany_proj_crop, buf_25)

# compare the resolution and dimension of elevation data and scene data
elev_germany_proj_crop
p193r27_1988_cdr_new_crop # --> elevation data has different dimensions and different resolution (non-square)

# resample the elevation data to adopt resolution and dimension to scene-data
elev_germany_proj_crop_resampled <- resample(elev_germany_proj_crop, p193r27_1988_cdr_new_crop, method="bilinear")

# check resolution and dimensions again
elev_germany_proj_crop_resampled
p193r27_1988_cdr_new_crop # --> parameters are identical now


# safe the created elevations data
writeRaster(elev_germany, "results/elev_germany.grd")
writeRaster(elev_germany_proj_crop_mask, "results/elev_germany_proj_crop_mask.grd")
writeRaster(elev_germany_proj_crop_resampled, "results/elev_germany_proj_crop_resampled.grd")


# load again the metadata for the tow scenes
p193r27_1988_xml <- readMeta("raster/LT05_p193r27_1988_07_20/LT05_L1TP_193027_19880720_20170208_01_T1.xml")
p193r27_2011_xml <- readMeta("raster/LT05_p193r27_2011_07_04/LT05_L1TP_193027_20110704_20161008_01_T1.xml")


# apply the topografic illumination correction
p193r27_1988_cdr_new_crop_illu <- topCor(p193r27_1988_cdr_new_crop, 
                                         dem=elev_germany_proj_crop_resampled, 
                                         metaData=p193r27_1988_xml, 
                                         method="C")

p193r27_2011_cdr_new_crop_illu <- topCor(p193r27_2011_cdr_new_crop, 
                                         dem=elev_germany_proj_crop_resampled, 
                                         metaData=p193r27_2011_xml, 
                                         method="C")

# plot to compare the results (2 plots in same window)
g1 <- ggRGB(p193r27_1988_cdr_new_crop, stretch="lin") +
            ggtitle("1988_crop without illumination correction")
g2 <- ggRGB(p193r27_1988_cdr_new_crop_illu, stretch="lin") +
            ggtitle("1988_crop with illumination correction")

g3 <- ggRGB(p193r27_2011_cdr_new_crop, stretch="lin") +
            ggtitle("2011_crop without illumination correction")
g4 <- ggRGB(p193r27_2011_cdr_new_crop_illu, stretch="lin") +
            ggtitle("2011_crop with illumination correction")

grid.arrange(g1, g2, ncol=2, nrow =1)
grid.arrange(g3, g4, ncol=2, nrow =1)


# plot to compare the results (2 plots per window, extract fromt the scene)
extract_illu <- extent(c(596055, 620000, 5267955, 5285000))
p193r27_1988_cdr_new_crop_extract <- crop(p193r27_1988_cdr_new_crop, extract_illu)
p193r27_1988_cdr_new_crop_illu_extract <- crop(p193r27_1988_cdr_new_crop_illu, extract_illu)
p193r27_2011_cdr_new_crop_extract <- crop(p193r27_2011_cdr_new_crop, extract_illu)
p193r27_2011_cdr_new_crop_illu_extract <- crop(p193r27_2011_cdr_new_crop_illu, extract_illu)

g1b <- ggRGB(p193r27_1988_cdr_new_crop_extract, stretch="lin") +
  ggtitle("Extract of 1988_crop without illumination correction")
g2b <- ggRGB(p193r27_1988_cdr_new_crop_illu_extract, stretch="lin") +
  ggtitle("Extract of 1988_crop with illumination correction")

g3b <- ggRGB(p193r27_2011_cdr_new_crop_extract, stretch="lin") +
  ggtitle("Extract of 2011_crop without illumination correction")
g4b <- ggRGB(p193r27_2011_cdr_new_crop_illu_extract, stretch="lin") +
  ggtitle("Ectract of 2011_crop with illumination correction")

grid.arrange(g1b, g2b, ncol=2, nrow =1)
grid.arrange(g3b, g4b, ncol=2, nrow =1)




# mask the corrected images to buffer-zone
p193r27_1988_cdr_new_crop_illu_mask <- mask(p193r27_1988_cdr_new_crop_illu, buf_25)
ggRGB(p193r27_1988_cdr_new_crop_illu_mask, stretch="lin") +
  ggtitle("1988, illumination corrected, 25-km-buffer around Marktoberdorf")

p193r27_2011_cdr_new_crop_illu_mask <- mask(p193r27_2011_cdr_new_crop_illu, buf_25)
ggRGB(p193r27_2011_cdr_new_crop_illu_mask, stretch="lin") +
  ggtitle("2011, illumination corrected, 25-km-buffer around Marktoberdorf")



# save the results
writeRaster(p193r27_1988_cdr_new_crop_illu, "results/p193r27_1988_cdr_new_crop_illu.grd")
writeRaster(p193r27_1988_cdr_new_crop_illu_mask, "results/p193r27_1988_cdr_new_crop_illu_mask.grd")
writeRaster(p193r27_2011_cdr_new_crop_illu, "results/p193r27_2011_cdr_new_crop_illu.grd")
writeRaster(p193r27_2011_cdr_new_crop_illu_mask, "results/p193r27_2011_cdr_new_crop_illu_mask.grd")



## 5) Landcover Classifications of single scenes ####

# Classification with supervised Classification (superClass RStoolbox).
# The classes 1: forest, 2: water, 3: urban/streets, 4: grassland/agriculture were distinguished.
# Training data consists of 5 polygons per class.
# Validation data also consists of 5 polygons per class.
# Classified cdr-raster data (Landsat 4-5 TM, Level 2) were illumination corrected ("C"-Method) 
# and masked to the 25km-buffer around Marktoberdorf.
# Chosen model was: "rf"


########## 1. read in the training- and validation data (defined in QGIS) and Raster scene
p193r27_1988_cdr_new_crop_illu_mask <- brick("results/p193r27_1988_cdr_new_crop_illu_mask.grd")
train_1988 <- readOGR(dsn="vector", layer="training_1988")
validate_1988 <- readOGR(dsn="vector", layer="validation_1988")

p193r27_2011_cdr_new_crop_illu_mask <- brick("results/p193r27_2011_cdr_new_crop_illu_mask.grd")
train_2011 <- readOGR(dsn="vector", layer="training_2011")
validate_2011 <- readOGR(dsn="vector", layer="validation_2011")


# make sure that the projections are identical
identical(projection(p193r27_1988_cdr_new_crop_illu_mask), projection(train_1988))
identical(projection(p193r27_1988_cdr_new_crop_illu_mask), projection(validate_1988))

identical(projection(p193r27_2011_cdr_new_crop_illu_mask), projection(train_2011))
identical(projection(p193r27_2011_cdr_new_crop_illu_mask), projection(validate_2011))




########## 2. Calucalte the NDVI per Scene and stack it to raster data
# NDVI 1988
p193r27_1988_cdr_NDVI <- spectralIndices(p193r27_1988_cdr_new_crop_illu_mask, 
                                         blue=1, red=2, green=3, nir=4, 
                                         indices="NDVI")
# stack NDVI with raster-bands
p193r27_1988_cdr_new_crop_illu_mask_NDVI <- stack(p193r27_1988_cdr_new_crop_illu_mask, p193r27_1988_cdr_NDVI)
# safe the "pure" NDVI values for later use
writeRaster(p193r27_1988_cdr_NDVI, "results/p193r27_1988_cdr_NDVI.grd")


# NDVI 2011
p193r27_2011_cdr_NDVI <- spectralIndices(p193r27_2011_cdr_new_crop_illu_mask, 
                                         blue=1, red=2, green=3, nir=4, 
                                         indices="NDVI")
# stack NDVI with raster-bands
p193r27_2011_cdr_new_crop_illu_mask_NDVI <- stack(p193r27_2011_cdr_new_crop_illu_mask, p193r27_2011_cdr_NDVI)
# safe the "pure" NDVI values for later use
writeRaster(p193r27_2011_cdr_NDVI, "results/p193r27_2011_cdr_NDVI.grd") 



# save the results
writeRaster(p193r27_1988_cdr_new_crop_illu_mask_NDVI, "results/p193r27_1988_cdr_new_crop_illu_mask_NDVI.grd")
writeRaster(p193r27_2011_cdr_new_crop_illu_mask_NDVI, "results/p193r27_2011_cdr_new_crop_illu_mask_NDVI.grd")

# reload results
p193r27_1988_cdr_new_crop_illu_mask_NDVI <- brick("results/p193r27_1988_cdr_new_crop_illu_mask_NDVI.grd")
p193r27_2011_cdr_new_crop_illu_mask_NDVI <- brick("results/p193r27_2011_cdr_new_crop_illu_mask_NDVI.grd")

p193r27_1988_cdr_NDVI <- raster("results/p193r27_1988_cdr_NDVI.grd")
p193r27_2011_cdr_NDVI <- raster("results/p193r27_2011_cdr_NDVI.grd")




########## 3. run the classifications
# classifications 1988
set.seed(6)
p193r27_1988_cdr_sclass1 <- superClass(img = p193r27_1988_cdr_new_crop_illu_mask, 
                                       model = "rf", 
                                       trainData = train_1988, 
                                       valData = validate_1988,
                                       responseCol = "id")

set.seed(6)
p193r27_1988_cdr_sclass2 <- superClass(img = p193r27_1988_cdr_new_crop_illu_mask_NDVI, 
                                       model = "rf", 
                                       trainData = train_1988, 
                                       valData = validate_1988,
                                       responseCol = "id")

set.seed(6)
p193r27_1988_cdr_sclass3 <- superClass(img = p193r27_1988_cdr_NDVI, 
                                       model = "rf", 
                                       trainData = train_1988, 
                                       valData = validate_1988,
                                       responseCol = "id")

## !! ##
set.seed(6)
p193r27_1988_cdr_sclass4 <- superClass(img = p193r27_1988_cdr_new_crop_illu_mask, 
                                       model = "svmRadial", 
                                       trainData = train_1988, 
                                       valData = validate_1988,
                                       responseCol = "id")


set.seed(6)
p193r27_1988_cdr_sclass5 <- superClass(img = p193r27_1988_cdr_new_crop_illu_mask, 
                                       model = "pls", 
                                       trainData = train_1988, 
                                       valData = validate_1988,
                                       responseCol = "id")

set.seed(6)
p193r27_1988_cdr_sclass6 <- superClass(img = p193r27_1988_cdr_new_crop_illu_mask_NDVI, 
                                       model = "svmRadial", 
                                       trainData = train_1988, 
                                       valData = validate_1988,
                                       responseCol = "id")





#classifications 2011
set.seed(6)
p193r27_2011_cdr_sclass1 <- superClass(img = p193r27_2011_cdr_new_crop_illu_mask, 
                                       model = "rf", 
                                       trainData = train_2011, 
                                       valData = validate_2011,
                                       responseCol = "id")

set.seed(6)
p193r27_2011_cdr_sclass2 <- superClass(img = p193r27_2011_cdr_new_crop_illu_mask_NDVI, 
                                       model = "rf", 
                                       trainData = train_2011, 
                                       valData = validate_2011,
                                       responseCol = "id")

set.seed(6)
p193r27_2011_cdr_sclass3 <- superClass(img = p193r27_2011_cdr_NDVI, 
                                       model = "rf", 
                                       trainData = train_2011, 
                                       valData = validate_2011,
                                       responseCol = "id")

set.seed(6)
p193r27_2011_cdr_sclass4 <- superClass(img = p193r27_2011_cdr_new_crop_illu_mask, 
                                       model = "svmRadial", 
                                       trainData = train_2011, 
                                       valData = validate_2011,
                                       responseCol = "id")

set.seed(6)
p193r27_2011_cdr_sclass5 <- superClass(img = p193r27_2011_cdr_new_crop_illu_mask, 
                                       model = "pls", 
                                       trainData = train_2011, 
                                       valData = validate_2011,
                                       responseCol = "id")


## !! ##
set.seed(6)
p193r27_2011_cdr_sclass6 <- superClass(img = p193r27_2011_cdr_new_crop_illu_mask_NDVI, 
                                       model = "svmRadial", 
                                       trainData = train_2011, 
                                       valData = validate_2011,
                                       responseCol = "id")





########## 4. Evaluate the quality of the Classifications

# accuracy for 1988
p193r27_1988_cdr_sclass1$validation$performance # Accuracy : 0.8162 ## rf without NDVI is best Model
p193r27_1988_cdr_sclass1$modelFit
p193r27_1988_cdr_sclass2$validation$performance # Accuracy : 0.8113
p193r27_1988_cdr_sclass3$validation$performance # Accuracy : 0.6237  
p193r27_1988_cdr_sclass4$validation$performance # Accuracy : 0.8043  
p193r27_1988_cdr_sclass5$validation$performance # Accuracy : 0.7645
p193r27_1988_cdr_sclass6$validation$performance # Accuracy : 0.8067

# accuracy for 2011
p193r27_2011_cdr_sclass1$validation$performance # Accuracy : 0.8986 
p193r27_2011_cdr_sclass1$modelFit
p193r27_2011_cdr_sclass2$validation$performance # Accuracy : 0.8943
p193r27_2011_cdr_sclass3$validation$performance # Accuracy : 0.6211 
p193r27_2011_cdr_sclass4$validation$performance # Accuracy : 0.9155
p193r27_2011_cdr_sclass5$validation$performance # Accuracy : 0.8355
p193r27_2011_cdr_sclass6$validation$performance # Accuracy : 0.9163 ## svmRadial mit NDVI is best Model


# save the map of the best classification
writeRaster(p193r27_1988_cdr_sclass4$map, "results/p193r27_1988_cdr_sclass4$map.grd")
writeRaster(p193r27_2011_cdr_sclass6$map, "results/p193r27_2011_cdr_sclass6$map.grd")


## !! ## 
# one the first run, 1988/sc_4 and 2011/sc_6 were the best Classifications.
# one the second run, 1988/sc_1 and 2011/sc_6 were the best Classifications (with set.seed)



########## 5. plot the Best Classifications
# 2011: "new" lake in the southwest --> Rottachstausee --> planned and constructed in the 70ths and 80ths
# first time flooded: 1990
# regulary used since: 1992
# --> add a mark in plot?

cols <- c("1"="seagreen3", "2"="blue", "3"="darkred", "4"="khaki1")

### Plot 1988
ggR(p193r27_1988_cdr_sclass1$map, forceCat = TRUE, geom_raster = TRUE) +
  ggtitle(paste("Landscape 1988\nSupervised classification 2 - model: rf")) +
  theme(plot.title = element_text(size = 12, colour = "black", face="bold"), 
        legend.title= element_text(size=11, colour="black", face="bold")) +
  scale_fill_manual(values = cols, 
                    labels=c("Class1: Forest", "Class2: Water", "Class3: Urban/Streets", "Class4: Agriculture/Grassland"), 
                    name = "Landcover\nClasses\n")



### Plot 2011
# define the coordinates of Rottachsee
gps_Rottach <- as.data.frame(cbind( x=10.394951, y=47.652036, id=1))
coordinates(gps_Rottach) <- c("x", "y")

projection(gps_Rottach)
projection(p193r27_1988_cdr)

proj4string(gps_Rottach) <- CRS("+proj=longlat +datum=WGS84")
gps_Rottach <- spTransform(gps_Rottach, CRS(projection(p193r27_1988_cdr)))

gps_Rottach


#Create a 5km-Buffer around Rottachsee
buf_5 <- gBuffer(gps_Rottach, width=5000, byid=TRUE) 

# prepare the polygons for plotting
buf_5_df <- fortify(buf_5, region="id") # transform the buffer-data to dataframe
buf_5_df_final <- merge(buf_5_df, buf_5@data, by="id") # add the information

# transform also coordinates back to dataframe for plotting
gps_Rottach_df <- as.data.frame(gps_Rottach) 
plot(buf_5)
plot(gps_Rottach, add=TRUE)


ggR(p193r27_2011_cdr_sclass6$map, forceCat = TRUE, geom_raster = TRUE) +
  ggtitle(paste("Landscape 2011\nSupervised classification 2 - model: svmRadial, with NDVI")) +
  theme(plot.title = element_text(size = 12, colour = "black", face="bold"), 
        legend.title= element_text(size=11, colour="black", face="bold")) +
  scale_fill_manual(values = cols, 
                    labels=c("Class1: Forest", "Class2: Water", "Class3: Urban/Streets", "Class4: Agriculture/Grassland"), 
                    name = "Landcover\nClasses\n")+
  geom_polygon(data=buf_5_df_final, 
               mapping=aes(x=buf_5_df_final$long, 
                           y=buf_5_df_final$lat,
                           group=group), 
               colour="red", 
               fill="red", 
               alpha=0.1)






## 5) Comparison of Landcover ####


########## 1. Area and Percentage of Landcover-Types in 1988
# count the pixels per class
p193r27_1988_cdr_sclass1.freq <- freq(p193r27_1988_cdr_sclass1$map, useNA="no")

# transform the Pixel-counts to surface-area
resLsat5_1988 <- res(p193r27_1988_cdr_new_crop_illu_mask) # query the spatial resolution of one pixel
area_km2_1988 <- p193r27_1988_cdr_sclass1.freq[ , "count"] * prod(resLsat5_1988) * 1e-6 # calculate the surface per class with: number of counts * pixelsize (30^2) * 1e-06 (in kilometers)
area_percent_1988 <- area_km2_1988/sum(area_km2_1988) # calculate also the %-Area per Lancover Type
area_km2_1988_df <- data.frame(Year=c(rep(1988,4)),
                               ClassID = c(1,2,3,4), 
                               Landcover = c("Forest", "Water", "Urban", "Agriculture"), 
                               area_km2 = area_km2_1988, 
                               area_percent = area_percent_1988)




########## 2. Area and Percetage of Landcover-Types in 2011
# count the pixels per class
p193r27_2011_cdr_sclass6.freq <- freq(p193r27_2011_cdr_sclass6$map, useNA="no")

# transform the Pixel-counts to surface-area
resLsat5_2011 <- res(p193r27_2011_cdr_new_crop_illu_mask) # query the spatial resolution of one pixel
area_km2_2011<- p193r27_2011_cdr_sclass6.freq[ , "count"] * prod(resLsat5_2011) * 1e-6 # calculate the surface per class with: number of counts * pixelsize (30^2) * 1e-06 (in kilometers)
area_percent_2011 <- area_km2_2011/sum(area_km2_2011) # calculate also the %-Area per Lancover Type
area_km2_2011_df <- data.frame(Year=c(rep(2011,4)),
                               ClassID = c(1,2,3,4), 
                               Landcover = c("Forest", "Water", "Urban", "Agriculture"), 
                               area_km2 = area_km2_2011, 
                               area_percent = area_percent_2011)

# connect the two dataframes to one
area_km2_df_complete <- rbind(area_km2_1988_df, area_km2_2011_df)




########## 3. Plotting the Percentage of Landcover Types

cols_2 <- c("Forest"="seagreen3", "Water"="blue", "Urban"="brown1", "Agriculture"="khaki1")

ggplot(data=area_km2_df_complete, 
       aes(x=as.factor(Year), 
           y=area_percent*100, 
           fill=Landcover, 
           label=round(area_percent*100, 2))) +
  geom_bar(stat="identity", 
           width=0.7) + 
  #geom_text(size = 4.5, 
  #          position = position_stack(vjust = 1.1), 
  #          colour="black") +
  geom_text(size = 6, hjust = 0.5, vjust = 1.6, position = "stack") +
  scale_fill_manual(values =cols_2) +
  ggtitle("Area per Landcover Class [%]\n\n") +
  xlab("\n\nYear") + 
  ylab("Amount per Landcover Class [%]\n\n") +
  theme(plot.title = element_text(size = 15, colour = "black", face="bold"), 
        legend.title=element_text(size = 14, colour = "black", face="bold"),
        legend.text = element_text(size = 12, colour = "black", face="plain"),
        axis.title.y = element_text(size = 14, colour = "black", face="bold"), 
        axis.title.x = element_text(size = 14, colour = "black", face="bold"), 
        axis.text.x = element_text(size = 13, colour = "black", face="bold"), 
        axis.text.y = element_text(size = 13, colour = "black", face="bold"), 
        plot.margin = unit(c(2,2,2,2), "lines"))





## 6) Comparison of Productivity per Zone and Year ####

# load the NDVI-Values
p193r27_1988_cdr_NDVI <- raster("results/p193r27_1988_cdr_NDVI.grd")
p193r27_2011_cdr_NDVI <- raster("results/p193r27_2011_cdr_NDVI.grd")

# group the single pixel- NDVI-values per class
forest_prod_1988 <- p193r27_1988_cdr_NDVI[p193r27_1988_cdr_sclass4$map$id == 1,]
water_prod_1988 <- p193r27_1988_cdr_NDVI[p193r27_1988_cdr_sclass4$map$id == 2,]
urban_prod_1988 <- p193r27_1988_cdr_NDVI[p193r27_1988_cdr_sclass4$map$id == 3,]
agriculture_prod_1988 <- p193r27_1988_cdr_NDVI[p193r27_1988_cdr_sclass4$map$id == 4,]
prod_1988 <- rbind(forest_prod_1988, 
                   water_prod_1988, 
                   urban_prod_1988, 
                   agriculture_prod_1988)
landcover_1988 <- rbind(rep("Forest", length(forest_prod_1988)), 
                        rep("Water", length(water_prod_1988)), 
                        rep("Urban", length(urban_prod_1988)), 
                        rep("Agriculture", length(agriculture_prod_1988)))

prod_landcover_1988 <- data.frame(Year=rep("1988", length(prod_1988)), 
                                  Landcover=landcover_1988, 
                                  Productivity= prod_1988)


forest_prod_2011 <- p193r27_2011_cdr_NDVI[p193r27_2011_cdr_sclass6$map$id == 1, ]
water_prod_2011 <- p193r27_2011_cdr_NDVI[p193r27_2011_cdr_sclass6$map$id == 2, ]
urban_prod_2011 <- p193r27_2011_cdr_NDVI[p193r27_2011_cdr_sclass6$map$id == 3, ]
agriculture_prod_2011 <- p193r27_2011_cdr_NDVI[p193r27_2011_cdr_sclass6$map$id == 4, ]


# plot the Productivity per class

landcover_classes <- names(prod_1988_df)
boxplot(prod_landcover_1988)
g1c <- ggplot(prod_1988_df, aes(x=names(prod_1988)))
g1c




## 4) Defining elevation zones ####


## 6) Comparison of Productivity per Zone ####


## 7) Productivity-Change of deforested areas/ Agricultural Areas then and now ####


## 8) Productivity-Change of deforested areas of different elevation ####



