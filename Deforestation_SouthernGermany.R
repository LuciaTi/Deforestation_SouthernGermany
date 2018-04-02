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
library(rgeos)


## 1) Reading in the Data, creating Marktoberdorf-buffer, cropping and first plotting ####



########## 1. Reading in and transforming the coordinates of Marktoberdorf
# Therefore: Read in the data from 1988 first for projection! (see below)

gps_Marktoberdorf <- as.data.frame(cbind( x=10.6158265, y=47.778483, id=1))

coordinates(gps_Marktoberdorf) <- c("x", "y")

projection(gps_Marktoberdorf)
projection(p193r27_1985_cdr)

proj4string(gps_Marktoberdorf) <- CRS("+proj=longlat +datum=WGS84")
gps_Marktoberdorf <- spTransform(gps_Marktoberdorf, CRS(projection(p193r27_1985_cdr)))

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



## 5) Landcover Classifications and Productivity of single scenes ####

# Classification with supervised Classification (superClass RStoolbox)
# the classes 1: forest, 2: water, 3: urban/streets, 4: grassland/agriculture were distinguished.
# training data consists of 5 polygons per class.
# validation data also consists of 5 polygons per class.




## 5) Comparison of Landcover ####


## 4) Defining elevation zones ####


## 6) Comparison of Productivity per Zone ####


## 7) Productivity-Change of deforested areas/ Agricultural Areas then and now ####


## 8) Productivity-Change of deforested areas of different elevation ####



