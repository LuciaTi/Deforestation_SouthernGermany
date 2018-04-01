##########################################################
## Deforestation of montane forests in southern Germany ##
##########################################################


## Theme Description:


## Chosen Data:
# path 193
# row 27

# Landsat Level-2 data (surface reflectance)
# Landsat 4/5 TM (1985, 2000)
# Landsat 7 ETM (2015)

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
gps_Marktoberdorf <- as.data.frame(cbind( x=10.6158265, y=47.778483, id=1))

coordinates(gps_Marktoberdorf) <- c("x", "y")

projection(gps_Marktoberdorf)
projection(p193r27_1985_cdr)

proj4string(gps_Marktoberdorf) <- CRS("+proj=longlat +datum=WGS84")
gps_Marktoberdorf <- spTransform(gps_Marktoberdorf, CRS(projection(p193r27_1985_cdr)))

gps_Marktoberdorf





########## Buffer around Marktoberdorf
buf_25 <- gBuffer(gps_Marktoberdorf, width=25000, byid=TRUE) 

# prepare the polygons for plotting
buf_25_df <- fortify(buf_25, region="id") # transform the buffer-data to dataframe
buf_25_df_final <- merge(buf_25_df, buf_25@data, by="id") # add the information

# transform also coordinates back to dataframe for plotting
gps_Marktoberdorf_df <- as.data.frame(gps_Marktoberdorf) 


plot(buf_25)
plot(gps_Marktoberdorf, add=TRUE)



########## (!!!) 1. Data from 1985
#p193r27_1985_xml <- readMeta("raster/LT05_p193r27_1985_09_30/LT05_L1GS_193027_19850930_20171212_01_T2.xml")
#p193r27_1985_cdr <- stackMeta(p193r27_1985_xml, quantity=c("sre", "bt")) # --> no bt-bands, only sre-bands are loaded

#scaleF_1985 <- getMeta(p193r27_1985_cdr, p193r27_1985_xml, what="SCALE_FACTOR")
#scaleF_1985

#p193r27_1985_cdr <- p193r27_1985_cdr*scaleF_1985

#ggRGB(p193r27_1985_cdr, stretch="lin")+
#      ggtitle("1985")




p193r27_1985_2_xml <- readMeta("raster/LT05_p193r27_1985_08_13/LT05_L1TP_193027_19850813_20170219_01_T1.xml")
p193r27_1985_2_cdr <- stackMeta(p193r27_1985_2_xml, quantity=c("sre", "bt")) # --> no bt-bands, only sre-bands are loaded

scaleF_1985_2 <- getMeta(p193r27_1985_2_cdr, p193r27_1985_2_xml, what="SCALE_FACTOR")
scaleF_1985_2

p193r27_1985_2_cdr <- p193r27_1985_2_cdr*scaleF_1985_2

# plot the data from 1985 and create an extent to crop
ggRGB(p193r27_1985_2_cdr, stretch="lin")+
  ggtitle("1985_2")



# create the extent of the 25km-buffer and crop to this extent
ex <- extent(buf_25)
p193r27_1985_2_cdr_crop <- crop(p193r27_1985_2_cdr, extent(buf_25))


# plot the data from 1985 and create an extent to crop
ggRGB(p193r27_1985_2_cdr_crop,
      stretch="lin", geom_raster=TRUE) +
  ggtitle("1985_2_crop") +
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




########## Data from 1988 



########## 2. Data from 2000
#p193r27_2000_xml <- readMeta("raster/LT05_p193r27_2000_10_25/LT05_L1GS_193027_20001025_20171210_01_T2.xml")
#p193r27_2000_cdr <- stackMeta(p193r27_2000_xml, quantity=c("sre", "bt")) # --> no bt-bands, only sre-bands are loaded

#scaleF_2000 <- getMeta(p193r27_2000_cdr, p193r27_2000_xml, what="SCALE_FACTOR")
#scaleF_2000

#p193r27_2000_cdr <- p193r27_2000_cdr*scaleF_2000

#ggRGB(p193r27_2000_cdr, stretch="lin", r=3, g=2, b=1)+
#  ggtitle("2000")
#
#ex_vector <- c(615000, 661500, 5270000, 5310000)
#ex <- extent(ex_vector)

#p193r27_2000_cdr_crop <- crop(p193r27_2000_cdr, ex)
#ggRGB(p193r27_2000_cdr_crop, stretch="lin", r=3, g=2, b=1)+
#  ggtitle("2000_crop")


########## 2. (!!!) Data from 2011
p193r27_2011_xml <- readMeta("raster/LT05_p193r27_2011_07_04/LT05_L1TP_193027_20110704_20161008_01_T1.xml")
p193r27_2011_cdr <- stackMeta(p193r27_2011_xml, quantity=c("sre", "bt")) # --> no bt-bands, only sre-bands are loaded

scaleF_2011 <- getMeta(p193r27_2011_cdr, p193r27_2011_xml, what="SCALE_FACTOR")
scaleF_2011

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





########## Data from 2015
#p193r27_2015_xml <- readMeta("raster/LC08_p193r27_2015_09_01/LC08_L1TP_193027_20150901_20170404_01_T1.xml")
#p193r27_2015_cdr <- stackMeta(p193r27_2015_xml, quantity=c("sre", "bt")) # --> no bt-bands, only sre-bands are loaded

#scaleF_2015 <- getMeta(p193r27_2015_cdr, p193r27_2015_xml, what="SCALE_FACTOR")
#scaleF_2015

#p193r27_2015_cdr <- p193r27_2015_cdr*scaleF_2015

#ggRGB(p193r27_2015_cdr, stretch="lin", r=4, g=3, b=2)+
#  ggtitle("2015")

#ex_vector <- c(615000, 661500, 5270000, 5310000)
#ex <- extent(ex_vector)

#p193r27_2015_cdr_crop <- crop(p193r27_2015_cdr, ex)
#ggRGB(p193r27_2015_cdr_crop, stretch="lin", r=4, g=3, b=2)+
#  ggtitle("2015_crop")



## 2) Check for Artefacts and remove them ####

########## 1. Data of 2015
boxplot(p193r27_2015_cdr, main="Values Bands 1-8, p193r27 from 2015")
lines(x=c(0,8), y=c(1,1), col="red", lty="dashed")
lines(x=c(0,8), y=c(0,0), col="red", lty="dashed")
summary(p193r27_2015_cdr)

p193r27_2015_cdr_new <- p193r27_2015_cdr # create a copy of the data
query_raster_p193r27_2015 <- ((p193r27_2015_cdr_new[[1:6]] < 0) | (p193r27_2015_cdr_new[[1:6]] > 1)) # create a query raster, which defines the values outside of the Intervall [0, 1]
p193r27_2015_cdr_new[query_raster_p193r27_2015] <- NA # set all these values to zero



  
########## 2. Data of 1985
boxplot(p193r27_1985_cdr, main="Values Bands 1-8, p193r27 from 1985")
lines(x=c(0,8), y=c(1,1), col="red", lty="dashed")
lines(x=c(0,8), y=c(0,0), col="red", lty="dashed")
summary(p193r27_1985_cdr)

p193r27_1985_cdr_new <- p193r27_1985_cdr # create a copy of the data
query_raster_p193r27_1985 <- ((p193r27_1985_cdr_new[[1:6]] < 0) | (p193r27_1985_cdr_new[[1:6]] > 1)) # create a query raster, which defines the values outside of the Intervall [0, 1]
p193r27_1985_cdr_new[query_raster_p193r27_1985] <- NA # set all these values to zero



## 3) Control for Clouds ####


## 3) Defining elevation zonation ####


## 4) Landcover Classifications and Productivity of single scenes ####


## 5) Comparison of Landcover ####


## 6) Comparison of Productivity ####


## 7) Productivity-Change of deforested areas ####


## 8) Productivity-Change of deforested areas of different elevation ####



