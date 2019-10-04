######################################################################################################################
##########################Species Distribution Modeling for Conservation in R and Wallace workshop ###################
##################################################AMNH October 4th 2019###############################################
#######################################Data exploration script by Andrea Paz##########################################
######################################################################################################################              

library(dismo)
library(sp)
library(rgdal)
library(raster)
library(rgeos)
setwd("~/Downloads/Workshop/")

##############################################################################################################
############################Exploring the Environmental Layers to be used in a modelling project##############
##############################################################################################################                

###Take a look at the potential layers for modeling in the area of interest.
###For this example we will look at CHELSA and Worldclim , Mean annual temperature (Bio1) and Annual Precipitation (Bio 12)
      ###First load a shapefile of the area (can be country, continent etc. or simple rectangle)
          ###I provided a rectangle encompassing SouthAmerica but you can change this to your are of interest
          StudyArea<-readOGR("./SA_mask/","SA_mask")
      ###Load different climatic variables to explore
          Bio1Chelsa<-raster("EnvironmentalData/CHELSA_bio10_01.tif")
          Bio1WC<-raster("EnvironmentalData/wc2.0_bio_30s_01.tif")
          Bio12Chelsa<-raster("EnvironmentalData/CHELSA_bio10_12.tif")
          Bio12WC<-raster("EnvironmentalData/wc2.0_bio_30s_12.tif")
        ##Lets first crop to our area of interest
          Bio1Chelsa<-crop(Bio1Chelsa,StudyArea)
          Bio1WC<-crop(Bio1WC,StudyArea)
          Bio12Chelsa<-crop(Bio12Chelsa,StudyArea)
          Bio12WC<-crop(Bio12WC,StudyArea)
        ###Now lets plot those layers
          par(mfrow=c(2,2)) ##this means we will plot 4 different things in 2 rows and 2 columns
          plot(Bio1Chelsa,main="Bio1 Chelsa")
          plot( Bio1WC, main= "Bio 1 WorldClim2")
          plot(Bio12Chelsa, main= "Annual precipitation Chelsa")
          plot(Bio12WC, main= "Annual precipitation WorldClim2")
          ####Notice anything different? Units are different for Bio1 so must divide layer 1 by 10. Also maximum precipitation is different (10000 vs. 7000)
          ###In R you can easily make operations on raster data, in this case:
           Bio1Chelsa<-Bio1Chelsa/10
           
           ###Now lets plot those layers again now specifying units and specifying limits for plotting
           par(mfrow=c(2,2)) ##this means we will plot 4 different things in 2 rows and 2 columns
           plot(Bio1Chelsa,main="Bio1 Chelsa (ºC)")
           plot(Bio1WC, main= "Bio 1 WorldClim2 (ºC)")
           plot(Bio12Chelsa, main= "Annual precipitation Chelsa (mm)",zlim=c(0,10000))
           plot(Bio12WC, main= "Annual precipitation WorldClim2 (mm)",zlim=c(0,10000))
           
           ## So do you have a preference visually? How about some numeric comparisons?
            ##We will create two comparison layers, not dividing because we have values of 0 temperature so substracting
           CompBio1<-Bio1Chelsa-Bio1WC
           CompBio12<-Bio12Chelsa-Bio12WC
          ##plot this along with the originals. Replace the X values here to plot 3 columns and 2 rows (each row one variable, each column a source and the last one the comparison)
           par(mfrow=c(2,3)) ##this means we will plot 4 different things in 2 rows and 2 columns
           plot(Bio1Chelsa,main="Bio1 Chelsa (ºC)")
           plot(Bio1WC, main= "Bio 1 WorldClim2 (ºC)")
           plot(CompBio1,main="Chelsa-WorldClim")
           plot(Bio12Chelsa, main= "Annual precipitation Chelsa (mm)",zlim=c(0,10000))
           plot(Bio12WC, main= "Annual precipitation WorldClim2 (mm)",zlim=c(0,10000))
           plot(CompBio12,main="Chelsa-WorldClim")
           ##as you can see there are a lot of differences, the dataset selected matters so take some time to consider what is best for your case and compare more variables these were just some examples
                  
      ###now try setting one of these on your own, the EnvironmentalData folder contains Bio1 and Bio12 layers
        #generated from remote sensing data by Vincent Deblauwe and collaborators obtained from https://vdeblauwe.wordpress.com/download/
  
                  
  ############################################################################################################
  ############################Exploring the localities for a species and doing some auto cleanup##############
  ############################################################################################################                
                
  ###We are taking a look at the Hypsiboas crepitans points dowloaded from GBIF on Ocotber 25 2018
    ###Are they ready for modeling?
###Load points and make into shapefile
points_coord<-read.table("occurrence.txt",h=T)
points_export<-points_coord
##Need to explain to R which columns have the coordinates
coordinates(points_coord) <- ~decimalLongitude+decimalLatitude
points_coord@data$Country<-as.character(points_coord@data$Country)
##Correct countries that should have spaces in their names and were not allowed in the dataframe
points_coord@data$Country[points_coord@data$Country=="Trinidad_and_Tobago"]<-"Trinidad and Tobago"
points_coord@data$Country[points_coord@data$Country=="French_Guiana"]<-"French Guiana"
points_coord@data$Country[points_coord@data$Country=="United_States"]<-"United States"

###Load political boundaries world
world_adm<-readOGR(".","world_adm0")
crs(points_coord) <- crs(world_adm)
###Take a first look
plot(world_adm)
points(points_coord,col="red",pch=19,cex=0.5)

###Validate political boundaries
    ###This is overlaying your points with the political boundaries it will produce a new dataframe with the 
        ###information from the political boundaries for every point.
ovr <- over(points_coord, world_adm)
cntr <- ovr$NAME
cntr<-as.character(cntr)

### If no country intersects with the point then no country name will appear. So those oints will be outside countries, in the ocean
i <- which(is.na(cntr))
  #See how many points are in the ocean
  length(i)
####- Country level 0 division
j <- which(cntr != as.character(points_coord$Country))
  #See how many points are in the wrong country
  length(j)
  #Take a look at which countries are wrongly represented in coordinates
  country_pb<-cbind(cntr, as.character(points_coord$Country))[j,]
  colnames(country_pb)<-c("Coordinates_country","GBIF_country") 
  country_pb


###Visualize problems so far
plot(points_coord,pch=19,cex=0.5)
plot(world_adm, add=T, lwd=1)
points(points_coord[j, ], col='red', pch=19, cex=0.5) ##wrong country
points(points_coord[i,],col="blue",pch=19,cex=0.5) ##ocean points

### Validate species IUCN distribution (or any other available range map)

    ###Load IUCN shapefile from folder
    IUCN_poly<-readOGR(".","data_0")
    ##Intersect points with the IUCN distribution
    ovr <- over(points_coord,IUCN_poly)
    cntr <- ovr$BINOMIAL
    cntr<-as.character(cntr)
    k <- which(is.na(cntr)) ##which points are not intersecting with this species range map
    ##Intersect points with the buffered IUCN distribution
    ovr1<-over(points_coord,gBuffer(IUCN_poly,width=0.1,byid=T))
    cntr1<-ovr1$BINOMIAL
    cntr1<-as.character(cntr1)
    k1 <- which(is.na(cntr1))
    #See how many points are outside de IUCN distribution
    length(k)
    #See how many points are outside de buffered IUCN distribution
    length(k1)
    ###Plot the points that are outside the buffered range in blue
    plot(points_coord,pch=19,cex=0.5)
    plot(world_adm,add=T)
    plot(IUCN_poly,col="red",add=T)
    points(points_coord[k,],col="blue",pch=19,cex=0.5) ##Points outside of IUCN range
  
  ####Challenge! Can you add the points that are outside the buffered IUCN range in purple? 



###ADD Flags of problems!
points_export$Country_Flag<-rep(NA,length(points_export$Country))
points_export$Country_Flag[j]<-cntr[j]
points_export$Ocean_Flag<-rep(NA,length(points_export$Country))
points_export$Ocean_Flag[i]<-"Ocean"
points_export$Species_Flag<-rep(NA,length(points_export$Country))
points_export$Species_Flag[k]<-"Outside_of_range"
points_export$Species_GbufferFlag<-rep(NA,length(points_export$Country))
points_export$Species_GbufferFlag[k1]<-"Outside_of_buffered_range"

###Export flags of problems to Database
write.csv(points_export,"db_H_crepitans_flags.csv")

##Eliminate problems and plot again
points_corrected<-points_export[-c(i,j,k1),]
coordinates(points_corrected) <- ~decimalLongitude+decimalLatitude
plot(points_corrected,col="red",pch=19,cex=0.5)
plot(world_adm,add=T)
