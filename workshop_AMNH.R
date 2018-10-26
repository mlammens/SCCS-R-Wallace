####Validate points
library(dismo)
library(sp)
library(rgdal)
library(raster)
library(rgeos)
setwd("~/Downloads/Workshop/")
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
