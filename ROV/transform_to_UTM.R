#CYCLONES PROJECT 2015, SCRIPT# 


#4th PART TRANSFORM DATASET AND STUDY SITE IN TO UTM 

#Projection WGS 1984
wgs1984.proj <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
#Projection i=to Geocscience Australia conform conic projection
gda94.proj<-CRS("+proj=lcc +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

#convert data in data.frame
coordinates(fgr)<-~long+lat
coordinates(fgr)<-~long+lat

#set study area
par(mfrow=c(2,3))#matrix plot representation
sa<-readShapePoly("study_area.shp")

#projecting study area in UTM and plotting
proj4string(sa)<-wgs1984.proj
sa.geos <- spTransform(sa, gda94.proj)
plot(sa.geos)

#projecting dataset of functional groups data.frame in UTM and plotting
proj4string(fgr)<-wgs1984.proj
gdata.geos <- spTransform(fgr, gda94.proj)
plot(gdata.geos)

#projecting dataset of species data.frame in UTM (same spatial data thatn funct groups so no need to be plotted)
proj4string(fgr)<-wgs1984.proj
gdata.geos <- spTransform(fgr, gda94.proj)
plot(gdata.geos)
-------------------------------------------------------------------------------------------------
  

  
  
  
  
  
  
  
  
  
















