# Accesssing EMODnet Physics data (ERDDAP)
# Written by Pascal Derycke (Marine Analyst)


install.packages('ggplot2')
install.packages('ggmap')
install.packages('ggrepel')
install.packages('raster')
install.packages('rasterVis')
install.packages('mapdata')
install.packages('sp')
install.packages('downloader')
install.packages('rgdal')
install.packages('XML')
install.packages("rgeos")

# Load R dependencies
library(rgdal)
library(downloader)
library(ggplot2)
library(mapdata)
library(ggmap)
library(ggrepel)
library(rasterVis)
library(rgeos)
library(sp)
library(raster)
library(XML)

# TITLE
# Setting the session parameters

Sessionid <- 'mySession'
source_provider <- "EMODnet Physics"
source_provider_url <- "https://www.emodnet.eu"
layer_title<-"Platforms collecting SLEV"
layer <- "EP_PLATFORMS_SLEV"
wfs_url <- "http://geoserver.emodnet-physics.eu/geoserver/emodnet/wfs?"
wms_url <- "http://geoserver.emodnet-physics.eu/geoserver/emodnet/wms?"
wms_layer <- "EP_PLATFORMS_SLEV"
geometry_name<-"position"
map_label<-"PlatformID"
epsg_code<-"EPSG:4326"


minlon <- -2.02
minlat <- 43.18
maxlon <- -1.17
maxlat <- 44.09

xmin <- as.numeric(minlon)
ymin <- as.numeric(minlat)
xmax <- as.numeric(maxlon)
ymax <- as.numeric(maxlat)

print (paste("West-Longitude:",round(xmin,2)))
print (paste("South-Latitude:",round(ymin,2)))
print (paste("East-Longitude:",round(xmax,2)))
print (paste("North-Latitude:",round(ymax,2)))

# TITLE
# Setting the session parameters

# Localisation at global scale #map displaying the defined area
sr=sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(cbind(c(xmin, xmin, xmax, xmax),c(ymax, ymin, ymin, ymax)))),"1")))
mpa=sp::SpatialPolygonsDataFrame(sr, data.frame(cbind(1:1), row.names=c("1")))
sp::proj4string(mpa)<-CRS("+proj=longlat +datum=WGS84")


zoom_value<-6 #zoom definition
base<-get_map(location=c(xmin-1,ymin-1,xmax+1,ymax+1), zoom=zoom_value, maptype="terrain-background", source = "stamen")
terrain <- ggmap(base)

map <- terrain + geom_polygon(data=mpa,aes(x=long,y=lat,group=group,fill="mpa"),colour="green",fill="blue",alpha=.1) +
ggtitle("")+xlab("Longitude")+ylab("Latitude")
plot(map)



# EMODnet physics WFS
# The WFS of the EMODnet Physics portal offers the possibility to identify the existing in-situ measurements for the region of interest (bbox).


# function to download point data as csv from the EMODnet physics WFS
getEMODnetPhysicsplatform<-function(name="EP_PLATFORMS_SLEV",xmin,xmax,ymin,ymax){
  name<-as.character(name)
  bbox<-paste(xmin,xmax,ymin,ymax,sep=",")
  con<-paste0(wfs_url,"service=WFS&VERSION=1.1.0&request=GetFeature&typeName=",name,"&OUTPUTFORMAT=csv&bbox=",bbox)
  utils::download.file(con,"dat.csv", quiet = TRUE, mode = "wb")
  pipo<-utils::read.csv("dat.csv")
  if(ncol(pipo)<=1){
    return(data.frame())
  }
  else{
    xy<-strsplit(gsub("\\)","",gsub("POINT \\(","",pipo$position)),split=" ")
    fun1<-function(a){as.numeric(a[1])}
    fun2<-function(a){as.numeric(a[2])}
    pipo$y<-sapply(xy,fun1)
    pipo$x<-sapply(xy,fun2)
    print(con)
    return(pipo)
  }
  
}

# define a function to combine different data layers in a dataframe
getEMODnetPhysicsplatformall<-function (xmin, xmax, ymin, ymax){
  physics <- data.frame()
  
  rez0 <- getEMODnetPhysicsplatform("EP_PLATFORMS_SLEV", ymin, xmin, ymax, xmax)
  if (nrow(rez0) > 0){
    physics1 <- data.frame(type = "EP_PLATFORMS_SLEV", status = rez0$ProjectsDescr,
                           purpose = rez0$ParametersGroupDescr, info = rez0$DataOwner, name = rez0$PlatformID, PltLink = rez0$PlatformInfoLink,
                           country = rez0$Country, x = rez0$x, y = rez0$y)
    physics <- rbind(physics, physics1)
    rm(rez0)
  }
  
  rez0 <- getEMODnetPhysicsplatform("EP_PLATFORMS_WIND", ymin, xmin, ymax, xmax)
  if (nrow(rez0) > 0){
    physics1 <- data.frame(type = "EP_PLATFORMS_WIND", status = rez0$ProjectsDescr,
                           purpose = rez0$ParametersGroupDescr, info = rez0$DataOwner, name = rez0$PlatformID, PltLink = rez0$PlatformInfoLink,
                           country = rez0$Country, x = rez0$x, y = rez0$y)
    physics <- rbind(physics, physics1)
    rm(rez0)
  }
  
  rez0 <- getEMODnetPhysicsplatform("EP_PLATFORMS_WAVE", ymin, xmin, ymax, xmax)
  if (nrow(rez0) > 0){
    physics1 <- data.frame(type = "EP_PLATFORMS_WAVE", status = rez0$ProjectsDescr,
                           purpose = rez0$ParametersGroupDescr, info = rez0$DataOwner, name = rez0$PlatformID, PltLink = rez0$PlatformInfoLink,
                           country = rez0$Country, x = rez0$x, y = rez0$y)
    physics <- rbind(physics, physics1)
    rm(rez0)
  }
  
  rez0 <- getEMODnetPhysicsplatform("EP_PLATFORMS_TEMP", ymin, xmin, ymax, xmax)
  if (nrow(rez0) > 0){
    physics1 <- data.frame(type = "EP_PLATFORMS_TEMP", status = rez0$ProjectsDescr,
                           purpose = rez0$ParametersGroupDescr, info = rez0$DataOwner, name = rez0$PlatformID, PltLink = rez0$PlatformInfoLink,
                           country = rez0$Country, x = rez0$x, y = rez0$y)
    physics <- rbind(physics, physics1)
    rm(rez0)
  }
  
  rez0 <- getEMODnetPhysicsplatform("EP_PLATFORMS_HCXX", ymin, xmin, ymax, xmax)
  if (nrow(rez0) > 0){
    physics1 <- data.frame(type = "EP_PLATFORMS_HCXX", status = rez0$ProjectsDescr,
                           purpose = rez0$ParametersGroupDescr, info = rez0$DataOwner, name = rez0$PlatformID, PltLink = rez0$PlatformInfoLink,
                           country = rez0$Country, x = rez0$x, y = rez0$y)
    physics <- rbind(physics, physics1)
    rm(rez0)
  }
  
  rez0 <- getEMODnetPhysicsplatform("EP_PLATFORMS_MO", ymin, xmin, ymax, xmax)
  if (nrow(rez0) > 0){
    physics1 <- data.frame(type = "EP_PLATFORMS_MO", status = rez0$ProjectsDescr,
                           purpose = rez0$ParametersGroupDescr, info = rez0$DataOwner, name = rez0$PlatformID, PltLink = rez0$PlatformInfoLink,
                           country = rez0$Country, x = rez0$x, y = rez0$y)
    physics <- rbind(physics, physics1)
    rm(rez0)
  }
  
  
  rez0 <- getEMODnetPhysicsplatform("EP_PLATFORMS_FB", ymin, xmin, ymax, xmax)
  if (nrow(rez0) > 0){
    physics1 <- data.frame(type = "EP_PLATFORMS_FB", status = rez0$ProjectsDescr,
                           purpose = rez0$ParametersGroupDescr, info = rez0$DataOwner, name = rez0$PlatformID, PltLink = rez0$PlatformInfoLink,
                           country = rez0$Country, x = rez0$x, y = rez0$y)
    physics <- rbind(physics, physics1)
    rm(rez0)
  }
  
  rez0 <- getEMODnetPhysicsplatform("EP_PLATFORMS_GL", ymin, xmin, ymax, xmax)
  if (nrow(rez0) > 0){
    physics1 <- data.frame(type = "EP_PLATFORMS_GL", status = rez0$ProjectsDescr,
                           purpose = rez0$ParametersGroupDescr, info = rez0$DataOwner, name = rez0$PlatformID, PltLink = rez0$PlatformInfoLink,
                           country = rez0$Country, x = rez0$x, y = rez0$y)
    physics <- rbind(physics, physics1)
    rm(rez0)
  }
  
  return(physics)
}

# get the data for the defined area
physics<-getEMODnetPhysicsplatformall(xmin,xmax,ymin,ymax)


# PLOT MAP WITH Platforms
map<-ggplot() +
  theme_bw() +
  borders("worldHires",xlim=c(xmin,xmax),ylim=c(ymin,ymax),fill="light grey",colour="light grey")+ 
  geom_point(data= physics,aes(x=x,y=y,shape=type,group=type),colour="red", alpha=1, size=4) +
  geom_polygon(data=mpa,aes(x=long,y=lat,group=group,fill="mpa"),colour="green",fill="blue",alpha=.1) +
  coord_quickmap(xlim=range(xmin,xmax),ylim=range(ymin,ymax)) +
  ggtitle("EMODnet Physics Platforms")+xlab("Longitude")+ylab("Latitude")
plot(map)


#Get layer names from WFS getcapabilities

Getcapabilities<-function(){
  con<-paste0(wfs_url,"service=WFS&version=1.1.0&request=Getcapabilities")
  pipo <- xmlParse(file = con)
  return(pipo)
}
WFS_GetCapa <- Getcapabilities()

rootnode <- xmlRoot(WFS_GetCapa)

rootsize <- xmlSize(rootnode)
numberOfLayers <- xmlSize(rootnode[[4]])

EMODnetPhysics<-data.frame()

for (i in 1:xmlSize(rootnode[[4]]))
{
Layer_i <- c(rootnode[[4]][[i]][[1]],rootnode[[4]][[i]][[2]])
EMODnetPhysics <- append(EMODnetPhysics, Layer_i)
}

EMODnetPhysics

#Get colnames from WFS
# DescribeFeatureType request function

DescribeFeatureType<-function(layer){
layer<-as.character(layer)
con<-paste0(wfs_url,"service=WFS&version=1.1.0&request=GetFeature&featureID=",geometry_name,"&typeName=",layer,"&OUTPUTFORMAT=csv")
pipo<-utils::read.csv(url(con), header= TRUE, encoding = 'UTF-8')
return(pipo)
}
WFS_Colnames <- DescribeFeatureType(layer)
colnames( WFS_Colnames )

rez_nblist<- c(1:dim(WFS_Colnames)[2])



#GET DATA as CSV

getWFSpoly<-function(layer, xmin, xmax, ymin, ymax){
layer<-as.character(layer)
#!!!! there is an error with the bbox xmin, xmax ... don't appear in the right order ???
#bbox<-paste(xmin,ymin,xmax,ymax,sep=",")
bbox<-paste(xmin,xmax,ymin,ymax,sep=",")
con<-paste0(wfs_url,"service=WFS&version=1.1.0&request=GetFeature&typeName=",layer,"&bbox=",bbox,",EPSG:4326&outputFormat=csv")
pipo<-utils::read.csv(url(con), header= TRUE, encoding = 'UTF-8')
# if no data return empty dataframe
print(con)
if(ncol(pipo)<=1){
return(data.frame())
} else {
# convert coordinates to numeric type
xy<-strsplit(gsub("\\)","",gsub("POINT \\(","",pipo[[geometry_name]])),split=" ")
fun1<-function(a){as.numeric(a[1])}
fun2<-function(a){as.numeric(a[2])}
pipo$y<-sapply(xy,fun1)
pipo$x<-sapply(xy,fun2)
return(pipo)
}
}

getWFSpolyall<-function (layer, xmin, xmax, ymin, ymax, rez_nblist){
wfs_data <- data.frame()
layer<-as.character(layer)
rez0 <- getWFSpoly(layer, xmin, xmax, ymin, ymax)
if (nrow(rez0) > 0) {
col_rez0 = c()
for (i in 1:length(rez_nblist))
{col_rez0 <- append(col_rez0, rez0[rez_nblist[i]])}

wfs_data1 <- data.frame(col_rez0)
wfs_data <- rbind(wfs_data,wfs_data1)
rm(rez0)
}
return(wfs_data)
}


wfs_data<-getWFSpolyall(layer, xmin, xmax, ymin, ymax, rez_nblist)


#plot data

if(nrow(wfs_data) > 0) {

wfs_data

} else {

print("No data available for the defined geographical extent")

}


#get data as GEOJSON

getGeojson<-function(layer, xmin, xmax, ymin, ymax){
layer<-as.character(layer)
bbox<-paste(xmin,ymin,xmax,ymax,sep=",")
con<-paste0(wfs_url,"service=WFS&srsName=",epsg_code,"&version=1.1.0&request=GetFeature&typeName=",
layer,"&OUTPUTFORMAT=application/json&bbox=",bbox,",EPSG:4326")
print(con)
ogrInfo(dsn=con,layer = 'OGRGeoJSON')
layer<-readOGR(dsn=con,layer = 'OGRGeoJSON', encoding = "UTF-8", use_iconv = TRUE)
return(layer)
}

#get data
WFS_json<-getGeojson(layer, xmin, xmax, ymin, ymax)
WFS_json<-spTransform(WFS_json,CRS("+proj=longlat +datum=WGS84"))
WFS_json <- intersect(WFS_json, mpa)


#print map
if(nrow(wfs_data) > 0) {

WFS_json_df <- as.data.frame(WFS_json)
colnames(WFS_json_df)[colnames(WFS_json_df) %in% c("coords.x1", "coords.x2")] <- c("x_longitude", "y_latitude")

map <- ggplot() +
borders("worldHires", fill = "gray", colour = "black", xlim = range(xmin,xmax), ylim = range(ymin,ymax), size = .25) +
  theme_bw() +
  theme(panel.grid.minor.y= element_blank(), panel.grid.minor.x = element_blank()) +
  geom_point(data=WFS_json_df,aes(x = x_longitude, y = y_latitude), color="red" ,alpha=1,size=2) +
  geom_polygon(data=mpa,aes(x=long,y=lat,group=group,fill="mpa"),colour="green",fill="blue",alpha=.1) +
  coord_quickmap(xlim=range(xmin,xmax),ylim=range(ymin,ymax)) +
  ggtitle(layer_title)+xlab("Longitude (x)")+ylab("Latitude (y)")

map

} else {

print("No data available for the defined geographical extent")
  
}

#print map with ids

if(nrow(wfs_data) > 0) {

map <- ggplot() +
borders("worldHires", fill = "gray", colour = "black", xlim = range(xmin,xmax), ylim = range(ymin,ymax), size = .25) +
  theme_bw() +
  theme(panel.grid.minor.y= element_blank(), panel.grid.minor.x = element_blank()) +
  geom_point(data=WFS_json_df,aes(x = x_longitude, y = y_latitude),colour="red",fill="blue",alpha=.9) +
  geom_polygon(data=mpa,aes(x=long,y=lat,group=group,fill="mpa"),colour="green",fill="blue",alpha=.1) +
  geom_text_repel(data=WFS_json_df, aes(label = WFS_json_df[[map_label]], x = x_longitude, y = y_latitude), size=2, hjust= 0, vjust=2, check_overlap = TRUE) +
  coord_quickmap(xlim=range(xmin,xmax),ylim=range(ymin,ymax)) +
  ggtitle(layer_title)+xlab("Longitude (x)")+ylab("Latitude (y)")

map


} else {

print("No data available for the defined geographical extent")

}

# Access measurements (7days) from ERDDAP
if(nrow(wfs_data) > 0) {

XSLEVLIST <- list() 
PlatformIDLIST <- c()
map_PlatformID <- 'PlatformID'
Parameter <- 'SLEV'
Units <- 'meter'


for (data in 1:nrow(wfs_data)){
PlatformID <- as.integer(wfs_data[[map_label]][data])
EndDate <- format(Sys.time(), '%d/%m/%Y')
StartDate <- format(as.Date(Sys.time())-7, '%d/%m/%Y')
erddap_url <- paste0("https://erddap.emodnet-physics.eu/erddap/tabledap/EP_ERD_INT_SLEV_AL_TS_NRT.csvp?EP_PLATFORM_ID,time,depth,",Parameter,"&EP_PLATFORM_ID=%22",PlatformID,"%22&time%3E=now-7days")

print(PlatformID)

tryCatch({CSLEV<- read.csv(erddap_url, header=T)


if(length(CSLEV) > 0) {

colnames(CSLEV) <- c('ID','DATE','DEPTH','SLEV')
CSLEV$DATE <- as.POSIXct(CSLEV$DATE, "UTC", "%Y-%m-%dT%H:%M:%S")
XSLEV<- subset(CSLEV, select=c("DATE","SLEV"))


#Select the first depth in the list
#XSLEV<-XSLEV[XSLEV$DEPTH == min(XSLEV$DEPTH, na.rm=T), ]

XSLEVLIST[[data]] <- subset(XSLEV, select=c("DATE","SLEV"))
PlatformIDLIST <- append(PlatformIDLIST, PlatformID)

map <- ggplot(XSLEV, aes(x = DATE, y = SLEV))+ geom_point()+geom_smooth(aes(color=DATE,fill=DATE))+
ggtitle(paste("PlatformID: ",PlatformID," - ",min(XSLEV$DEPTH, na.rm=T)))+xlab("7 days")+ylab(paste0(Parameter," (",Units,")"))
plot(map)


} else {
print(paste("PlatformID:",as.integer(PlatformID),"- No accessible data"))
}
}, error=function(e){cat("ERROR: No accessible data\n",conditionMessage(e), "\n")})
}
}


