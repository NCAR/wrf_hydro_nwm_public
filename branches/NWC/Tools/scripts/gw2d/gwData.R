require(gstat)
require(maptools)
require(mapdata)
require(raster)

require(sp)
require(rgdal)
require(ncdf)



#import combined HK200 Data
dataDir <- "~/docs/WRF-Hydro/AmmerData/hkBayern/merged"

geoEm   <- "/home/fersch-b/devel/ndhms_wrf_hydro/trunk/NDHMS/rRott/geo_em.d01.nc"
# hiRes   <- "/home/fersch-b/devel/ndhms_wrf_hydro/trunk/NDHMS/rRott/hires_100m.nc"
hiRes   <- "/home/fersch-b/devel/ndhms_wrf_hydro/trunk/NDHMS/rRott/hires_25m.nc"
outName <- "./RottData/kfRott.nc"
dem     <- readGDAL("./RottData/hiresRott.tif")      # open geoTIFF


# geoEm <- "/home/fersch-b/devel/ndhms_wrf_hydro/trunk/NDHMS/rAmmer/geo_em.d03.nc"
# hiRes <- "/home/fersch-b/devel/ndhms_wrf_hydro/trunk/NDHMS/rAmmer/hires_geo_full_1km.nc"


fList<-grep("shp",dir(dataDir, f=T), value=T)
layer<-unlist(strsplit(unlist(strsplit(fList, "/"))[length(unlist(strsplit(fList, "/")))], ".shp"))
poly<-readOGR(dsn=fList, layer=layer)


#get geo_em wrf grid
nc<-open.ncdf(hiRes)
wrfXlat<-get.var.ncdf(nc, "LATITUDE")
wrfXlong<-get.var.ncdf(nc, "LONGITUDE")
close.ncdf(nc)

hXdim <- dim(wrfXlat)[1]
hYdim <- dim(wrfXlat)[2]

nc<-open.ncdf(geoEm)
centerLat <- att.get.ncdf(nc, 0, "CEN_LAT")$value
centerLon <- att.get.ncdf(nc, 0, "CEN_LON")$value
trueLat1  <- att.get.ncdf(nc, 0, "TRUELAT1")$value
trueLat2  <- att.get.ncdf(nc, 0, "TRUELAT2")$value

cornerLats <- att.get.ncdf(nc, 0, "corner_lats")$value
cornerLons <- att.get.ncdf(nc, 0, "corner_lons")$value

xdim <-  att.get.ncdf(nc, 0, "WEST-EAST_GRID_DIMENSION")$value-1
ydim <-  att.get.ncdf(nc, 0, "SOUTH-NORTH_GRID_DIMENSION")$value-1

projType <-  att.get.ncdf(nc, 0, "MAP_PROJ")$value

dx <- att.get.ncdf(nc, 0, "DX")$value
dy <- att.get.ncdf(nc, 0, "DY")$value

close.ncdf(nc)


if(projType == 1) projCode="lcc"
if(projType != 1) stop("Projection not implemented")

llCRS <- CRS("+proj=longlat +datum=WGS84")
wrfCRS <- CRS(sprintf("+proj=%s +a=6370000.0 +b=6370000.0 +lat_0=%f +lat_1=%f +lat_2=%f +lon_0=%f +datum=WGS84 +ellps=MERIT", projCode, centerLat, trueLat1, trueLat2, centerLon))
wrfCoords<-SpatialPoints(data.frame(as.vector(wrfXlong), as.vector(wrfXlat)), proj4string=llCRS)
wrfCoords <- spTransform(wrfCoords, poly@proj4string)

xx<-over(wrfCoords, poly)
xy<-xx$KF_L

hycond <- SpatialPointsDataFrame(wrfCoords, data.frame(factor(xy)))

mat<-as.matrix(flip(raster(matrix(xy,nrow=hXdim, ncol=hYdim)),"x"))

mat[which(is.na(mat))]<-(100)



nkf <- mat
nkf[which(nkf==1)] <- 10**-1.5       # sehr hoch
nkf[which(nkf==2)] <- 10**-2.5       # hoch
nkf[which(nkf==3)] <- 10**-3.5       # mittel
nkf[which(nkf==4)] <- 10**-4.0       # mittel bis mäßig
nkf[which(nkf==5)] <- 10**-4.5       # mäßig
nkf[which(nkf==6)] <- 10**-5.0       # mäßig bis gering
nkf[which(nkf==7)] <- 10**-6.0       # gering
nkf[which(nkf==8)] <- 10**-7.0       # gering bis äußerst gering
nkf[which(nkf==9)] <- 10**-8.0       # sehr gering
nkf[which(nkf==10)]<- 10**-10.0      # äußerst gering
nkf[which(nkf==11)] <- 10**-5        # stark variabel
nkf[which(nkf==12)] <- 10**-2.4      # unbekannt
nkf[which(nkf==99)] <- 10**-5        # Gewässer
nkf[which(is.na(nkf))] <- 10**-5     # NoData

rdem<-as.matrix(t(raster(dem)))
ndem<-as.matrix(flip(t(rdem),"x"))
# ndem<-as.matrix(t(rdem))
rdem<-as.matrix(flip(t(raster(dem)),"x"))

ncdfout<- function(spath, cm, long, lat, longname="NA", fact=1, varname="X", dim1="x", dim2="y", u1=" ", u2=" ", uvar=" ", datfm=NA, 
                    xdim=as.double(0:(dim(cm)[1]-1)), ydim=as.double(0:(dim(cm)[2]-1)), time="", add=FALSE, flipY=FALSE, dtype="double"){

    require(ncdf)

    if(flipY) ydim <- rev(ydim)
    
    if(add){
        nc <- open.ncdf(spath, write=TRUE)
        
        dimx <- nc$dim[[1]]
        dimy <- nc$dim[[2]]
        dimt <- nc$dim[[3]]
        
        varz  <- var.def.ncdf(varname, uvar, list(dimx,dimy, dimt), -1, longname=longname,    prec=dtype)

        nc <- var.add.ncdf(nc, varz)
        if(dtype=="double")  put.var.ncdf(nc, varz$name, as.double(cm*fact))
        if(dtype=="integer") put.var.ncdf(nc, varz$name, as.integer(cm*fact))
        att.put.ncdf(nc, varz$name,   "coordinates",   "XLONG XLAT")
        close.ncdf(nc)
        }

    else{

     if(is.na(datfm)) dimx<-dim.def.ncdf(dim1, u1, xdim)
     else             dimx<-dim.def.ncdf(dim1, datfm, xdim)

     dimy<-dim.def.ncdf(dim2, u2, ydim)
     dimt<-dim.def.ncdf("time", time, 1)
     varz  <-var.def.ncdf(varname, uvar, list(dimx,dimy, dimt), -1, longname=longname,    prec="double")

     xxlat <-var.def.ncdf("XLAT",  "degree_north", list(dimx,dimy, dimt), -1, longname="latitude" , prec="double")
     xxlong<-var.def.ncdf("XLONG", "degree_east", list(dimx,dimy, dimt), -1, longname="longitude", prec="double")
     nc <- create.ncdf(spath, list(varz, xxlat, xxlong))
     put.var.ncdf(nc, varz,   as.double(cm*fact))
     put.var.ncdf(nc, xxlat,  as.double(lat))
     put.var.ncdf(nc, xxlong, as.double(long))
     put.var.ncdf(nc, "time", 0)

     att.put.ncdf(nc, varz,   "coordinates",   "XLONG XLAT")
     att.put.ncdf(nc, xxlat,  "standard_name", "latitude")
     att.put.ncdf(nc, xxlong, "standard_name", "longitude")

     close.ncdf(nc)
    }
    }


# ncdfout("AmmerData/gwhires.nc", mat, lat=wrfXlat, long=wrfXlong, varname="KF_L")
ncdfout(outName, mat, lat=rev(wrfXlat), long=wrfXlong, varname="KF_L")
ncdfout(outName, nkf, varname="HC", add=T)

ncdfout(outName, rdem*0+0.4,   varname="POR", add=T)
ncdfout(outName, rdem-0.1, varname="IHEAD", add=T)
ncdfout(outName, rdem-25, varname="BOTELV", add=T)


# gridObject for geo_em 

cornerCoords <- SpatialPoints(data.frame(cornerLats[13:16], cornerLons[13:16]), proj4string=llCRS)
cornerCoords <- spTransform(cornerCoords, wrfCRS)

llc<-as.numeric(as.data.frame(cornerCoords)[1,])

gridTopoWRF<-GridTopology(llc, cellsize=c(dx,dy), c(xdim,ydim))

gridWRF<-SpatialGrid(gridToptWRF, proj4string=wrfCRS)

cs <- c(dx,dy)


 spplot(map, col.regions=bpy.colors(10), key.space="right", fill=TRUE)



